# Connection Manager Module
# Manages WebSocket connections and session handling

# A WebSocket message as DEBUG logs show it. Its text comes from a visitor or
# an app, so it is cut short and cleaned of control characters as other
# logged request values are; text that cannot be read is described by its
# size, so that logging never fails a session.
describe_ws_message <- function(data) {
  if (is.raw(data)) {
    return(paste(length(data), "binary bytes"))
  }
  tryCatch(log_value(data, 100), error = function(e) paste(nchar(data, type = "bytes"), "bytes of text"))
}

# The size of a WebSocket message as it is sent
ws_message_bytes <- function(data) {
  if (is.raw(data)) length(data) else sum(nchar(data, type = "bytes"))
}

# The size of an HTTP response body in any form that httpuv sends: a raw
# vector, text, or the file that a large body was written to
response_body_bytes <- function(body) {
  if (is.null(body)) {
    return(0)
  }
  if (is.list(body)) {
    size <- if (is.character(body$file)) file.size(body$file) else NA
    return(if (length(size) == 1 && !is.na(size)) size else 0)
  }
  if (is.raw(body) || is.character(body)) ws_message_bytes(body) else 0
}

# A refused WebSocket handshake is logged at WARN at most once in this many
# seconds for each reason. Handshakes cost a client nothing, and a page in any
# visitor's browser can open them in a loop, which would otherwise fill the
# server log with warnings.
REJECTED_HANDSHAKE_LOG_SECONDS <- 10

# A callback for a socket of the websocket package, or a handler of httpuv's
# servers and sockets. Each package runs these inside a tryCatch() of its own
# that catches an interrupt: the websocket package only prints a message, and
# httpuv answers a request with HTTP 500. A Ctrl-C that R notices while the
# callback runs, as it may while an app streams messages or a request starts
# an app, would then leave the server running. The interrupt is noted, and the
# event loop shuts the server down as for any other Ctrl-C.
note_interrupts <- function(config, callback) {
  force(callback)
  function(...) {
    withCallingHandlers(callback(...), interrupt = function(e) {
      config$interrupt_requested <- TRUE
    })
  }
}

# Why a session for an app is refused when its process is not the verified
# listener on its port. A process that is not listening yet, or no longer, is
# the common cause; another program that holds the port is reported once a
# start, when the app is checked for readiness.
unverified_backend_text <- function(app_name, port) {
  sprintf("Rejecting WebSocket for app %s: its process is not running, or is not what listens on port %s", app_name, port)
}

# Connection Manager Class
ConnectionManager <- setRefClass("ConnectionManager",
  fields = list(
    config = "ANY",
    process_manager = "ANY", # Reference to process manager for immediate stops
    # For each reason a handshake was refused: when it was last logged at
    # WARN, and how many refusals have been logged only at DEBUG since
    rejected_handshakes = "environment"
  ),
  methods = list(
    initialize = function(server_config, proc_manager = NULL) {
      config <<- server_config
      process_manager <<- proc_manager
      rejected_handshakes <<- new.env(hash = TRUE, parent = emptyenv())
    },
    note_rejected_handshake = function(reason, text) {
      "Log a refused WebSocket handshake: at WARN once per reason every REJECTED_HANDSHAKE_LOG_SECONDS, otherwise at DEBUG"

      now <- proc.time()[["elapsed"]]
      previous <- rejected_handshakes[[reason]]
      if (!is.null(previous) && now - previous$logged_at < REJECTED_HANDSHAKE_LOG_SECONDS) {
        previous$suppressed <- previous$suppressed + 1
        assign(reason, previous, envir = rejected_handshakes)
        logger::log_debug("{text}", text = text)
        return(invisible(FALSE))
      }
      suppressed <- previous$suppressed %||% 0
      if (suppressed > 0) {
        text <- sprintf("%s (and %d more for the same reason since the last such warning)", text, suppressed)
      }
      assign(reason, list(logged_at = now, suppressed = 0), envir = rejected_handshakes)
      # The text holds values from the request, so it is the format's only
      # argument and is never itself read as a format
      logger::log_warn("{text}", text = text)
      invisible(TRUE)
    },
    create_backend_connection = function(app_name, session_id, client_ws) {
      "Create WebSocket connection to backend Shiny app"

      app_config <- config$get_app_config(app_name)
      if (is.null(app_config)) {
        logger::log_error("App not found: {app_name}", app_name = app_name)
        return(NULL)
      }
      # Browser cookies and credentials go only to the app's own process.
      if (!config$app_backend_verified(app_name)) {
        note_rejected_handshake(paste("listener", app_name), unverified_backend_text(app_name, app_config$port))
        close_client_connection(session_id)
        return(NULL)
      }

      # A handshake that names only the app gets Shiny's own socket path
      backend_path <- "/websocket/"
      route <- parse_proxy_path(strip_base_path(client_ws$request$PATH_INFO, config$config$base_path))
      if (route$valid && identical(route$app_name, app_name) && route$backend_path != "") {
        backend_path <- route$backend_path
      }
      backend_query <- client_ws$request$QUERY_STRING %||% ""
      if (backend_query != "" && !startsWith(backend_query, "?")) {
        backend_query <- paste0("?", backend_query)
      }
      backend_url <- paste0("ws://127.0.0.1:", app_config$port, backend_path, backend_query)
      # The query string can carry a token, so only DEBUG records it
      logger::log_info("Connecting to backend: ws://127.0.0.1:{port}{path} for app {app_name}",
        port = app_config$port, path = backend_path, app_name = app_name
      )
      logger::log_debug("Backend URL for app {app_name}: {backend_url}",
        app_name = app_name, backend_url = backend_url
      )

      # Create WebSocket connection to backend. An app's session reads its
      # request headers from this handshake, so it carries the same headers as
      # a proxied HTTP request, such as the identity headers an authenticating
      # reverse proxy adds. The handshake's own Sec-WebSocket fields belong to
      # each connection, and the proxy has already checked Origin, which would
      # not match the app's internal address.
      headers <- forwarded_request_headers(client_ws$request, skip = "ORIGIN")
      headers <- headers[!grepl("^SEC-WEBSOCKET-", names(headers))]
      connect_backend(app_name, session_id, client_ws, backend_url, headers,
        process = config$get_app_process(app_name), first_attempt = Sys.time()
      )
    },
    connect_backend = function(app_name, session_id, client_ws, backend_url, headers, process, first_attempt) {
      "Open the app's side of a session, again after a handshake that the app did not complete"

      # Fixed now, not when a callback first reads them: the process and the
      # time of the session's first attempt
      force(process)
      force(first_attempt)

      # Apps may send large outputs (e.g. htmlwidget data) as one message;
      # don't impose the websocket package's 32 MiB default on them.
      backend_ws <- websocket::WebSocket$new(backend_url,
        headers = headers,
        maxMessageSize = .Machine$integer.max
      )

      # Store connection info with ready state and timestamp. A new attempt
      # keeps the messages that the browser sent while it waited.
      previous <- config$get_backend_connection(session_id)
      config$add_backend_connection(session_id, list(
        app_name = app_name,
        ws = backend_ws,
        client_ws = client_ws,
        ready = FALSE,
        pending_messages = previous$pending_messages %||% list(),
        last_activity = Sys.time(),
        opened_clock = proc.time()[["elapsed"]],
        created_at = previous$created_at %||% Sys.time()
      ))

      # Handle messages from backend to client
      backend_ws$onMessage(note_interrupts(config, function(event) {
        logger::log_debug("Backend->Client message for {app_name}: {data}",
          app_name = app_name, data = describe_ws_message(event$data)
        )
        conn <- config$get_backend_connection(session_id)
        if (is.null(conn) || !identical(conn$ws, backend_ws)) return()
        if (is.null(config$get_ws_connection(session_id))) return()
        # httpuv queues what it sends a browser in this process and cannot
        # tell how much a browser has yet to read, so a client that stops
        # reading while the app keeps sending would hold it all here. The
        # app's traffic also keeps the session from counting as idle. So a
        # session on which the app has sent more than max_unanswered_mb since
        # the browser's last message is closed. A browser that reads, such as
        # a dashboard left open without interaction, cannot be told apart and
        # is disconnected too, and a reload of the page reconnects it.
        limit_mb <- config$config$max_unanswered_mb %||% DEFAULT_MAX_UNANSWERED_MB
        if (limit_mb > 0) {
          unanswered <- config$add_unanswered_bytes(session_id, ws_message_bytes(event$data))
          if (unanswered > limit_mb * 1024^2) {
            logger::log_warn(paste(
              "Closing session {session_id} of app {app_name}: the app sent more than",
              "max_unanswered_mb ({limit} MB) without a message from the browser"
            ), session_id = session_id, app_name = app_name, limit = format(limit_mb, scientific = FALSE))
            close_client_connection(session_id)
            return()
          }
        }
        # Traffic from the app alone keeps a session from counting as idle
        config$touch_session(session_id)
        client_ws$send(event$data)
      }))

      # Handle backend connection events
      backend_ws$onOpen(note_interrupts(config, function(event) {
        logger::log_info("Backend connection opened for {app_name}", app_name = app_name)

        # Mark connection as ready. A socket that the session no longer uses,
        # because it ended or a later attempt replaced this one, is closed.
        conn_info <- config$get_backend_connection(session_id)
        if (is.null(conn_info) || !identical(conn_info$ws, backend_ws)) {
          tryCatch(backend_ws$close(), error = function(e) {})
          return()
        }
        if (!is.null(conn_info)) {
          conn_info$ready <- TRUE
          config$add_backend_connection(session_id, conn_info)

          # Send any pending messages
          pending <- conn_info$pending_messages
          if (length(pending) > 0) {
            logger::log_info("Sending {count} pending messages for app {app_name}",
              count = length(pending), app_name = app_name
            )
            conn_info$pending_messages <- list()
            config$add_backend_connection(session_id, conn_info)
            # A message that cannot be sent ends the session, as it does once
            # the socket is open, rather than leave it without the rest
            for (msg in pending) {
              sent <- tryCatch({
                backend_ws$send(msg)
                TRUE
              }, error = function(e) {
                logger::log_error("WebSocket message error: {error}", error = conditionMessage(e))
                FALSE
              })
              if (!sent) {
                close_client_connection(session_id)
                return()
              }
            }
          }
        }
      }))

      backend_ws$onClose(note_interrupts(config, function(event) {
        logger::log_info("Backend connection closed for app {app_name}", app_name = app_name)
        conn <- config$get_backend_connection(session_id)
        if (is.null(conn) || !identical(conn$ws, backend_ws)) return()
        config$remove_backend_connection(session_id)
        close_client_connection(session_id)
      }))

      backend_ws$onError(note_interrupts(config, function(event) {
        logger::log_error("Backend connection error for app {app_name}: {error}",
          app_name = app_name, error = event$message
        )
        conn <- config$get_backend_connection(session_id)
        if (is.null(conn) || !identical(conn$ws, backend_ws)) return()
        # The websocket package gives up on a handshake after 5 seconds. An
        # app's R process completes handshakes only between its computations,
        # so an app that is busy for longer, such as with another visitor's
        # long calculation, misses that limit, while a browser connected to
        # the app directly would simply wait. The browser's socket is already
        # open, so it waits here instead, with its messages queued, as long
        # as the same app process runs.
        # Only a handshake that timed out is tried again: the app accepted the
        # connection. One that it refused or answered with an error fails.
        # A clock set back since the first attempt, which R cannot rule out,
        # ends the retries rather than prolonging them by as much
        elapsed <- as.numeric(difftime(Sys.time(), first_attempt, units = "secs"))
        timed_out <- grepl("handshake timed out", event$message %||% "", ignore.case = TRUE)
        if (timed_out && !isTRUE(conn$ready) && elapsed >= 0 && elapsed < config$WS_BACKEND_CONNECT_SECONDS &&
            !is.null(config$get_ws_connection(session_id)) &&
            identical(config$get_app_process(app_name), process) && is_process_alive(process) &&
            config$app_backend_verified(app_name)) {
          logger::log_info("Connecting to app {app_name} again for session {session_id}",
            app_name = app_name, session_id = session_id
          )
          later::later(function() {
            conn <- config$get_backend_connection(session_id)
            if (is.null(conn) || !identical(conn$ws, backend_ws) ||
                is.null(config$get_ws_connection(session_id))) return()
            tryCatch(
              connect_backend(app_name, session_id, client_ws, backend_url, headers, process, first_attempt),
              error = function(e) {
                logger::log_error("Could not connect to app {app_name}: {error}",
                  app_name = app_name, error = conditionMessage(e)
                )
                close_client_connection(session_id)
              }
            )
          }, 0.5)
          return()
        }
        close_client_connection(session_id)
      }))

      return(backend_ws)
    },
    handle_client_message = function(session_id, message, app_name) {
      "Handle a message from a WebSocket client. FALSE: the message was refused, and the browser is to be told and its socket closed."

      # Validate message
      message_validation <- validate_ws_message(message, config$MAX_MESSAGE_SIZE)
      if (!message_validation$valid) {
        logger::log_error("Invalid WebSocket message for app {app_name}, session {session_id}: {error}",
          app_name = app_name, session_id = session_id, error = message_validation$error
        )
        return(FALSE)
      }
      validated_message <- message_validation$sanitized

      logger::log_debug("Client message: {message}", message = describe_ws_message(validated_message))

      # The app's socket is opened when the browser connects, and the browser's
      # socket is closed along with it. A message with nowhere to go belongs
      # to a session that has already been removed, for example by a restart.
      backend_conn <- config$get_backend_connection(session_id)
      if (is.null(config$get_ws_connection(session_id)) || is.null(backend_conn) || is.null(backend_conn$ws)) {
        logger::log_debug("Ignoring message for untracked session {session_id}", session_id = session_id)
        return(FALSE)
      }
      config$touch_session(session_id)
      config$clear_unanswered_bytes(session_id)

      tryCatch(
        {
          if (backend_conn$ready) {
            backend_conn$ws$send(validated_message)
          } else {
            # Messages wait here until the app's socket opens. The first that
            # Shiny's browser code sends starts the session in the app, so a
            # queue that dropped any would leave the browser connected to a
            # session that never starts. A message that does not fit ends the
            # session instead, which the browser shows as disconnected.
            current_pending <- backend_conn$pending_messages
            pending_bytes <- sum(vapply(current_pending, ws_message_bytes, numeric(1))) +
              ws_message_bytes(validated_message)
            if (length(current_pending) >= config$MAX_PENDING_MESSAGES || pending_bytes > config$MAX_PENDING_BYTES) {
              logger::log_warn(paste(
                "Closing session {session_id} of app {app_name}: more messages arrived than are kept",
                "while the app's socket opens ({count} messages, {bytes} bytes)"
              ), session_id = session_id, app_name = app_name, count = length(current_pending) + 1,
                bytes = format(pending_bytes, scientific = FALSE))
              close_client_connection(session_id)
              return(TRUE)
            }
            backend_conn$pending_messages <- append(current_pending, list(validated_message))
            config$add_backend_connection(session_id, backend_conn)
            logger::log_debug("Queued message for pending connection ({count}/{max}) for app {app_name}",
              count = length(current_pending) + 1, max = config$MAX_PENDING_MESSAGES, app_name = app_name
            )
          }
          return(TRUE)
        },
        error = function(e) {
          # The message was valid, but the app's socket could not take it,
          # as when it has closed and its close has not been handled yet. The
          # session cannot go on, and the browser is not told the message
          # was invalid.
          logger::log_warn("Closing session {session_id} of app {app_name}: the message could not be passed to the app: {error}",
            session_id = session_id, app_name = app_name, error = conditionMessage(e)
          )
          close_client_connection(session_id)
          return(TRUE)
        }
      )
    },
    add_client_connection = function(session_id, ws, app_name, client_ip, user_agent) {
      "Add a new client WebSocket connection"
      cancel_session_check(app_name)

      config$add_ws_connection(session_id, list(
        ws = ws,
        app_name = app_name,
        client_ip = client_ip,
        user_agent = user_agent,
        last_activity = Sys.time(),
        opened_clock = proc.time()[["elapsed"]],
        created_at = Sys.time()
      ))

      logger::log_info("WebSocket connection added for app {app_name} from {client_ip}",
        app_name = app_name, client_ip = client_ip
      )
    },
    close_client_connection = function(session_id) {
      "Close a client socket and idempotently clean up its session"
      conn <- config$get_ws_connection(session_id)
      # Remove tracking before closing sockets: close callbacks may run inline.
      remove_client_connection(session_id)
      if (!is.null(conn$ws)) {
        tryCatch(conn$ws$close(), error = function(e) {})
      }
    },
    remove_client_connection = function(session_id) {
      "Remove a client WebSocket connection and cleanup"

      # Get the app name before removing the connection
      conn_info <- config$get_ws_connection(session_id)
      app_name <- if (!is.null(conn_info)) conn_info$app_name else NULL

      # Clean up client connection. A session that the proxy closes is removed
      # here first and again by its socket's close handler, so the close is
      # logged only when it ends a tracked session.
      if (isTRUE(config$remove_ws_connection(session_id))) {
        logger::log_info("WebSocket connection closed for session {session_id}", session_id = session_id)
      }

      # Stop an idle on-demand app only after a grace period: on a reload or
      # navigation the old page's socket closes after the new page's response
      # has completed, just before the new page reconnects
      if (!is.null(app_name)) {
        schedule_session_check(app_name)
      }

      # Clean up backend connection
      backend_conn <- config$get_backend_connection(session_id)
      if (!is.null(backend_conn)) {
        config$remove_backend_connection(session_id)
        if (!is.null(backend_conn$ws)) {
          tryCatch(backend_conn$ws$close(), error = function(e) {})
        }
      }
    },
    begin_http_request = function(app_name) {
      cancel_session_check(app_name)
      count <- config$active_http_requests[[app_name]] %||% 0L
      assign(app_name, count + 1L, envir = config$active_http_requests)
    },
    end_http_request = function(app_name, response_bytes = 0) {
      "Record that a request to an app has its response, of response_bytes bytes"
      count <- max(0L, (config$active_http_requests[[app_name]] %||% 0L) - 1L)
      assign(app_name, count, envir = config$active_http_requests)
      # The request ends when the app has answered, but httpuv only then
      # starts sending the response, and gives no sign when the browser has
      # it. A large page may take a slow browser longer than the grace period
      # to download, and its WebSocket opens only after that, so the time
      # such a download may take is added to the idle checks' grace.
      transfer_seconds <- min(floor(response_bytes / config$HTTP_TRANSFER_BYTES_PER_SECOND),
        config$HTTP_TRANSFER_MAX_SECONDS)
      if (transfer_seconds > 0) {
        ends <- proc.time()[["elapsed"]] + transfer_seconds
        assign(app_name, max(ends, config$http_transfer_ends[[app_name]] %||% 0), envir = config$http_transfer_ends)
      }
      # The response may be a new page whose WebSocket has not opened yet, so
      # the app is only checked for idleness after the usual grace period.
      if (count == 0L) {
        schedule_session_check(app_name)
      }
    },
    transfer_seconds_left = function(app_name) {
      "How long a slow browser may still be receiving an app's last large response"
      ends <- config$http_transfer_ends[[app_name]]
      if (is.null(ends)) {
        return(0)
      }
      max(0, ends - proc.time()[["elapsed"]])
    },
    schedule_session_check = function(app_name, grace_seconds = config$HTTP_SESSION_GRACE_SECONDS) {
      "Reclaim an HTTP-only launch after allowing time for a browser session"
      app_config <- config$get_app_config(app_name)
      process <- config$get_app_process(app_name)
      if (is.null(process_manager) || is.null(app_config) || app_config$resident ||
          is.null(process) || config$get_app_connection_count(app_name) > 0L) return(FALSE)
      grace_seconds <- grace_seconds + transfer_seconds_left(app_name)
      # Every finished HTTP request asks for a check, so drop the timer this
      # one replaces instead of leaving it queued for the whole grace period.
      cancel_session_check(app_name)
      token <- new.env(parent = emptyenv())
      assign(app_name, token, envir = config$pending_session_checks)
      token$cancel <- later::later(function() {
        if (!identical(config$pending_session_checks[[app_name]], token)) return(FALSE)
        assign(app_name, NULL, envir = config$pending_session_checks)
        if (!identical(config$get_app_process(app_name), process) ||
            (config$active_http_requests[[app_name]] %||% 0L) > 0L ||
            config$get_app_connection_count(app_name) > 0L) return(FALSE)
        # A slow start, such as a Quarto render, is not thrown away because
        # the visitor's page stopped waiting: look again after another grace
        # period, and stop the app then if it is ready and still unused.
        if (is_still_starting(app_name, process)) {
          schedule_session_check(app_name)
          return(FALSE)
        }
        maybe_stop_idle_app(app_name)
      }, grace_seconds)
      TRUE
    },
    is_still_starting = function(app_name, process) {
      "Whether an unused on-demand app has yet to listen on its port and may be given more time"

      # Where ps cannot list sockets, every live process counts as verified,
      # so an app that is still starting is stopped as before
      if (!is_process_alive(process) || config$app_backend_verified(app_name)) {
        return(FALSE)
      }
      # An app that never listens is not kept for ever
      uptime <- process_uptime_seconds(process)
      !is.null(uptime) && uptime < config$ON_DEMAND_START_LIMIT_SECONDS
    },
    cancel_session_check = function(app_name) {
      "Withdraw an app's pending idle check and its timer"
      pending <- config$pending_session_checks[[app_name]]
      # Forget the check before cancelling its timer: a timer that still fires
      # then finds that it is no longer the pending one and does nothing.
      assign(app_name, NULL, envir = config$pending_session_checks)
      if (is.function(pending$cancel)) pending$cancel()
    },
    maybe_stop_idle_app = function(app_name) {
      "Stop on-demand app if it has no active connections"

      # Validate input
      if (is.null(app_name) || app_name == "") {
        logger::log_warn("maybe_stop_idle_app called with invalid app_name")
        return(FALSE)
      }

      ws_count <- config$get_app_connection_count(app_name)

      logger::log_debug("WebSocket connections for {app_name}: {count}", app_name = app_name, count = ws_count)

      # If no WebSocket connections remain, immediately stop non-resident apps
      if (ws_count == 0 && !is.null(process_manager)) {
        app_config <- config$get_app_config(app_name)
        if (!is.null(app_config) && !app_config$resident) {
          # A request in flight keeps the app alive; end_http_request()
          # schedules the next idle check when the last one finishes.
          if ((config$active_http_requests[[app_name]] %||% 0L) > 0L) {
            return(TRUE)
          }
          logger::log_info("No WebSocket connections remain for non-resident app {app_name}, stopping immediately", app_name = app_name)
          # A process that outlives the attempt stays tracked, and nothing
          # else would look at the unused app again
          if (isFALSE(process_manager$stop_app_immediately(app_name)) &&
              isTRUE(schedule_session_check(app_name))) {
            logger::log_warn("Trying again to stop unused app {app_name} after {seconds} seconds",
              app_name = app_name, seconds = config$HTTP_SESSION_GRACE_SECONDS)
          }
        }
      }

      return(TRUE)
    }
  )
)

# Create connection manager factory function
create_connection_manager <- function(server_config, process_manager = NULL) {
  return(ConnectionManager$new(server_config, process_manager))
}
