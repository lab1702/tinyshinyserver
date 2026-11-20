# Connection Manager Module
# Manages WebSocket connections and session handling

# A WebSocket message as DEBUG logs show it. Text that cannot be cut as
# characters is described by its size, so that logging never fails a session.
describe_ws_message <- function(data) {
  if (is.raw(data)) {
    return(paste(length(data), "binary bytes"))
  }
  tryCatch(substring(data, 1, 100), error = function(e) paste(nchar(data, type = "bytes"), "bytes of text"))
}

# Connection Manager Class
ConnectionManager <- setRefClass("ConnectionManager",
  fields = list(
    config = "ANY",
    process_manager = "ANY" # Reference to process manager for immediate stops
  ),
  methods = list(
    initialize = function(server_config, proc_manager = NULL) {
      config <<- server_config
      process_manager <<- proc_manager
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
        logger::log_warn("Not connecting to app {app_name}: its process does not own port {port}",
          app_name = app_name, port = app_config$port
        )
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
      # Apps may send large outputs (e.g. htmlwidget data) as one message;
      # don't impose the websocket package's 32 MiB default on them.
      backend_ws <- websocket::WebSocket$new(backend_url,
        headers = headers,
        maxMessageSize = .Machine$integer.max
      )

      # Store connection info with ready state and timestamp
      config$add_backend_connection(session_id, list(
        app_name = app_name,
        ws = backend_ws,
        client_ws = client_ws,
        ready = FALSE,
        pending_messages = list(),
        last_activity = Sys.time(),
        created_at = Sys.time()
      ))

      # Handle messages from backend to client
      backend_ws$onMessage(function(event) {
        logger::log_debug("Backend->Client message for {app_name}: {data}",
          app_name = app_name, data = describe_ws_message(event$data)
        )
        conn <- config$get_backend_connection(session_id)
        if (is.null(conn) || !identical(conn$ws, backend_ws)) return()
        if (is.null(config$get_ws_connection(session_id))) return()
        # Traffic from the app alone keeps a session from counting as idle
        config$touch_session(session_id)
        client_ws$send(event$data)
      })

      # Handle backend connection events
      backend_ws$onOpen(function(event) {
        logger::log_info("Backend connection opened for {app_name}", app_name = app_name)

        # Mark connection as ready
        conn_info <- config$get_backend_connection(session_id)
        if (!is.null(conn_info)) {
          conn_info$ready <- TRUE
          config$add_backend_connection(session_id, conn_info)

          # Send any pending messages
          pending <- conn_info$pending_messages
          if (length(pending) > 0) {
            logger::log_info("Sending {count} pending messages for app {app_name}",
              count = length(pending), app_name = app_name
            )
            for (msg in pending) {
              backend_ws$send(msg)
            }
            conn_info$pending_messages <- list()
            config$add_backend_connection(session_id, conn_info)
          }
        }
      })

      backend_ws$onClose(function(event) {
        logger::log_info("Backend connection closed for app {app_name}", app_name = app_name)
        conn <- config$get_backend_connection(session_id)
        if (is.null(conn) || !identical(conn$ws, backend_ws)) return()
        config$remove_backend_connection(session_id)
        close_client_connection(session_id)
      })

      backend_ws$onError(function(event) {
        logger::log_error("Backend connection error for app {app_name}: {error}",
          app_name = app_name, error = event$message
        )
        conn <- config$get_backend_connection(session_id)
        if (is.null(conn) || !identical(conn$ws, backend_ws)) return()
        close_client_connection(session_id)
      })

      return(backend_ws)
    },
    handle_client_message = function(session_id, message, app_name) {
      "Handle a message from a WebSocket client"

      # Validate message
      message_validation <- validate_ws_message(message, config$MAX_MESSAGE_SIZE)
      if (!message_validation$valid) {
        logger::log_error("Invalid WebSocket message: {error}", error = message_validation$error)
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

      tryCatch(
        {
          if (backend_conn$ready) {
            backend_conn$ws$send(validated_message)
          } else {
            # Queue message for when connection is ready (with size limit)
            current_pending <- backend_conn$pending_messages
            if (length(current_pending) >= config$MAX_PENDING_MESSAGES) {
              # Drop oldest messages to make room
              current_pending <- tail(current_pending, config$MAX_PENDING_MESSAGES - 1)
              logger::log_warn("Pending queue full, dropped oldest messages for app {app_name}", app_name = app_name)
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
          logger::log_error("WebSocket message error: {error}", error = e$message)
          return(FALSE)
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

      logger::log_info("WebSocket connection closed for session {session_id}", session_id = session_id)

      # Get the app name before removing the connection
      conn_info <- config$get_ws_connection(session_id)
      app_name <- if (!is.null(conn_info)) conn_info$app_name else NULL

      # Clean up client connection
      config$remove_ws_connection(session_id)

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
    end_http_request = function(app_name) {
      count <- max(0L, (config$active_http_requests[[app_name]] %||% 0L) - 1L)
      assign(app_name, count, envir = config$active_http_requests)
      # The response may be a new page whose WebSocket has not opened yet, so
      # the app is only checked for idleness after the usual grace period.
      if (count == 0L) {
        schedule_session_check(app_name)
      }
    },
    schedule_session_check = function(app_name, grace_seconds = config$HTTP_SESSION_GRACE_SECONDS) {
      "Reclaim an HTTP-only launch after allowing time for a browser session"
      app_config <- config$get_app_config(app_name)
      process <- config$get_app_process(app_name)
      if (is.null(process_manager) || is.null(app_config) || app_config$resident ||
          is.null(process) || config$get_app_connection_count(app_name) > 0L) return(FALSE)
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
          process_manager$stop_app_immediately(app_name)
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
