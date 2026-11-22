# HTTP and WebSocket Handlers Module
# Handles all HTTP requests and WebSocket connections

# httpuv receives a request's whole body before calling the app, and the proxy
# then reads it into memory to forward it, so reject oversized bodies from the
# headers alone, as Shiny's own server does. Chunked bodies have no declared
# size and are rejected, whatever length the request also declares. The
# headers are read under the names the client sent: req$CONTENT_LENGTH would
# also answer for a "Content_Length" header that states a smaller size.
reject_large_request_body <- function(req, max_bytes) {
  size <- 0
  declared <- request_header(req, "content-length")
  if (!is.null(request_header(req, "transfer-encoding"))) {
    size <- Inf
  } else if (!is.null(declared)) {
    size <- suppressWarnings(as.numeric(declared))
  }
  if (is.na(size) || size > max_bytes) {
    return(create_error_response("Request body too large", 413))
  }
  NULL
}

# HTTP request handler
handle_http_request <- function(req, config, template_manager, connection_manager, process_manager = NULL) {
  "Main HTTP request handler with routing"

  response <- respond_safely(answer_http_request(req, config, template_manager, connection_manager, process_manager))
  without_head_body(req, response)
}

answer_http_request <- function(req, config, template_manager, connection_manager, process_manager = NULL) {
  "Validate and route a proxy request"

  if (!is_allowed_proxy_host(req, config)) {
    return(create_error_response("Invalid Host header", 403))
  }

  # The landing page's links are absolute, but send /base to /base/ anyway
  base_path <- config$config$base_path %||% ""
  if (nzchar(base_path) && identical(req$PATH_INFO, base_path)) {
    return(create_redirect_response(paste0(base_path, "/"), req$QUERY_STRING))
  }

  # Validate request inputs
  validation_result <- validate_request_inputs(
    strip_base_path(req$PATH_INFO, base_path),
    req$REQUEST_METHOD,
    req$QUERY_STRING
  )

  if (!validation_result$valid) {
    return(validation_result$response)
  }

  path <- validation_result$path
  method <- validation_result$method
  query_string <- validation_result$query_string

  logger::log_debug("HTTP request: {method} {path}", method = method, path = path)

  # Route handling
  return(route_http_request(
    path, method, query_string, req, config,
    template_manager, connection_manager, process_manager
  ))
}

route_http_request <- function(path, method, query_string, req, config, template_manager, connection_manager,
                               process_manager = NULL) {
  "Route HTTP requests to appropriate handlers"

  # Proxy requests to apps, in any method the proxy forwards
  if (startsWith(path, "/proxy/")) {
    return(handle_proxy_request(
      path, method, query_string, req, config,
      process_manager, connection_manager, template_manager
    ))
  }

  # The proxy's own pages and endpoints are read-only
  own_route <- path %in% c("/", "", "/health", "/api/apps") || startsWith(path, "/templates/")
  if (own_route && !method %in% c("GET", "HEAD")) {
    return(create_method_not_allowed_response())
  }

  # Landing page
  if (path == "/" || path == "") {
    return(handle_landing_page(config, template_manager))
  }

  # Health check endpoint
  if (path == "/health") {
    return(handle_health_check())
  }

  # API endpoint for app status
  if (path == "/api/apps") {
    return(handle_apps_api(config, process_manager))
  }

  # Static files (CSS, JS, images)
  if (startsWith(path, "/templates/")) {
    return(handle_static_file(path, template_manager, req$HTTP_IF_NONE_MATCH))
  }

  # 404 for unknown paths
  return(create_error_response("Not Found", 404))
}

handle_landing_page <- function(config, template_manager) {
  "Handle landing page requests"

  tryCatch(
    {
      html <- template_manager$generate_landing_page(config)
      return(create_html_response(html))
    },
    error = function(e) {
      logger::log_error("Error generating landing page: {error}", error = e$message)
      return(create_error_response("Internal Server Error", 500))
    }
  )
}

handle_health_check <- function() {
  "Handle health check requests"

  return(create_json_response(list(status = "healthy")))
}

handle_apps_api <- function(config, process_manager = NULL) {
  "Handle API requests for app status"

  # This would normally be in management_api.R but needed here for landing page
  tryCatch(
    {
      # App state lives in config, so a manager made here reports the same status
      process_manager <- process_manager %||% create_process_manager(config)
      apps_status <- process_manager$get_all_app_status()

      # The proxy port may be public, so expose only what the landing page
      # needs; paths, ports, and PIDs stay on the loopback management API
      apps_status <- lapply(apps_status, function(app) {
        app[intersect(c("name", "status", "resident", "connections"), names(app))]
      })

      return(create_json_response(apps_status))
    },
    error = function(e) {
      logger::log_error("Error getting app status: {error}", error = e$message)
      return(create_error_response("Internal Server Error", 500))
    }
  )
}

handle_static_file <- function(path, template_manager, if_none_match = NULL) {
  "Handle static file requests (CSS, JS, images)"

  # Remove /templates/ prefix
  file_path <- gsub("^/templates/", "", path)

  return(template_manager$serve_static_file(file_path, if_none_match))
}

# The route to an app, /proxy/<app name><path the app sees>, for HTTP requests
# and WebSocket handshakes alike. `backend_path` is "" when the app name ends
# the path.
parse_proxy_path <- function(path) {
  invalid <- function(error) list(valid = FALSE, error = error)

  if (!is.character(path) || length(path) != 1 || is.na(path) ||
    !startsWith(path, "/proxy/") || startsWith(path, "/proxy//")) {
    return(invalid("Invalid proxy path"))
  }

  rest <- substring(path, nchar("/proxy/") + 1L)
  app_name <- sub("/.*$", "", rest)
  if (app_name == "") {
    return(invalid("Invalid proxy path"))
  }

  app_name_validation <- validate_app_name(app_name)
  if (!app_name_validation$valid) {
    return(invalid(paste("Invalid app name:", app_name_validation$error)))
  }

  list(valid = TRUE, app_name = app_name, backend_path = substring(rest, nchar(app_name) + 1L))
}

# Start an on-demand app that a request or WebSocket has arrived for, unless it
# is already running. FALSE when the app could not be started.
ensure_on_demand_app_started <- function(app_config, config, process_manager, trigger) {
  app_name <- app_config$name
  if (app_config$resident || is.null(process_manager) || is_process_alive(config$get_app_process(app_name))) {
    return(TRUE)
  }
  logger::log_info("Starting non-resident app {app_name} on demand for {trigger}",
    app_name = app_name, trigger = trigger
  )
  isTRUE(process_manager$start_app_on_demand(app_name))
}

handle_proxy_request <- function(path, method, query_string, req, config, process_manager = NULL,
                                 connection_manager = NULL, template_manager = NULL) {
  "Handle proxy requests to Shiny apps"

  route <- parse_proxy_path(path)
  if (!route$valid) {
    return(create_error_response(route$error, 400))
  }

  app_name <- route$app_name
  app_config <- config$get_app_config(app_name)

  if (is.null(app_config)) {
    return(create_error_response("App not found", 404))
  }

  # Canonicalize the mount URL before starting an app or forwarding a body.
  if (route$backend_path == "") {
    return(create_redirect_response(paste0(config$config$base_path, path, "/"), query_string))
  }

  if (!ensure_on_demand_app_started(app_config, config, process_manager, "HTTP request")) {
    return(create_error_response("Failed to start app on demand", 502))
  }

  # Only the routing prefix is removed; the backend path is passed on verbatim.
  target_url <- paste0("http://127.0.0.1:", app_config$port, route$backend_path)

  # httpuv includes the leading question mark in QUERY_STRING.
  if (!is.null(query_string) && query_string != "") {
    target_url <- paste0(target_url, if (startsWith(query_string, "?")) "" else "?", query_string)
  }

  # Keep the backend alive until this request finishes, including failures.
  manager <- connection_manager %||% create_connection_manager(config, process_manager)
  manager$begin_http_request(app_name)
  released <- FALSE
  release <- function() {
    if (!released) {
      released <<- TRUE
      manager$end_http_request(app_name)
    }
  }
  tryCatch({
    response <- forward_request(method, target_url, req, app_name, config, template_manager)
    if (promises::is.promise(response)) return(promises::finally(response, release))
    release()
    response
  }, error = function(e) {
    release()
    stop(e)
  })
}

# Request bodies above this size are offered to the app with
# "Expect: 100-continue" before they are sent. libcurl writes a smaller body
# in one piece, right after the request headers.
EXPECT_CONTINUE_BYTES <- 65536

# Request headers that are not passed on to an app: they describe the client's
# own connection to the proxy, or httpuv has already acted on them.
UNFORWARDED_REQUEST_HEADERS <- c(
  "HOST", "CONNECTION", "KEEP_ALIVE", "TRANSFER_ENCODING",
  "TE", "TRAILER", "UPGRADE", "PROXY_AUTHORIZATION",
  "PROXY_AUTHENTICATE", "CONTENT_LENGTH", "CONTENT_TYPE",
  # httpuv has already answered the client's expectation and read the body
  "EXPECT"
)

forwarded_request_headers <- function(req, skip = character()) {
  "The request headers to send to an app, for HTTP requests and WebSocket handshakes alike"

  sent <- request_headers(req)
  headers <- list()
  for (name in names(sent)) {
    # An app's own server reads "X_Forwarded_User" as X-Forwarded-User, in
    # place of the value a reverse proxy set, so no header whose name has an
    # underscore is passed on
    if (grepl("_", name, fixed = TRUE)) next
    header_name <- toupper(name)
    if (!gsub("-", "_", header_name, fixed = TRUE) %in% c(UNFORWARDED_REQUEST_HEADERS, skip)) {
      headers[[header_name]] <- sent[[name]]
    }
  }
  headers
}

forward_request <- function(method, target_url, req, app_name, config, template_manager = NULL) {
  "Forward HTTP requests without blocking the server's event loop"

  proxy_error <- function(e) {
    logger::log_error("Proxy error for app {app_name}: {error}",
      app_name = app_name, error = conditionMessage(e)
    )
    # curl errors name the app's internal address, so keep them in the log
    create_error_response("Bad Gateway", 502)
  }

  # The proxy's own answers that mean "try again shortly". A browser opening
  # the app's page cannot act on a JSON error or a Retry-After header, so it
  # receives a page that reloads itself, with the same status and headers;
  # every other client receives the JSON.
  retry_seconds <- 2
  for_browser <- function(response) {
    opens_page <- identical(method, "GET") &&
      grepl("text/html", req$HTTP_ACCEPT %||% "", fixed = TRUE)
    if (is.null(template_manager) || !opens_page) {
      return(response)
    }
    page <- tryCatch(
      template_manager$generate_starting_page(app_name, retry_seconds),
      error = function(e) {
        logger::log_error("Error generating the starting page: {error}", error = conditionMessage(e))
        NULL
      }
    )
    if (!is.null(page)) {
      response$body <- page
      response$headers[["Content-Type"]] <- "text/html"
    }
    response
  }
  unavailable <- function(message) {
    for_browser(create_503_response(message, retry_seconds))
  }

  tryCatch({
    startup_state <- config$get_app_startup_state(app_name)
    app_config <- config$get_app_config(app_name)
    request_process <- config$get_app_process(app_name)
    grace_period <- app_config$appstart_timeout %||% 2
    wait_seconds <- 0
    starting <- !is.null(startup_state) && startup_state$state == "starting"
    # A startup that took too long is reported to one request, and only if the
    # app still does not answer: its process is still running and may yet do
    # so. This is the one place that ends a timed-out startup state.
    timed_out <- !is.null(startup_state) && startup_state$state == "timeout"
    if (timed_out) config$set_app_ready(app_name)
    if (starting) {
      wait_seconds <- max(0, grace_period - startup_state$elapsed)
      if (wait_seconds == 0) {
        return(unavailable(sprintf("App '%s' is starting up, please retry", app_name)))
      }
    }

    # Capture the request body while the httpuv input stream is available.
    # A new curl handle below owns all transport state for this request only.
    headers <- forwarded_request_headers(req)
    body <- NULL
    if (!method %in% c("GET", "HEAD", "OPTIONS")) {
      if (!is.null(req$rook.input)) body <- req$rook.input$read()
      # A body is passed on when the client sent one, or declared an empty one
      # with "Content-Length: 0". It keeps the client's Content-Type: libcurl
      # gives a body without one "application/x-www-form-urlencoded", which a
      # header with an empty value prevents.
      declared <- !is.null(request_header(req, "content-length"))
      if (length(body) > 0 || declared) {
        body <- body %||% raw()
        headers[["Content-Type"]] <- request_header(req, "content-type") %||% ""
      } else {
        body <- NULL
      }
    }

    # A running app whose own process holds its port needs no probe. While an
    # app starts, or when its port cannot be verified, probe first so the
    # request is never sent before the app is ready.
    probe <- starting || !config$app_backend_verified(app_name)
    backend_ready <- if (probe) {
      wait_for_backend(target_url, wait_seconds)
    } else {
      promises::promise_resolve(TRUE)
    }

    response <- promises::then(backend_ready, function(ready) {
      if (!identical(config$get_app_process(app_name), request_process)) {
        return(unavailable(sprintf("App '%s' restarted, please retry", app_name)))
      }
      if (ready && !config$app_backend_verified(app_name)) {
        logger::log_warn("Not forwarding to app {app_name}: its process does not own port {port}",
          app_name = app_name, port = app_config$port
        )
        ready <- FALSE
      }
      if (!ready) {
        if (timed_out) {
          return(for_browser(create_error_response("App startup timed out", 502)))
        }
        message <- if (starting) {
          sprintf("App '%s' is starting up, please retry", app_name)
        } else {
          sprintf("App '%s' is not ready", app_name)
        }
        return(unavailable(message))
      }
      if (starting) config$set_app_ready(app_name)
      # Bound stalled transfers rather than total time, so slow downloads and
      # documents that render during the request still complete. The handle and
      # its pool are never reused, so close the backend connection when done
      # instead of leaving it open until garbage collection. The app receives
      # the path as the client sent it: libcurl would otherwise remove "." and
      # ".." segments, including their "%2e" spellings.
      handle <- new_backend_handle(
        customrequest = method, nobody = identical(method, "HEAD"), path_as_is = TRUE,
        connecttimeout = 10, low_speed_limit = 1, low_speed_time = 600, followlocation = FALSE,
        accept_encoding = "identity", http_content_decoding = FALSE, forbid_reuse = TRUE
      )
      # An app can refuse an upload from its headers alone, as Shiny does above
      # shiny.maxRequestSize, and then closes the connection. Body data sent
      # after that resets the connection, which can discard the app's answer
      # before it is read. So offer a large body first and send it once the
      # app agrees. The curl package turns this handshake off: it sets no wait
      # and handle_setheaders() adds an empty "Expect" header, hence the
      # lower-case name.
      if (length(body) > EXPECT_CONTINUE_BYTES) {
        headers[["expect"]] <- "100-continue"
        curl::handle_setopt(handle, expect_100_timeout_ms = 1000)
      }
      curl::handle_setheaders(handle, .list = headers)
      if (!is.null(body)) {
        curl::handle_setopt(handle, postfields = body)
      }
      promises::then(fetch_backend_async(target_url, handle), function(response) {
        response_headers <- proxy_response_headers(
          response$headers, target_url, app_name, method, config$config$base_path %||% ""
        )
        # A response that cannot have a body is sent without one. Given an
        # empty body, httpuv compresses it for a client that accepts gzip and
        # sends the result, which may not follow these responses.
        status <- response$status_code
        if (identical(method, "HEAD") || status < 200 || status %in% c(204L, 304L)) {
          discard_response_body(response$content)
          return(list(status = status, headers = response_headers, body = NULL))
        }
        # httpuv sends the body unchanged, from memory or from the file a
        # large one was written to. Scanning or converting the bytes would
        # briefly multiply a large download's memory use.
        list(
          status = status,
          headers = response_headers,
          body = response$content
        )
      }, onRejected = function(e) {
        if (!inherits(e, "backend_connect_error")) stop(e)
        # The app no longer listens where it was verified, so check its port
        # again before the next request is forwarded.
        logger::log_warn("App {app_name} did not accept a connection: {error}",
          app_name = app_name, error = conditionMessage(e)
        )
        if (exists(app_name, envir = config$verified_backends, inherits = FALSE)) {
          rm(list = app_name, envir = config$verified_backends)
        }
        unavailable(sprintf("App '%s' is not ready", app_name))
      })
    })
    promises::then(response, onRejected = proxy_error)
  }, error = proxy_error)
}

# Whether strings hold only ASCII characters, which tolower() can read in any
# encoding, including "bytes"
is_ascii <- function(x) !grepl("[^\001-\177]", x, useBytes = TRUE)

# The header fields of an app's final response, from libcurl's raw header
# block, as a list named by lower-case field name. Each value keeps the bytes
# the app sent. curl::parse_headers_list() returns no fields at all for a
# block that holds a byte that is not valid UTF-8, such as a Latin-1 file
# name, and the response would then be sent without its Content-Type, cookies,
# or redirect. A line whose field name is not an HTTP token is left out.
parse_response_headers <- function(raw_headers) {
  text <- rawToChar(raw_headers)
  Encoding(text) <- "bytes"
  blocks <- strsplit(text, "\r\n\r\n|\n\n|\r\r", useBytes = TRUE)[[1]]
  if (length(blocks) == 0) {
    return(list())
  }
  lines <- strsplit(blocks[length(blocks)], "\r\n|\n|\r", useBytes = TRUE)[[1]]
  colon <- regexpr(":", lines, fixed = TRUE, useBytes = TRUE)
  names <- substr(lines, 1, colon - 1)
  fields <- colon > 1 & grepl("^[!#$%&'*+.^_`|~0-9A-Za-z-]+$", names, useBytes = TRUE)
  values <- as.list(trimws(substr(lines[fields], colon[fields] + 1, nchar(lines[fields], type = "bytes"))))
  names(values) <- tolower(names[fields])
  values
}

# Preserve end-to-end headers, including repeated Set-Cookie fields. Transfer
# framing belongs to httpuv; libcurl already removes chunk framing, but content
# decoding is disabled so Content-Encoding still describes the returned bytes.
# A response the app sent without a Content-Type is passed on without one.
proxy_response_headers <- function(raw_headers, target_url, app_name, method, base_path = "") {
  headers <- parse_response_headers(raw_headers)
  connection <- as.character(unlist(headers[names(headers) == "connection"], use.names = FALSE))
  nominated <- trimws(unlist(strsplit(connection, ",", fixed = TRUE, useBytes = TRUE)))
  nominated <- tolower(nominated[is_ascii(nominated)])
  # httpuv adds its own Date to every response; the app's would be a second one
  excluded <- c("connection", "keep-alive", "transfer-encoding", "te", "trailer",
    "upgrade", "proxy-authenticate", "proxy-authorization", "date", nominated)
  if (method != "HEAD") excluded <- c(excluded, "content-length")
  headers <- headers[!names(headers) %in% excluded]
  # Redirects and cookie paths must name the app's public URL
  prefix <- paste0(base_path, "/proxy/", app_name)
  authority <- sub("^(https?://[^/?#]+).*", "\\1", target_url)
  for (i in seq_along(headers)) {
    value <- headers[[i]]
    if (names(headers)[i] == "location") {
      # Keep backend-local redirects on this app's public proxy route.
      parts <- regmatches(value, regexec("^(?:https?:)?//([^/?#]+)(.*)$", value,
        ignore.case = TRUE, perl = TRUE))[[1]]
      backend_authority <- sub("^https?://", "", authority)
      backend_local <- length(parts) == 3 && is_ascii(parts[2]) &&
        tolower(parts[2]) == tolower(backend_authority)
      if (backend_local) {
        value <- parts[3]
        if (!startsWith(value, "/")) value <- paste0("/", value)
      }
      if (backend_local || (startsWith(value, "/") && !startsWith(value, "//"))) {
        value <- paste0(prefix, value)
      }
      headers[[i]] <- value
    } else if (names(headers)[i] == "set-cookie") {
      # Backend cookies must not become shared cookies for every hosted app.
      if (!grepl(";[[:space:]]*path=/", value, ignore.case = TRUE)) {
        request_path <- sub("[?#].*$", "", substring(target_url, nchar(authority) + 1))
        default_path <- sub("/[^/]*$", "", request_path)
        if (default_path == "" || !startsWith(default_path, "/")) default_path <- "/"
        value <- paste0(value, "; Path=", default_path)
      }
      value <- gsub("(;[[:space:]]*path=)(/[^;]*)", paste0("\\1", prefix, "\\2"), value,
        ignore.case = TRUE, perl = TRUE)
      value <- gsub(";[[:space:]]*domain=\\.?((127\\.0\\.0\\.1)|localhost)(?=;|$)", "", value,
        ignore.case = TRUE, perl = TRUE)
      headers[[i]] <- value
    }
  }
  headers
}

# Apps listen on loopback, so their traffic must never follow http_proxy or
# ALL_PROXY: a proxy cannot reach the app, and would be sent users' cookies
# and request bodies.
new_backend_handle <- function(...) {
  curl::new_handle(..., noproxy = "*")
}

# A transfer that failed before a connection was established never reached
# the app. Such failures carry the class "backend_connect_error".
backend_fetch_error <- function(message, handle) {
  connected <- tryCatch(
    curl::handle_data(handle)$times[["connect"]] > 0,
    error = function(e) TRUE
  )
  structure(
    class = c(if (!isTRUE(connected)) "backend_connect_error", "simpleError", "error", "condition"),
    list(message = as.character(message), call = NULL)
  )
}

# An app's response is held in memory up to this size. A larger one is written
# to a temporary file as it arrives and sent from there, so that a big
# download does not occupy the memory of the process that serves every app.
RESPONSE_SPOOL_BYTES <- 8 * 1024^2

# Collects a response body from curl's data callback. finish() returns it in
# a form httpuv sends: a raw vector, or a file that httpuv deletes once it has
# been sent.
response_body_collector <- function(limit = RESPONSE_SPOOL_BYTES) {
  chunks <- list()
  size <- 0
  path <- NULL
  writer <- NULL
  list(
    write = function(data, final = FALSE) {
      if (length(data) == 0) {
        return(invisible(NULL))
      }
      size <<- size + length(data)
      if (is.null(writer) && size > limit) {
        # curl's writer holds no R connection, of which a session has few
        path <<- tempfile("tss-response-")
        writer <<- curl::file_writer(path)
        for (chunk in chunks) writer(chunk)
        chunks <<- list()
      }
      if (is.null(writer)) chunks[[length(chunks) + 1L]] <<- data else writer(data)
      invisible(NULL)
    },
    finish = function() {
      if (is.null(writer)) {
        return(if (length(chunks) > 0) unlist(chunks, use.names = FALSE) else raw())
      }
      writer(raw(), close = TRUE)
      # The file now belongs to the response: a later discard() leaves it
      spooled <- list(file = path, owned = TRUE)
      writer <<- NULL
      path <<- NULL
      spooled
    },
    discard = function() {
      chunks <<- list()
      if (!is.null(writer)) {
        writer(raw(), close = TRUE)
        writer <<- NULL
      }
      if (!is.null(path)) {
        unlink(path)
        path <<- NULL
      }
    }
  )
}

# Remove the file of a collected body that will not be sent
discard_response_body <- function(body) {
  if (is.list(body) && !is.null(body$file)) unlink(body$file)
}

# Drive libcurl without blocking: after each step, wait for the transfer's own
# sockets, so other HTTP and WebSocket callbacks run in between and a transfer
# continues as soon as it can. Polling on a timer instead sent one buffer per
# tick, which capped uploads at a few megabytes per second. Each transfer has
# its own pool and fresh easy handle; neither cookies nor authentication state
# can survive into another request. The response's `content` is what
# response_body_collector() returns.
fetch_backend_async <- function(url, handle) {
  promises::promise(function(resolve, reject) {
    pool <- curl::new_pool()
    body <- response_body_collector()
    curl::curl_fetch_multi(url, handle = handle, pool = pool,
      data = body$write,
      done = function(response) {
        response$content <- body$finish()
        resolve(response)
      },
      fail = function(message) {
        body$discard()
        reject(backend_fetch_error(message, handle))
      }
    )
    # later_fd() passes the sockets' readiness, which multi_run() finds itself
    pump <- function(...) {
      tryCatch({
        result <- curl::multi_run(timeout = 0, pool = pool)
        if (result$pending > 0) {
          fds <- curl::multi_fdset(pool)
          # libcurl's timers (connect and stall timeouts) need a call even
          # when no socket becomes ready; its timeout is in milliseconds
          wait <- if (fds$timeout < 0) 1 else min(fds$timeout / 1000, 1)
          if (length(fds$reads) + length(fds$writes) + length(fds$exceptions) == 0) {
            # Nothing to wait on yet, for example before the socket exists
            later::later(pump, min(wait, 0.01))
          } else {
            later::later_fd(pump, fds$reads, fds$writes, fds$exceptions, timeout = wait)
          }
        }
      }, error = function(e) {
        curl::multi_cancel(handle)
        body$discard()
        reject(e)
      })
    }
    later::later(pump, 0)
  })
}

# Probe TCP readiness without sending an HTTP request (in particular, never
# replay a POST while waiting for startup). Both the probe and retry are async.
wait_for_backend <- function(url, wait_seconds = 0) {
  promises::promise(function(resolve, reject) {
    deadline <- Sys.time() + wait_seconds
    probe <- function() {
      remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
      timeout_ms <- if (wait_seconds > 0) max(1, min(1000, remaining * 1000)) else 1000
      # Close the probe connection when it completes. Before libcurl 8, a
      # connect-only connection otherwise stays open in the unused pool until
      # garbage collection.
      handle <- new_backend_handle(connect_only = TRUE, forbid_reuse = TRUE,
        timeout_ms = ceiling(timeout_ms))
      promises::then(fetch_backend_async(url, handle),
        onFulfilled = function(response) resolve(TRUE),
        onRejected = function(error) {
          remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
          if (remaining > 0) {
            later::later(probe, min(0.1, remaining))
          } else {
            resolve(FALSE)
          }
        }
      )
    }
    probe()
  })
}

is_allowed_proxy_host <- function(req, config) {
  "Whether the proxy should serve a request with this Host header"

  # A loopback-only proxy must not answer other names, or a DNS-rebinding
  # page could become same-origin with it and drive local apps
  proxy_host <- config$config$proxy_host %||% "127.0.0.1"
  if (proxy_host %in% c("127.0.0.1", "localhost", "::1")) {
    return(is_loopback_host_header(req$HTTP_HOST))
  }
  TRUE
}

is_same_origin_websocket <- function(req) {
  "Whether a WebSocket handshake has no Origin or one matching the request host"

  origin <- req$HTTP_ORIGIN
  if (is.null(origin) || identical(origin, "")) {
    return(TRUE) # Non-browser clients do not send Origin
  }
  origin <- tolower(trimws(origin))
  match <- regmatches(origin, regexec("^(https?)://([^/]+)$", origin))[[1]]
  if (length(match) != 3) {
    return(FALSE)
  }
  normalize <- function(authority, default_port) {
    sub(paste0(":", default_port, "$"), "", tolower(trimws(authority)))
  }
  default_port <- if (match[2] == "https") "443" else "80"
  origin_authority <- normalize(match[3], default_port)

  hosts <- req$HTTP_HOST
  # A reverse proxy on this machine may rewrite Host but report the original
  remote_addr <- req$REMOTE_ADDR
  forwarded_host <- request_header(req, "x-forwarded-host")
  if (!is.null(remote_addr) && remote_addr %in% c("127.0.0.1", "::1", "::ffff:127.0.0.1") &&
      !is.null(forwarded_host)) {
    hosts <- c(hosts, strsplit(forwarded_host, ",")[[1]])
  }
  hosts <- hosts[!is.na(hosts) & trimws(hosts) != ""]
  origin_authority %in% normalize(hosts, default_port)
}

# WebSocket handler
handle_websocket_connection <- function(ws, config, connection_manager, process_manager = NULL) {
  "Handle new WebSocket connections"

  logger::log_info("WebSocket connection opened")

  if (!is_allowed_proxy_host(ws$request, config)) {
    logger::log_warn("Rejecting WebSocket with Host {host}", host = ws$request$HTTP_HOST)
    ws$close()
    return()
  }

  # Browsers attach cookies and cached credentials to cross-site WebSocket
  # handshakes, so only accept sessions opened by pages served from this host
  if (!is_same_origin_websocket(ws$request)) {
    logger::log_warn("Rejecting cross-origin WebSocket from {origin}", origin = ws$request$HTTP_ORIGIN)
    ws$close()
    return()
  }

  # Generate session ID
  session_id <- generate_session_id()

  # Determine which app this WebSocket is for
  request_path_validation <- validate_path(strip_base_path(ws$request$PATH_INFO, config$config$base_path))
  if (!request_path_validation$valid) {
    logger::log_error("Invalid WebSocket path: {error}", error = request_path_validation$error)
    ws$close()
    return()
  }

  # Do not fall back to another app when the path names none
  route <- parse_proxy_path(request_path_validation$sanitized)
  if (!route$valid) {
    logger::log_error("Could not determine app for WebSocket connection: {error}", error = route$error)
    ws$close()
    return()
  }
  app_name <- route$app_name

  logger::log_info("WebSocket routed to app: {app_name}", app_name = app_name)

  app_config <- config$get_app_config(app_name)
  if (is.null(app_config)) {
    logger::log_warn("Rejecting WebSocket for unknown app: {app_name}", app_name = app_name)
    ws$close()
    return()
  }

  if (!ensure_on_demand_app_started(app_config, config, process_manager, "WebSocket connection")) {
    logger::log_error("Failed to start app {app_name} on demand for WebSocket", app_name = app_name)
    ws$close()
    return()
  }

  # Check if app is still starting up
  if (config$is_app_starting(app_name)) {
    logger::log_info("App {app_name} is starting, closing WebSocket with retry message", app_name = app_name)
    connection_manager$schedule_session_check(app_name,
      (app_config$appstart_timeout %||% 2) + config$HTTP_SESSION_GRACE_SECONDS)
    ws$send(jsonlite::toJSON(list(
      error = "App is starting",
      message = "The application is starting up. Please refresh the page in a few seconds.",
      retry_after_seconds = 3
    ), auto_unbox = TRUE))
    ws$close()
    return()
  }

  # Get client connection info
  client_ip <- get_client_ip(ws$request)
  user_agent <- ws$request$HTTP_USER_AGENT %||% "unknown"

  # Add connection to manager
  connection_manager$add_client_connection(session_id, ws, app_name, client_ip, user_agent)

  # Set up message handler
  ws$onMessage(function(binary, message) {
    tryCatch(
      {
        success <- connection_manager$handle_client_message(session_id, message, app_name)
        if (!success) {
          tryCatch(ws$send(jsonlite::toJSON(list(error = "Invalid message"), auto_unbox = TRUE)), error = function(e) {})
          tryCatch(ws$close(), error = function(e) {})
        }
      },
      error = function(e) {
        logger::log_error("Error handling WebSocket message: {error}", error = e$message)
        tryCatch(ws$close(), error = function(e) {})
      }
    )
  })

  # Set up close handler
  ws$onClose(function() {
    connection_manager$remove_client_connection(session_id)
  })

  # Connect to the app now rather than on the browser's first message: on
  # some sockets, such as Shiny's autoreload socket, only the app speaks.
  # Messages that arrive before the app's socket opens are queued.
  tryCatch(
    connection_manager$create_backend_connection(app_name, session_id, ws),
    error = function(e) {
      logger::log_error("Could not connect to app {app_name}: {error}",
        app_name = app_name, error = conditionMessage(e)
      )
      connection_manager$close_client_connection(session_id)
    }
  )
}
