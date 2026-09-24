# HTTP and WebSocket Handlers Module
# Handles all HTTP requests and WebSocket connections

# HTTP request handler
handle_http_request <- function(req, config, template_manager, connection_manager, process_manager = NULL) {
  "Main HTTP request handler with routing"

  if (!is_allowed_proxy_host(req, config)) {
    return(create_error_response("Invalid Host header", 403))
  }

  # Validate request inputs
  validation_result <- validate_request_inputs(
    req$PATH_INFO,
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

  # Attach managers to request for routing
  req$process_manager <- process_manager
  req$connection_manager <- connection_manager

  # Route handling
  return(route_http_request(path, method, query_string, req, config, template_manager, connection_manager))
}

route_http_request <- function(path, method, query_string, req, config, template_manager, connection_manager) {
  "Route HTTP requests to appropriate handlers"

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
    return(handle_apps_api(config, req$process_manager))
  }

  # Static files (CSS, JS, images)
  if (startsWith(path, "/templates/")) {
    return(handle_static_file(path, template_manager))
  }

  # Proxy requests to apps
  if (startsWith(path, "/proxy/")) {
    # We need to get references to process_manager and connection_manager
    # This will be passed from the main HTTP handler
    return(handle_proxy_request(
      path, method, query_string, req, config,
      req$process_manager, req$connection_manager
    ))
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
      if (!is.null(process_manager)) {
        # Use the enhanced status from process manager
        apps_status <- process_manager$get_all_app_status()
      } else {
        # Fallback to basic status if no process manager available
        apps_status <- list()

        for (app_config in config$get_sorted_apps()) {
          app_name <- app_config$name
          process <- config$get_app_process(app_name)

          status <- if (is.null(process)) {
            if (app_config$resident) "stopped" else "dormant"
          } else if (is_process_alive(process)) {
            "running"
          } else {
            "crashed"
          }

          # Get connection count from cache (O(1) instead of O(n))
          app_connections <- config$get_app_connection_count(app_name)

          apps_status[[app_name]] <- list(
            name = app_name,
            status = status,
            resident = app_config$resident,
            port = app_config$port,
            path = app_config$path,
            connections = app_connections,
            pid = if (!is.null(process) && is_process_alive(process)) process$get_pid() else NULL
          )
        }
      }

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

handle_static_file <- function(path, template_manager) {
  "Handle static file requests (CSS, JS, images)"

  # Remove /templates/ prefix
  file_path <- gsub("^/templates/", "", path)

  return(template_manager$serve_static_file(file_path))
}

handle_proxy_request <- function(path, method, query_string, req, config, process_manager = NULL, connection_manager = NULL) {
  "Handle proxy requests to Shiny apps"

  if (!startsWith(path, "/proxy/") || startsWith(path, "/proxy//")) {
    return(create_error_response("Invalid proxy path", 400))
  }

  path_parts <- strsplit(path, "/")[[1]]
  path_parts <- path_parts[path_parts != ""] # Remove empty parts

  if (length(path_parts) < 2) {
    return(create_error_response("Invalid proxy path", 400))
  }

  # Validate app name
  app_name_validation <- validate_app_name(path_parts[2])
  if (!app_name_validation$valid) {
    return(create_error_response(paste("Invalid app name:", app_name_validation$error), 400))
  }

  app_name <- app_name_validation$sanitized
  app_config <- config$get_app_config(app_name)

  if (is.null(app_config)) {
    return(create_error_response("App not found", 404))
  }

  # Canonicalize the mount URL before starting an app or forwarding a body.
  if (identical(path, paste0("/proxy/", app_name))) {
    query <- query_string %||% ""
    if (query != "" && !startsWith(query, "?")) query <- paste0("?", query)
    return(list(status = 308L, headers = list(Location = paste0(path, "/", query)), body = ""))
  }

  # Start app on demand if it's non-resident and not running
  if (!app_config$resident && !is.null(process_manager)) {
    process <- config$get_app_process(app_name)
    if (is.null(process) || !is_process_alive(process)) {
      logger::log_info("Starting non-resident app {app_name} on demand for HTTP request", app_name = app_name)
      success <- process_manager$start_app_on_demand(app_name)
      if (!success) {
        return(create_error_response("Failed to start app on demand", 502))
      }
    }
  }

  # Remove only the routing prefix, preserving the backend path verbatim.
  target_path <- substring(path, nchar(paste0("/proxy/", app_name)) + 1L)
  if (target_path == "") target_path <- "/"
  target_url <- paste0("http://127.0.0.1:", app_config$port, target_path)

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
    response <- forward_request(method, target_url, req, app_name, config)
    if (promises::is.promise(response)) return(promises::finally(response, release))
    release()
    response
  }, error = function(e) {
    release()
    stop(e)
  })
}

forward_request <- function(method, target_url, req, app_name, config) {
  "Forward HTTP requests without blocking the server's event loop"

  proxy_error <- function(e) {
    logger::log_error("Proxy error for app {app_name}: {error}",
      app_name = app_name, error = conditionMessage(e)
    )
    create_error_response(paste("Bad Gateway:", conditionMessage(e)), 502)
  }

  tryCatch({
    startup_state <- config$get_app_startup_state(app_name)
    app_config <- config$get_app_config(app_name)
    request_process <- config$get_app_process(app_name)
    grace_period <- app_config$appstart_timeout %||% 2
    wait_seconds <- 0
    starting <- !is.null(startup_state)
    if (starting) {
      if (startup_state$state == "timeout") {
        return(create_error_response("App startup timed out", 502))
      }
      wait_seconds <- max(0, grace_period - startup_state$elapsed)
      if (wait_seconds == 0) {
        return(create_503_response(
          sprintf("App '%s' is starting up, please retry", app_name), 2
        ))
      }
    }

    # Capture the request body while the httpuv input stream is available.
    # A new curl handle below owns all transport state for this request only.
    skip_headers <- c(
      "HOST", "CONNECTION", "KEEP_ALIVE", "TRANSFER_ENCODING",
      "TE", "TRAILER", "UPGRADE", "PROXY_AUTHORIZATION",
      "PROXY_AUTHENTICATE", "CONTENT_LENGTH", "CONTENT_TYPE"
    )
    headers <- list()
    for (name in names(req)) {
      if (startsWith(name, "HTTP_")) {
        header_name <- substring(name, 6)
        if (!header_name %in% skip_headers) {
          headers[[gsub("_", "-", header_name)]] <- req[[name]]
        }
      }
    }
    body <- NULL
    if (!method %in% c("GET", "HEAD", "OPTIONS")) {
      if (!is.null(req$rook.input)) body <- req$rook.input$read()
      content_type <- req$CONTENT_TYPE %||% req$HTTP_CONTENT_TYPE
      if (!is.null(body) && length(body) > 0 && !is.null(content_type)) {
        headers[["Content-Type"]] <- content_type
      }
    }

    response <- promises::then(wait_for_backend(target_url, wait_seconds), function(ready) {
      if (!identical(config$get_app_process(app_name), request_process)) {
        return(create_503_response(sprintf("App '%s' restarted, please retry", app_name), 2))
      }
      if (ready && !config$app_backend_verified(app_name)) {
        logger::log_warn("Not forwarding to app {app_name}: its process does not own port {port}",
          app_name = app_name, port = app_config$port
        )
        ready <- FALSE
      }
      if (!ready) {
        message <- if (starting) {
          sprintf("App '%s' is starting up, please retry", app_name)
        } else {
          sprintf("App '%s' is not ready", app_name)
        }
        return(create_503_response(message, 2))
      }
      if (starting) config$set_app_ready(app_name)
      # Bound stalled transfers rather than total time, so slow downloads and
      # documents that render during the request still complete.
      handle <- curl::new_handle(
        customrequest = method, nobody = identical(method, "HEAD"),
        connecttimeout = 10, low_speed_limit = 1, low_speed_time = 600, followlocation = FALSE,
        accept_encoding = "identity", http_content_decoding = FALSE
      )
      curl::handle_setheaders(handle, .list = headers)
      if (!is.null(body) && length(body) > 0) {
        curl::handle_setopt(handle, postfields = body)
      }
      promises::then(fetch_backend_async(target_url, handle), function(response) {
        response_headers <- proxy_response_headers(response$headers, target_url, app_name, method)
        content_type <- response_headers[["content-type"]] %||% "text/html"
        content <- response$content
        is_binary <- grepl("image/|font/|application/octet-stream|application/pdf",
          content_type, ignore.case = TRUE
        ) || any(content == 0) || !is.null(response_headers[["content-encoding"]])
        if (is.null(response_headers[["content-type"]])) response_headers[["content-type"]] <- content_type
        list(
          status = response$status_code,
          headers = response_headers,
          body = if (is_binary) content else rawToChar(content)
        )
      })
    })
    promises::then(response, onRejected = proxy_error)
  }, error = proxy_error)
}

# Preserve end-to-end headers, including repeated Set-Cookie fields. Transfer
# framing belongs to httpuv; libcurl already removes chunk framing, but content
# decoding is disabled so Content-Encoding still describes the returned bytes.
proxy_response_headers <- function(raw_headers, target_url, app_name, method) {
  headers <- curl::parse_headers_list(raw_headers)
  connection <- as.character(unlist(headers[names(headers) == "connection"], use.names = FALSE))
  nominated <- tolower(trimws(unlist(strsplit(connection, ",", fixed = TRUE))))
  excluded <- c("connection", "keep-alive", "transfer-encoding", "te", "trailer",
    "upgrade", "proxy-authenticate", "proxy-authorization", nominated)
  if (method != "HEAD") excluded <- c(excluded, "content-length")
  headers <- headers[!names(headers) %in% excluded]
  prefix <- paste0("/proxy/", app_name)
  authority <- sub("^(https?://[^/?#]+).*", "\\1", target_url)
  for (i in seq_along(headers)) {
    value <- headers[[i]]
    if (names(headers)[i] == "location") {
      # Keep backend-local redirects on this app's public proxy route.
      parts <- regmatches(value, regexec("^(?:https?:)?//([^/?#]+)(.*)$", value,
        ignore.case = TRUE, perl = TRUE))[[1]]
      backend_authority <- sub("^https?://", "", authority)
      backend_local <- length(parts) == 3 && tolower(parts[2]) == tolower(backend_authority)
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

# Drive libcurl with zero-timeout polls so other HTTP and WebSocket callbacks
# can run between polls. Each transfer has its own pool and fresh easy handle;
# neither cookies nor authentication state can survive into another request.
fetch_backend_async <- function(url, handle) {
  promises::promise(function(resolve, reject) {
    pool <- curl::new_pool()
    curl::curl_fetch_multi(url, handle = handle, pool = pool,
      done = resolve, fail = function(message) reject(simpleError(as.character(message)))
    )
    pump <- function() {
      tryCatch({
        result <- curl::multi_run(timeout = 0, pool = pool)
        if (result$pending > 0) later::later(pump, 0.01)
      }, error = function(e) {
        curl::multi_cancel(handle)
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
      handle <- curl::new_handle(connect_only = TRUE, timeout_ms = ceiling(timeout_ms))
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
  if (!is.null(remote_addr) && remote_addr %in% c("127.0.0.1", "::1", "::ffff:127.0.0.1") &&
      !is.null(req$HTTP_X_FORWARDED_HOST)) {
    hosts <- c(hosts, strsplit(req$HTTP_X_FORWARDED_HOST, ",")[[1]])
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
  session_id <- generate_session_id(ws$request)

  # Determine which app this WebSocket is for
  request_path_validation <- validate_path(ws$request$PATH_INFO)
  if (!request_path_validation$valid) {
    logger::log_error("Invalid WebSocket path: {error}", error = request_path_validation$error)
    ws$close()
    return()
  }

  request_path <- request_path_validation$sanitized
  app_name <- extract_app_name_from_ws_path(request_path, config)

  if (is.null(app_name)) {
    logger::log_error("Could not determine app for WebSocket connection")
    ws$close()
    return()
  }

  logger::log_info("WebSocket routed to app: {app_name}", app_name = app_name)

  # Start app on demand if it's non-resident and not running
  app_config <- config$get_app_config(app_name)
  if (is.null(app_config)) {
    logger::log_warn("Rejecting WebSocket for unknown app: {app_name}", app_name = app_name)
    ws$close()
    return()
  }
  if (!is.null(app_config) && !app_config$resident && !is.null(process_manager)) {
    process <- config$get_app_process(app_name)
    if (is.null(process) || !is_process_alive(process)) {
      logger::log_info("Starting non-resident app {app_name} on demand for WebSocket connection", app_name = app_name)
      success <- process_manager$start_app_on_demand(app_name)
      if (!success) {
        logger::log_error("Failed to start app {app_name} on demand for WebSocket", app_name = app_name)
        ws$close()
        return()
      }
    }
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
}

extract_app_name_from_ws_path <- function(request_path, config) {
  "Extract app name from WebSocket request path

  Returns NULL if app cannot be determined from path.
  Caller should handle NULL by rejecting the connection with an error.
  "

  app_name <- NULL

  if (startsWith(request_path, "/proxy/")) {
    path_parts <- strsplit(request_path, "/")[[1]]
    path_parts <- path_parts[path_parts != ""]
    if (length(path_parts) >= 2) {
      # Validate app name for WebSocket routing
      app_name_validation <- validate_app_name(path_parts[2])
      if (app_name_validation$valid) {
        app_name <- app_name_validation$sanitized
      }
    }
  }

  # Do NOT default to first app - return NULL if routing failed
  # This prevents silently routing connections to the wrong app
  return(app_name)
}
