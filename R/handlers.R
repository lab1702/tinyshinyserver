# HTTP and WebSocket Handlers Module
# Handles all HTTP requests and WebSocket connections

# HTTP request handler
handle_http_request <- function(req, config, template_manager, connection_manager, process_manager = NULL) {
  "Main HTTP request handler with routing"

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

        # Sort apps alphabetically by name
        sorted_apps <- config$config$apps[order(sapply(config$config$apps, function(app) app$name))]

        for (app_config in sorted_apps) {
          app_name <- app_config$name
          process <- config$get_app_process(app_name)

          status <- if (is.null(process)) {
            if (app_config$resident) "stopped" else "dormant"
          } else if (is_process_alive(process)) {
            "running"
          } else {
            "crashed"
          }

          # Count connections for this app
          app_connections <- 0
          for (conn in config$get_all_ws_connections()) {
            if (!is.null(conn$app_name) && conn$app_name == app_name) {
              app_connections <- app_connections + 1
            }
          }

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

  # Build target URL
  if (length(path_parts) > 2) {
    target_path <- paste0("/", paste(path_parts[3:length(path_parts)], collapse = "/"))
  } else {
    target_path <- "/"
  }

  target_url <- paste0("http://127.0.0.1:", app_config$port, target_path)

  # Add query string if present
  if (!is.null(query_string) && query_string != "") {
    target_url <- paste0(target_url, "?", query_string)
  }

  # Forward the request
  return(forward_request(method, target_url, req, app_name))
}

forward_request <- function(method, target_url, req, app_name) {
  "Forward HTTP request to backend Shiny app"

  tryCatch(
    {
      logger::log_debug("Forwarding {method} to {target_url} for app {app_name}",
        method = method, target_url = target_url, app_name = app_name
      )

      # Make the request with timeout and connection timeout
      # Configure timeouts: 30s total, 10s connection establishment  
      timeout_config <- httr::timeout(30)
      # Set connection timeout to 10 seconds using curl option
      connect_config <- httr::config(connecttimeout = 10)
      
      if (method == "GET") {
        response <- httr::GET(target_url, timeout_config, connect_config)
      } else if (method == "POST") {
        # Handle POST data
        body <- req$rook.input$read_lines()
        response <- httr::POST(target_url, body = body, timeout_config, connect_config)
      } else {
        # Handle other methods
        response <- httr::VERB(method, target_url, timeout_config, connect_config)
      }

      # Get response headers safely
      response_headers <- response$headers
      content_type <- if (!is.null(response_headers) && "content-type" %in% names(response_headers)) {
        response_headers[["content-type"]]
      } else {
        "text/html"
      }

      # Handle binary vs text content
      raw_content <- httr::content(response, "raw")

      # Check if content is binary
      is_binary <- grepl("image/|font/|application/octet-stream|application/pdf", content_type, ignore.case = TRUE) ||
        any(raw_content == 0)

      # Return response
      if (is_binary) {
        return(list(
          status = httr::status_code(response),
          headers = list("Content-Type" = content_type),
          body = raw_content
        ))
      } else {
        return(list(
          status = httr::status_code(response),
          headers = list("Content-Type" = content_type),
          body = rawToChar(raw_content)
        ))
      }
    },
    error = function(e) {
      logger::log_error("Proxy error for app {app_name}: {error}",
        app_name = app_name, error = e$message
      )
      return(create_error_response(paste("Bad Gateway:", e$message), 502))
    }
  )
}

# WebSocket handler
handle_websocket_connection <- function(ws, config, connection_manager, process_manager = NULL) {
  "Handle new WebSocket connections"

  logger::log_info("WebSocket connection opened")

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

  # Get client connection info
  client_ip <- get_client_ip(ws$request)
  user_agent <- ws$request$HTTP_USER_AGENT %||% "unknown"

  # Add connection to manager
  connection_manager$add_client_connection(session_id, ws, app_name, client_ip, user_agent)

  # Set up message handler
  ws$onMessage(function(binary, message) {
    success <- connection_manager$handle_client_message(session_id, message, app_name)
    if (!success) {
      ws$send('{"error": "Invalid message"}')
      ws$close()
    }
  })

  # Set up close handler
  ws$onClose(function() {
    connection_manager$remove_client_connection(session_id)
  })
}

extract_app_name_from_ws_path <- function(request_path, config) {
  "Extract app name from WebSocket request path"

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

  # Default to first app if no specific routing
  if (is.null(app_name) && length(config$config$apps) > 0) {
    app_name <- config$config$apps[[1]]$name
  }

  return(app_name)
}
