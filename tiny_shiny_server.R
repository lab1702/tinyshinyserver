#!/usr/bin/env Rscript

# WebSocket-enabled Shiny Proxy Server
# This replaces the plumber-based proxy with full WebSocket support

library(httpuv)
library(websocket)
library(jsonlite)
library(callr)
library(later)
library(httr)
library(digest)
library(future)
library(promises)
library(logger)

# Global variables
config <- NULL
app_processes <- list()
ws_connections <- list()
backend_connections <- list()

# Memory management constants
MAX_PENDING_MESSAGES <- 100
CONNECTION_TIMEOUT_MINUTES <- 30
CLEANUP_INTERVAL_SECONDS <- 300 # 5 minutes

# Input validation constants
MAX_PATH_LENGTH <- 1000
MAX_QUERY_LENGTH <- 2048
MAX_MESSAGE_SIZE <- 1048576 # 1MB
ALLOWED_HTTP_METHODS <- c("GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS")

# Input validation functions
validate_path <- function(path) {
  if (is.null(path) || !is.character(path) || length(path) != 1) {
    return(list(valid = FALSE, error = "Invalid path type"))
  }

  if (nchar(path) > MAX_PATH_LENGTH) {
    return(list(valid = FALSE, error = "Path too long"))
  }

  # Check for null bytes
  if (grepl("\\x00", path, useBytes = TRUE)) {
    return(list(valid = FALSE, error = "Path contains null bytes"))
  }

  # Check for path traversal attempts (only dangerous patterns)
  if (grepl("\\.\\.[\\/\\\\]", path) || grepl("[\\/\\\\]\\.\\.", path)) {
    return(list(valid = FALSE, error = "Path traversal detected"))
  }

  # Check for invalid characters (only reject very dangerous control chars)
  # ASCII 1-8: SOH, STX, ETX, EOT, ENQ, ACK, BEL, BS
  # ASCII 14-31: SO, SI, DLE, DC1-4, NAK, SYN, ETB, CAN, EM, SUB, ESC, FS, GS, RS, US
  # ASCII 127: DEL
  if (grepl("[\001-\010\016-\037\177]", path)) {
    return(list(valid = FALSE, error = "Path contains invalid characters"))
  }

  return(list(valid = TRUE, sanitized = path))
}

validate_http_method <- function(method) {
  if (is.null(method) || !is.character(method) || length(method) != 1) {
    return(list(valid = FALSE, error = "Invalid method type"))
  }

  method <- toupper(trimws(method))

  if (!method %in% ALLOWED_HTTP_METHODS) {
    return(list(valid = FALSE, error = "HTTP method not allowed"))
  }

  return(list(valid = TRUE, sanitized = method))
}

validate_query_string <- function(query_string) {
  if (is.null(query_string)) {
    return(list(valid = TRUE, sanitized = ""))
  }

  if (!is.character(query_string) || length(query_string) != 1) {
    return(list(valid = FALSE, error = "Invalid query string type"))
  }

  if (nchar(query_string) > MAX_QUERY_LENGTH) {
    return(list(valid = FALSE, error = "Query string too long"))
  }

  # Check for null bytes
  if (grepl("\\x00", query_string, useBytes = TRUE)) {
    return(list(valid = FALSE, error = "Query string contains null bytes"))
  }

  # Basic URL encoding validation
  if (grepl("%[^0-9A-Fa-f]", query_string) || grepl("%[0-9A-Fa-f][^0-9A-Fa-f]", query_string)) {
    return(list(valid = FALSE, error = "Invalid URL encoding"))
  }

  return(list(valid = TRUE, sanitized = query_string))
}

validate_ws_message <- function(message) {
  if (is.null(message)) {
    return(list(valid = FALSE, error = "Message is null"))
  }

  if (!is.character(message) || length(message) != 1) {
    return(list(valid = FALSE, error = "Invalid message type"))
  }

  if (nchar(message, type = "bytes") > MAX_MESSAGE_SIZE) {
    return(list(valid = FALSE, error = "Message too large"))
  }

  # Check for null bytes
  if (grepl("\\x00", message, useBytes = TRUE)) {
    return(list(valid = FALSE, error = "Message contains null bytes"))
  }

  return(list(valid = TRUE, sanitized = message))
}

validate_app_name <- function(app_name) {
  if (is.null(app_name) || !is.character(app_name) || length(app_name) != 1) {
    return(list(valid = FALSE, error = "Invalid app name type"))
  }

  # Allow only alphanumeric, hyphen, underscore
  if (!grepl("^[a-zA-Z0-9_-]+$", app_name)) {
    return(list(valid = FALSE, error = "App name contains invalid characters"))
  }

  if (nchar(app_name) > 50) {
    return(list(valid = FALSE, error = "App name too long"))
  }

  return(list(valid = TRUE, sanitized = app_name))
}

validate_config <- function(config) {
  if (!is.list(config)) {
    return(list(valid = FALSE, error = "Config must be a list"))
  }

  # Validate required fields
  required_fields <- c("apps", "log_dir")
  for (field in required_fields) {
    if (!field %in% names(config)) {
      return(list(valid = FALSE, error = paste("Missing required field:", field)))
    }
  }

  # Validate apps array
  if (!is.list(config$apps) || length(config$apps) == 0) {
    return(list(valid = FALSE, error = "Apps must be a non-empty list"))
  }

  # Validate each app configuration
  for (i in seq_along(config$apps)) {
    app <- config$apps[[i]]

    if (!is.list(app)) {
      return(list(valid = FALSE, error = paste("App", i, "must be a list")))
    }

    # Check required app fields
    app_required <- c("name", "port", "path")
    for (field in app_required) {
      if (!field %in% names(app)) {
        return(list(valid = FALSE, error = paste("App", i, "missing field:", field)))
      }
    }

    # Validate app name
    name_validation <- validate_app_name(app$name)
    if (!name_validation$valid) {
      return(list(valid = FALSE, error = paste("App", i, "name validation:", name_validation$error)))
    }

    # Validate port
    if (!is.numeric(app$port) || length(app$port) != 1 || app$port < 1 || app$port > 65535) {
      return(list(valid = FALSE, error = paste("App", i, "invalid port")))
    }

    # Validate path
    if (!is.character(app$path) || length(app$path) != 1) {
      return(list(valid = FALSE, error = paste("App", i, "path must be a string")))
    }
  }

  # Validate proxy_port if present
  if ("proxy_port" %in% names(config)) {
    if (!is.numeric(config$proxy_port) || length(config$proxy_port) != 1 ||
      config$proxy_port < 1 || config$proxy_port > 65535) {
      return(list(valid = FALSE, error = "Invalid proxy_port"))
    }
  }

  # Validate proxy_host if present
  if ("proxy_host" %in% names(config)) {
    if (!is.character(config$proxy_host) || length(config$proxy_host) != 1) {
      return(list(valid = FALSE, error = "proxy_host must be a string"))
    }

    # Allow common host values
    allowed_hosts <- c("localhost", "127.0.0.1", "0.0.0.0", "::1", "::")
    if (!config$proxy_host %in% allowed_hosts) {
      return(list(valid = FALSE, error = paste("proxy_host must be one of:", paste(allowed_hosts, collapse = ", "))))
    }
  }

  # Validate log_dir
  if (!is.character(config$log_dir) || length(config$log_dir) != 1) {
    return(list(valid = FALSE, error = "log_dir must be a string"))
  }

  return(list(valid = TRUE, sanitized = config))
}


# Load configuration
load_config <- function() {
  if (file.exists("config.json")) {
    # Read with explicit UTF-8 encoding
    config_text <- readLines("config.json", encoding = "UTF-8", warn = FALSE)
    parsed_config <- fromJSON(paste(config_text, collapse = "\n"), simplifyDataFrame = FALSE)

    # Validate configuration
    validation_result <- validate_config(parsed_config)
    if (!validation_result$valid) {
      stop(paste("Configuration validation failed:", validation_result$error))
    }

    config <<- validation_result$sanitized
  } else {
    stop("config.json not found")
  }
}

# Initialize proper logging
setup_logging <- function() {
  # Ensure log directory exists
  if (!dir.exists(config$log_dir)) dir.create(config$log_dir, recursive = TRUE)

  # Set log level threshold
  log_threshold(INFO)

  # Configure file appender with same format as console
  log_file <- file.path(config$log_dir, "server.log")

  # Use a simple custom appender that writes to both console and file
  log_appender(function(lines) {
    # Write to console (stderr by default)
    cat(lines, sep = "\n", file = stderr())

    # Write to file with same format
    tryCatch(
      {
        cat(lines, sep = "\n", file = log_file, append = TRUE)
      },
      error = function(e) {
        # If file writing fails, continue with console only
        warning("Could not write to log file: ", e$message)
      }
    )
  })

  log_info("Logging system initialized with file output")
}

# Start Shiny app process
start_app <- function(app_config) {
  app_name <- app_config$name
  app_port <- app_config$port
  app_path <- normalizePath(app_config$path, mustWork = FALSE)

  log_info("Starting app {app_name} on port {app_port}", app_name = app_name, app_port = app_port)

  # Prepare log files
  output_log <- file.path(config$log_dir, paste0(app_name, "_output.log"))
  error_log <- file.path(config$log_dir, paste0(app_name, "_error.log"))

  # Start the app process
  process <- r_bg(
    function(app_path, port, output_log, error_log) {
      # Redirect output to log files
      output_con <- file(output_log, open = "w", encoding = "UTF-8")
      error_con <- file(error_log, open = "w", encoding = "UTF-8")

      # Ensure file handles are closed on exit
      on.exit({
        tryCatch(
          {
            sink(type = "output") # Reset output sink to default
            sink(type = "message") # Reset message sink to default
            if (exists("output_con")) close(output_con)
            if (exists("error_con")) close(error_con)
          },
          error = function(e) {
            # Log cleanup errors but don't fail
            cat("Warning: Error during file handle cleanup:", e$message, "\n")
          }
        )
      })

      sink(output_con, type = "output")
      sink(error_con, type = "message")

      # Start the Shiny app
      shiny::runApp(
        appDir = app_path,
        port = port,
        host = "127.0.0.1",
        launch.browser = FALSE
      )
    },
    args = list(
      app_path = app_path,
      port = app_port,
      output_log = output_log,
      error_log = error_log
    )
  )

  app_processes[[app_name]] <<- process

  # Wait for app to start
  Sys.sleep(3)

  if (process$is_alive()) {
    log_info("Successfully started app {app_name}", app_name = app_name)
    return(TRUE)
  } else {
    log_error("Failed to start app {app_name}", app_name = app_name)
    return(FALSE)
  }
}

# Health check for apps
health_check <- function() {
  for (app_config in config$apps) {
    app_name <- app_config$name

    if (app_name %in% names(app_processes)) {
      process <- app_processes[[app_name]]

      if (!process$is_alive()) {
        log_error("App {app_name} died, restarting", app_name = app_name)

        # Clean up WebSocket connections for this app
        cleanup_app_connections(app_name)

        # Restart the app
        Sys.sleep(config$restart_delay %||% 5)
        start_app(app_config)
      }
    }
  }
}

# Clean up WebSocket connections for an app
cleanup_app_connections <- function(app_name) {
  # Find and close connections for this app
  for (session_id in names(backend_connections)) {
    conn_info <- backend_connections[[session_id]]
    if (conn_info$app_name == app_name) {
      if (!is.null(conn_info$ws)) {
        conn_info$ws$close()
      }
      backend_connections[[session_id]] <<- NULL
    }
  }
}

# Clean up stale connections based on timeout
cleanup_stale_connections <- function() {
  current_time <- Sys.time()
  timeout_threshold <- current_time - (CONNECTION_TIMEOUT_MINUTES * 60)

  connections_cleaned <- 0

  # Clean up stale backend connections
  for (session_id in names(backend_connections)) {
    conn_info <- backend_connections[[session_id]]

    # Check if connection has timestamp and is stale
    if (!is.null(conn_info$last_activity) && conn_info$last_activity < timeout_threshold) {
      if (!is.null(conn_info$ws)) {
        tryCatch(conn_info$ws$close(), error = function(e) {})
      }
      backend_connections[[session_id]] <<- NULL
      connections_cleaned <- connections_cleaned + 1
    }
  }

  # Clean up stale client connections
  for (session_id in names(ws_connections)) {
    conn_info <- ws_connections[[session_id]]

    # Check if connection has timestamp and is stale
    if (!is.null(conn_info$last_activity) && conn_info$last_activity < timeout_threshold) {
      ws_connections[[session_id]] <<- NULL
      connections_cleaned <- connections_cleaned + 1
    }
  }

  if (connections_cleaned > 0) {
    log_info("Cleaned up {connections_cleaned} stale connections", connections_cleaned = connections_cleaned)
  }

  # Log current connection counts for monitoring
  backend_count <- length(backend_connections)
  ws_count <- length(ws_connections)
  log_debug("Active connections - Backend: {backend_count}, WebSocket: {ws_count}",
    backend_count = backend_count, ws_count = ws_count
  )

  return(connections_cleaned)
}

# Clean up dead processes from tracking
cleanup_dead_processes <- function() {
  processes_cleaned <- 0

  for (app_name in names(app_processes)) {
    process <- app_processes[[app_name]]
    if (!is.null(process) && !process$is_alive()) {
      app_processes[[app_name]] <<- NULL
      processes_cleaned <- processes_cleaned + 1
      log_info("Removed dead process for app {app_name}", app_name = app_name)
    }
  }

  if (processes_cleaned > 0) {
    log_info("Cleaned up {processes_cleaned} dead processes", processes_cleaned = processes_cleaned)
  }

  return(processes_cleaned)
}

# Get app configuration by name
get_app_config <- function(app_name) {
  for (app_config in config$apps) {
    if (app_config$name == app_name) {
      return(app_config)
    }
  }
  return(NULL)
}

# Generate session ID
generate_session_id <- function(req) {
  user_agent <- if (is.null(req$HTTP_USER_AGENT)) "" else req$HTTP_USER_AGENT
  remote_addr <- if (is.null(req$REMOTE_ADDR)) "" else req$REMOTE_ADDR
  timestamp <- as.character(Sys.time())

  # Add random entropy for better security
  random_salt <- paste(sample(c(0:9, letters, LETTERS), 16, replace = TRUE), collapse = "")

  # Create session ID with SHA-256 (more secure than MD5)
  session_data <- paste(user_agent, remote_addr, timestamp, random_salt, sep = "|")
  session_id <- digest::digest(session_data, algo = "sha256")

  return(session_id)
}

# Create WebSocket connection to backend Shiny app
create_backend_connection <- function(app_name, session_id, client_ws) {
  app_config <- get_app_config(app_name)
  if (is.null(app_config)) {
    log_error("App not found: {app_name}", app_name = app_name)
    return(NULL)
  }

  backend_url <- paste0("ws://127.0.0.1:", app_config$port, "/websocket/")
  log_info("Connecting to backend: {backend_url} for app {app_name}",
    backend_url = backend_url, app_name = app_name
  )

  # Create WebSocket connection to backend
  backend_ws <- WebSocket$new(backend_url)

  # Store connection info with ready state and timestamp
  backend_connections[[session_id]] <<- list(
    app_name = app_name,
    ws = backend_ws,
    client_ws = client_ws,
    ready = FALSE,
    pending_messages = list(),
    last_activity = Sys.time(),
    created_at = Sys.time()
  )

  # Handle messages from backend to client
  backend_ws$onMessage(function(event) {
    log_debug("Backend->Client message for {app_name}: {data}",
      app_name = app_name, data = substring(event$data, 1, 100)
    )
    client_ws$send(event$data)
  })

  # Handle backend connection events
  backend_ws$onOpen(function(event) {
    log_info("Backend connection opened for {app_name}", app_name = app_name)

    # Mark connection as ready
    if (session_id %in% names(backend_connections)) {
      backend_connections[[session_id]]$ready <<- TRUE

      # Send any pending messages
      pending <- backend_connections[[session_id]]$pending_messages
      if (length(pending) > 0) {
        log_info("Sending {count} pending messages for app {app_name}",
          count = length(pending), app_name = app_name
        )
        for (msg in pending) {
          backend_ws$send(msg)
        }
        backend_connections[[session_id]]$pending_messages <<- list()
      }
    }
  })

  backend_ws$onClose(function(event) {
    log_info("Backend connection closed for app {app_name}", app_name = app_name)
    backend_connections[[session_id]] <<- NULL
  })

  backend_ws$onError(function(event) {
    log_error("Backend connection error for app {app_name}: {error}",
      app_name = app_name, error = event$message
    )
  })

  return(backend_ws)
}

# Generate landing page HTML
generate_landing_page <- function() {
  app_cards <- ""

  for (app_config in config$apps) {
    app_cards <- paste0(app_cards, sprintf('
    <div class="app-card">
      <h3>%s</h3>
      <div class="app-links">
        <a href="/proxy/%s/" class="proxy-link">Open App</a>
      </div>
    </div>', app_config$name, app_config$name))
  }

  html <- sprintf('
<!DOCTYPE html>
<html>
<head>
  <title>Tiny Shiny Server</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <style>
    :root {
      --bg-color: #ffffff;
      --text-color: #333333;
      --card-bg: #f8f9fa;
      --border-color: #dee2e6;
      --link-color: #007bff;
      --link-hover: #0056b3;
    }

    @media (prefers-color-scheme: dark) {
      :root {
        --bg-color: #1a1a1a;
        --text-color: #e0e0e0;
        --card-bg: #2d2d2d;
        --border-color: #404040;
        --link-color: #4dabf7;
        --link-hover: #339af0;
      }
    }

    * {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }

    body {
      font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
      background-color: var(--bg-color);
      color: var(--text-color);
      line-height: 1.6;
      padding: 20px;
      transition: background-color 0.3s, color 0.3s;
    }

    .container {
      max-width: 1200px;
      margin: 0 auto;
    }

    .header {
      text-align: center;
      margin-bottom: 40px;
    }

    .header h1 {
      font-size: 2.5rem;
      margin-bottom: 10px;
      background: linear-gradient(135deg, var(--link-color), var(--link-hover));
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      background-clip: text;
    }

    .header p {
      font-size: 1.1rem;
      opacity: 0.8;
    }

    .apps-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
      gap: 20px;
      margin-bottom: 40px;
    }

    .app-card {
      background: var(--card-bg);
      border: 1px solid var(--border-color);
      border-radius: 12px;
      padding: 24px;
      transition: transform 0.2s, box-shadow 0.2s;
    }

    .app-card:hover {
      transform: translateY(-2px);
      box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
    }

    .app-card h3 {
      font-size: 1.4rem;
      margin-bottom: 16px;
      text-transform: uppercase;
    }

    .app-links {
      display: flex;
      justify-content: center;
    }

    .app-links a {
      padding: 12px 24px;
      border-radius: 6px;
      text-decoration: none;
      font-weight: 500;
      transition: all 0.2s;
      text-align: center;
      min-width: 150px;
    }

    .proxy-link {
      background-color: var(--link-color);
      color: white;
    }

    .proxy-link:hover {
      background-color: var(--link-hover);
    }

    .info {
      background: var(--card-bg);
      border: 1px solid var(--border-color);
      border-radius: 8px;
      padding: 20px;
      margin-top: 20px;
    }

    .info h4 {
      margin-bottom: 10px;
      color: var(--link-color);
    }

    .info p {
      margin-bottom: 8px;
      opacity: 0.9;
    }
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>Tiny Shiny Server</h1>
      <p>WebSocket-enabled proxy server for Shiny applications</p>
    </div>

    <div class="apps-grid">
      %s
    </div>

  </div>
</body>
</html>', app_cards)

  return(html)
}

# HTTP request handler
handle_http_request <- function(req) {
  # Validate path
  path_validation <- validate_path(req$PATH_INFO)
  if (!path_validation$valid) {
    log_error("Invalid path: {error}", error = path_validation$error)
    return(list(
      status = 400L,
      headers = list("Content-Type" = "application/json"),
      body = paste0('{"error": "400 - Bad Request: ', path_validation$error, '"}')
    ))
  }
  path <- path_validation$sanitized

  # Validate HTTP method
  method_validation <- validate_http_method(req$REQUEST_METHOD)
  if (!method_validation$valid) {
    log_error("Invalid method: {error}", error = method_validation$error)
    return(list(
      status = 405L,
      headers = list("Content-Type" = "application/json"),
      body = paste0('{"error": "405 - Method Not Allowed: ', method_validation$error, '"}')
    ))
  }
  method <- method_validation$sanitized

  # Validate query string
  query_validation <- validate_query_string(req$QUERY_STRING)
  if (!query_validation$valid) {
    log_error("Invalid query string: {error}", error = query_validation$error)
    return(list(
      status = 400L,
      headers = list("Content-Type" = "application/json"),
      body = paste0('{"error": "400 - Bad Request: ', query_validation$error, '"}')
    ))
  }
  query_string <- query_validation$sanitized

  log_debug("HTTP request: {method} {path}", method = method, path = path)

  # Landing page
  if (path == "/" || path == "") {
    return(list(
      status = 200L,
      headers = list("Content-Type" = "text/html"),
      body = generate_landing_page()
    ))
  }

  # Health check endpoint
  if (path == "/health") {
    return(list(
      status = 200L,
      headers = list("Content-Type" = "application/json"),
      body = '{"status": "healthy"}'
    ))
  }

  # Proxy requests to apps
  if (startsWith(path, "/proxy/")) {
    path_parts <- strsplit(path, "/")[[1]]
    path_parts <- path_parts[path_parts != ""] # Remove empty parts

    if (length(path_parts) >= 2) {
      # Validate app name
      app_name_validation <- validate_app_name(path_parts[2])
      if (!app_name_validation$valid) {
        log_error("Invalid app name: {error}", error = app_name_validation$error)
        return(list(
          status = 400L,
          headers = list("Content-Type" = "application/json"),
          body = paste0('{"error": "400 - Bad Request: ', app_name_validation$error, '"}')
        ))
      }
      app_name <- app_name_validation$sanitized
      app_config <- get_app_config(app_name)

      if (!is.null(app_config)) {
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

        # Forward the request with timeout for better responsiveness
        tryCatch(
          {
            log_info("Forwarding {method} to {target_url} for app {app_name}",
              method = method, target_url = target_url, app_name = app_name
            )

            # Make the request with timeout to prevent blocking
            if (method == "GET") {
              response <- GET(target_url, timeout(30)) # 30 second timeout
            } else if (method == "POST") {
              # Handle POST data
              body <- req$rook.input$read_lines()
              response <- POST(target_url, body = body, timeout(30))
            } else {
              # Handle other methods
              response <- VERB(method, target_url, timeout(30))
            }

            # Get response headers safely
            response_headers <- response$headers
            content_type <- if (!is.null(response_headers) && "content-type" %in% names(response_headers)) {
              response_headers[["content-type"]]
            } else {
              "text/html"
            }

            # Handle binary vs text content
            raw_content <- content(response, "raw")

            # Check if content is binary based on content type or presence of null bytes
            is_binary <- grepl("image/|font/|application/octet-stream|application/pdf", content_type, ignore.case = TRUE) ||
              any(raw_content == 0)

            # Return response with appropriate body format
            if (is_binary) {
              return(list(
                status = status_code(response),
                headers = list("Content-Type" = content_type),
                body = raw_content
              ))
            } else {
              return(list(
                status = status_code(response),
                headers = list("Content-Type" = content_type),
                body = rawToChar(raw_content)
              ))
            }
          },
          error = function(e) {
            log_error("Proxy error for app {app_name}: {error}",
              app_name = app_name, error = e$message
            )
            return(list(
              status = 502L,
              headers = list("Content-Type" = "application/json"),
              body = paste0('{"error": "502 - Bad Gateway: ', e$message, '"}')
            ))
          }
        )
      }
    }
  }

  # 404 for unknown paths
  return(list(
    status = 404L,
    headers = list("Content-Type" = "application/json"),
    body = '{"error": "404 - Not Found"}'
  ))
}

# WebSocket handler
handle_websocket <- function(ws) {
  log_info("WebSocket connection opened")

  # Generate session ID
  session_id <- generate_session_id(ws$request)

  # Determine which app this WebSocket is for based on the request path
  request_path_validation <- validate_path(ws$request$PATH_INFO)
  if (!request_path_validation$valid) {
    log_error("Invalid WebSocket path: {error}", error = request_path_validation$error)
    ws$close()
    return()
  }
  request_path <- request_path_validation$sanitized
  app_name <- NULL

  if (startsWith(request_path, "/proxy/")) {
    path_parts <- strsplit(request_path, "/")[[1]]
    path_parts <- path_parts[path_parts != ""]
    if (length(path_parts) >= 2) {
      # Validate app name for WebSocket routing
      app_name_validation <- validate_app_name(path_parts[2])
      if (!app_name_validation$valid) {
        log_error("Invalid WebSocket app name: {error}", error = app_name_validation$error)
        ws$close()
        return()
      }
      app_name <- app_name_validation$sanitized
    }
  }

  # Default to first app if no specific routing
  if (is.null(app_name) && length(config$apps) > 0) {
    app_name <- config$apps[[1]]$name
  }

  log_info("WebSocket routed to app: {app_name}", app_name = app_name)

  # Store client WebSocket with app info and timestamp
  ws_connections[[session_id]] <<- list(
    ws = ws,
    app_name = app_name,
    last_activity = Sys.time(),
    created_at = Sys.time()
  )

  ws$onMessage(function(binary, message) {
    # Validate WebSocket message
    message_validation <- validate_ws_message(message)
    if (!message_validation$valid) {
      log_error("Invalid WebSocket message: {error}", error = message_validation$error)
      # Send error back to client and close connection
      ws$send(paste0('{"error": "Invalid message: ', message_validation$error, '"}'))
      ws$close()
      return()
    }
    validated_message <- message_validation$sanitized

    log_debug("Client message: {message}", message = substring(validated_message, 1, 100))

    # Update last activity timestamp
    if (session_id %in% names(ws_connections)) {
      ws_connections[[session_id]]$last_activity <<- Sys.time()
    }

    tryCatch(
      {
        if (!is.null(app_name)) {
          # Get or create backend connection
          if (!session_id %in% names(backend_connections)) {
            create_backend_connection(app_name, session_id, ws)
          }

          # Forward message to backend
          backend_conn <- backend_connections[[session_id]]
          if (!is.null(backend_conn) && !is.null(backend_conn$ws)) {
            if (backend_conn$ready) {
              backend_conn$ws$send(validated_message)
              # Update last activity timestamp
              backend_connections[[session_id]]$last_activity <<- Sys.time()
            } else {
              # Queue message for when connection is ready (with size limit)
              current_pending <- backend_connections[[session_id]]$pending_messages
              if (length(current_pending) >= MAX_PENDING_MESSAGES) {
                # Drop oldest messages to make room
                current_pending <- tail(current_pending, MAX_PENDING_MESSAGES - 1)
                log_warn("Pending queue full, dropped oldest messages for app {app_name}", app_name = app_name)
              }
              backend_connections[[session_id]]$pending_messages <<-
                append(current_pending, validated_message)
              log_debug("Queued message for pending connection ({count}/{max}) for app {app_name}",
                count = length(current_pending) + 1, max = MAX_PENDING_MESSAGES, app_name = app_name
              )
            }
          }
        }
      },
      error = function(e) {
        log_error("WebSocket message error: {error}", error = e$message)
      }
    )
  })

  ws$onClose(function() {
    log_info("WebSocket connection closed")

    # Clean up connections
    if (session_id %in% names(ws_connections)) {
      ws_connections[[session_id]] <<- NULL
    }

    if (session_id %in% names(backend_connections)) {
      backend_conn <- backend_connections[[session_id]]
      if (!is.null(backend_conn) && !is.null(backend_conn$ws)) {
        backend_conn$ws$close()
      }
      backend_connections[[session_id]] <<- NULL
    }
  })
}

# Start the proxy server
start_proxy_server <- function() {
  proxy_port <- config$proxy_port %||% 3838
  proxy_host <- config$proxy_host %||% "127.0.0.1"

  # Convert localhost to 127.0.0.1 for httpuv compatibility
  if (proxy_host == "localhost") {
    proxy_host <- "127.0.0.1"
  }

  log_info("Starting WebSocket-enabled proxy server on {proxy_host}:{proxy_port}",
    proxy_host = proxy_host, proxy_port = proxy_port
  )

  # Start the httpuv server
  server <- startServer(
    host = proxy_host,
    port = proxy_port,
    app = list(
      call = handle_http_request,
      onWSOpen = handle_websocket
    )
  )

  return(server)
}

# Cleanup function to terminate all processes
cleanup_and_exit <- function() {
  log_info("Shutting down server...")

  # Close all WebSocket connections
  for (session_id in names(backend_connections)) {
    backend_conn <- backend_connections[[session_id]]
    if (!is.null(backend_conn) && !is.null(backend_conn$ws)) {
      tryCatch(backend_conn$ws$close(), error = function(e) {})
    }
  }
  backend_connections <<- list()
  ws_connections <<- list()

  # Terminate all app processes
  for (app_name in names(app_processes)) {
    process <- app_processes[[app_name]]
    if (!is.null(process) && process$is_alive()) {
      log_info("Terminating app: {app_name}", app_name = app_name)
      tryCatch(
        {
          process$kill()
          # Wait a moment for graceful shutdown
          Sys.sleep(1)
          # Force kill if still alive
          if (process$is_alive()) {
            process$kill_tree()
          }
        },
        error = function(e) {
          log_error("Error terminating {app_name}: {error}", app_name = app_name, error = e$message)
        }
      )
    }
  }

  log_info("All processes terminated. Exiting.")
  quit(status = 0)
}

# Main function
main <- function() {
  # Load configuration
  load_config()

  # Initialize proper logging
  setup_logging()

  log_info("Starting WebSocket Shiny Server process manager")
  log_info("Press Ctrl-C to shutdown gracefully")

  # Start all apps sequentially to ensure proper process tracking
  # Note: Parallel startup was causing issues with global process tracking
  log_info("Starting apps...")
  for (app_config in config$apps) {
    start_app(app_config)
  }

  # Start health monitoring
  log_info("Starting health monitor")

  # Define a recursive health check function
  schedule_health_check <- function() {
    health_check()
    # Schedule the next health check
    later::later(schedule_health_check, config$health_check_interval %||% 10)
  }

  # Start the health check after initial delay
  later::later(schedule_health_check, 5)

  # Start memory management and cleanup monitoring with async operations
  log_info("Starting memory management monitor")

  # Define async cleanup function
  async_cleanup <- function() {
    # Run cleanup operations in parallel to avoid blocking
    cleanup_future <- future::future(
      {
        list(
          stale_cleanup = cleanup_stale_connections(),
          process_cleanup = cleanup_dead_processes()
        )
      },
      seed = NULL
    )

    # Schedule next cleanup cycle
    later::later(async_cleanup, CLEANUP_INTERVAL_SECONDS)
  }

  # Start first cleanup cycle
  later::later(async_cleanup, CLEANUP_INTERVAL_SECONDS)

  # Start proxy server
  proxy_server <- start_proxy_server()

  # Set up future plan for async operations
  future::plan(future::multisession, workers = 4)

  # Keep the server running with event-driven approach
  tryCatch({
    # Use httpuv's service function which is more efficient than busy waiting
    # Run later tasks and then block until events occur
    while (TRUE) {
      # Process any pending later tasks
      later::run_now()

      # Service httpuv events with timeout to allow periodic cleanup
      # This is more efficient than constant polling
      httpuv::service(0.5) # 500ms timeout instead of 100ms sleep
    }
  }, interrupt = function(e) {
    # Handle Ctrl-C interrupt
    cleanup_and_exit()
  }, error = function(e) {
    log_error("Server error: {error}", error = e$message)
    cleanup_and_exit()
  }, finally = {
    # Always cleanup on exit
    cleanup_and_exit()
  })
}

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Run if called directly
if (sys.nframe() == 0) {
  main()
}
