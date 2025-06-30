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
management_server <- NULL

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

# Helper functions for connection tracking
get_client_ip <- function(req) {
  # Try to get real IP from headers (for reverse proxy setups)
  forwarded_for <- req$HTTP_X_FORWARDED_FOR
  if (!is.null(forwarded_for) && forwarded_for != "") {
    # Take the first IP if multiple are present
    ip <- strsplit(forwarded_for, ",")[[1]][1]
    return(trimws(ip))
  }

  # Try other common headers
  real_ip <- req$HTTP_X_REAL_IP
  if (!is.null(real_ip) && real_ip != "") {
    return(trimws(real_ip))
  }

  # Fall back to REMOTE_ADDR
  remote_addr <- req$REMOTE_ADDR
  if (!is.null(remote_addr) && remote_addr != "") {
    return(remote_addr)
  }

  return("unknown")
}

get_dns_name <- function(ip) {
  # Skip DNS resolution for localhost and unknown IPs
  if (ip %in% c("127.0.0.1", "::1", "localhost", "unknown")) {
    return(ip)
  }

  # Try to resolve DNS name with timeout
  tryCatch(
    {
      # Use system nslookup command for reverse DNS with timeout
      result <- system2("timeout", c("2", "nslookup", ip),
        stdout = TRUE, stderr = TRUE, timeout = 3
      )

      # Parse nslookup output for name
      if (length(result) > 0 && !is.null(attr(result, "status")) && attr(result, "status") == 0) {
        name_lines <- grep("name =", result, value = TRUE)
        if (length(name_lines) > 0) {
          name <- gsub(".*name = (.+)\\.", "\\1", name_lines[1])
          if (name != ip && nchar(name) > 0) {
            return(name)
          }
        }
      }

      return(ip)
    },
    error = function(e) {
      return(ip)
    }
  )
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

  # Determine app type - prioritize traditional Shiny apps over Rmd
  has_app_r <- file.exists(file.path(app_path, "app.R"))
  has_ui_server <- file.exists(file.path(app_path, "ui.R")) && file.exists(file.path(app_path, "server.R"))
  has_traditional_shiny <- has_app_r || has_ui_server

  rmd_files <- list.files(app_path, pattern = "\\.Rmd$", full.names = TRUE)
  is_rmd_app <- !has_traditional_shiny && length(rmd_files) > 0

  if (is_rmd_app) {
    log_info("Detected R Markdown app for {app_name}, using rmarkdown::run", app_name = app_name)
  } else if (has_traditional_shiny) {
    log_info("Detected traditional Shiny app for {app_name}", app_name = app_name)
  }

  # Prepare log files
  output_log <- file.path(config$log_dir, paste0(app_name, "_output.log"))
  error_log <- file.path(config$log_dir, paste0(app_name, "_error.log"))

  # Start the app process
  process <- r_bg(
    function(app_path, port, output_log, error_log, is_rmd_app, rmd_files) {
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

      if (is_rmd_app && length(rmd_files) > 0) {
        # Load rmarkdown library with error handling
        tryCatch(
          {
            library(rmarkdown)
          },
          error = function(e) {
            stop("rmarkdown package required. Install with: install.packages('rmarkdown')")
          }
        )

        # Use the first .Rmd file found (use absolute path for security)
        rmd_file <- rmd_files[1]

        # Validate that the Rmd file exists and is readable
        if (!file.exists(rmd_file)) {
          stop("R Markdown file not found: ", rmd_file)
        }

        # Start the R Markdown app using rmarkdown::run
        rmarkdown::run(
          file = rmd_file,
          shiny_args = list(
            port = port,
            host = "127.0.0.1",
            launch.browser = FALSE
          )
        )
      } else {
        # Start the regular Shiny app
        shiny::runApp(
          appDir = app_path,
          port = port,
          host = "127.0.0.1",
          launch.browser = FALSE
        )
      }
    },
    args = list(
      app_path = app_path,
      port = app_port,
      output_log = output_log,
      error_log = error_log,
      is_rmd_app = is_rmd_app,
      rmd_files = rmd_files
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
  # Capture session information (first 3 lines only)
  session_info_output <- capture.output(sessionInfo())
  session_info_text <- paste(head(session_info_output, 3), collapse = "\n")
  # Escape HTML characters
  session_info_text <- gsub("&", "&amp;", session_info_text)
  session_info_text <- gsub("<", "&lt;", session_info_text)
  session_info_text <- gsub(">", "&gt;", session_info_text)

  app_cards <- ""

  for (app_config in config$apps) {
    # HTML escape app name for security
    safe_name <- gsub("&", "&amp;", app_config$name)
    safe_name <- gsub("<", "&lt;", safe_name)
    safe_name <- gsub(">", "&gt;", safe_name)
    safe_name <- gsub("\"", "&quot;", safe_name)
    safe_name <- gsub("'", "&#39;", safe_name)

    app_cards <- paste0(app_cards, sprintf('
    <div class="app-card" data-app="%1$s">
      <h3>%1$s</h3>
      <div class="app-status">
        <span class="status-badge" id="status-%1$s">Loading...</span>
        <span class="connections-count" id="connections-%1$s">0 connections</span>
      </div>
      <div class="app-links">
        <a href="/proxy/%1$s/" class="proxy-link">Open App</a>
      </div>
    </div>', safe_name))
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

    .app-status {
      display: flex;
      justify-content: space-between;
      align-items: center;
      margin-bottom: 16px;
      gap: 10px;
    }

    .status-badge {
      padding: 4px 12px;
      border-radius: 20px;
      font-size: 12px;
      font-weight: bold;
      text-transform: uppercase;
    }

    .status-running {
      background-color: #d4edda;
      color: #155724;
    }

    .status-stopped {
      background-color: #f8d7da;
      color: #721c24;
    }

    .status-crashed {
      background-color: #fff3cd;
      color: #856404;
    }

    @media (prefers-color-scheme: dark) {
      .status-running {
        background-color: #1e3a2e;
        color: #4ade80;
      }

      .status-stopped {
        background-color: #3a1e1e;
        color: #f87171;
      }

      .status-crashed {
        background-color: #3a2e1e;
        color: #fbbf24;
      }
    }

    .connections-count {
      font-size: 12px;
      color: var(--muted-text);
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

    .session-info {
      background: var(--card-bg);
      border: 1px solid var(--border-color);
      border-radius: 8px;
      padding: 20px;
      margin-top: 20px;
    }

    .session-info h4 {
      margin-bottom: 10px;
      color: var(--link-color);
    }

    .session-info pre {
      background: var(--bg-color);
      border: 1px solid var(--border-color);
      border-radius: 4px;
      padding: 15px;
      margin: 0;
      font-family: "Courier New", "Monaco", "Menlo", monospace;
      font-size: 0.85rem;
      line-height: 1.4;
      overflow-x: auto;
      white-space: pre-wrap;
      color: var(--text-color);
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

    <div class="session-info">
      <h4>Server Information</h4>
      <pre>%s</pre>
    </div>
  </div>

  <script>
    let refreshInterval;

    function updateAppStatus() {
      fetch(\"/api/apps\")
        .then(response => response.json())
        .then(data => {
          Object.values(data).forEach(app => {
            const statusElement = document.getElementById(\"status-\" + app.name);
            const connectionsElement = document.getElementById(\"connections-\" + app.name);

            if (statusElement) {
              statusElement.textContent = app.status;
              statusElement.className = \"status-badge status-\" + app.status;
            }

            if (connectionsElement) {
              const connectionText = app.connections === 1 ? \"1 connection\" : app.connections + \" connections\";
              connectionsElement.textContent = connectionText;
            }
          });
        })
        .catch(error => {
          console.error(\"Error fetching app status:\", error);
          // Update all status badges to show error state
          document.querySelectorAll(\".status-badge\").forEach(badge => {
            badge.textContent = \"error\";
            badge.className = \"status-badge status-stopped\";
          });
        });
    }

    // Initial load
    updateAppStatus();

    // Auto-refresh every 5 seconds
    refreshInterval = setInterval(updateAppStatus, 5000);

    // Clean up interval when page is about to unload
    window.addEventListener(\"beforeunload\", function() {
      if (refreshInterval) {
        clearInterval(refreshInterval);
      }
    });
  </script>
</body>
</html>', app_cards, session_info_text)

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

  # API endpoint for app status (needed for landing page)
  if (path == "/api/apps") {
    return(list(
      status = 200L,
      headers = list("Content-Type" = "application/json"),
      body = get_apps_status_json()
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

  # Get client connection info
  client_ip <- get_client_ip(ws$request)
  client_dns <- get_dns_name(client_ip)
  user_agent <- ws$request$HTTP_USER_AGENT %||% "unknown"

  # Store client WebSocket with app info and timestamp
  ws_connections[[session_id]] <<- list(
    ws = ws,
    app_name = app_name,
    client_ip = client_ip,
    client_dns = client_dns,
    user_agent = user_agent,
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

# Management server functions
handle_management_request <- function(req) {
  method <- req$REQUEST_METHOD
  path <- req$PATH_INFO %||% "/"
  query_string <- req$QUERY_STRING %||% ""

  # Validate inputs
  method_validation <- validate_http_method(method)
  if (!method_validation$valid) {
    return(list(
      status = 400L,
      headers = list("Content-Type" = "application/json"),
      body = paste0('{"error": "400 - Bad Request: ', method_validation$error, '"}')
    ))
  }
  method <- method_validation$sanitized

  path_validation <- validate_path(path)
  if (!path_validation$valid) {
    return(list(
      status = 400L,
      headers = list("Content-Type" = "application/json"),
      body = paste0('{"error": "400 - Bad Request: ', path_validation$error, '"}')
    ))
  }
  path <- path_validation$sanitized

  log_info("Management {method} {path}", method = method, path = path)

  # Management dashboard
  if (path == "/" && method == "GET") {
    return(list(
      status = 200L,
      headers = list("Content-Type" = "text/html"),
      body = generate_management_html()
    ))
  }

  # API endpoints
  if (path == "/api/apps" && method == "GET") {
    return(list(
      status = 200L,
      headers = list("Content-Type" = "application/json"),
      body = get_apps_status_json()
    ))
  }

  if (path == "/api/connections" && method == "GET") {
    return(list(
      status = 200L,
      headers = list("Content-Type" = "application/json"),
      body = get_connections_json()
    ))
  }

  if (path == "/api/status" && method == "GET") {
    return(list(
      status = 200L,
      headers = list("Content-Type" = "application/json"),
      body = get_system_status_json()
    ))
  }

  # App restart endpoint
  if (startsWith(path, "/api/apps/") && endsWith(path, "/restart") && method == "POST") {
    path_parts <- strsplit(path, "/")[[1]]
    path_parts <- path_parts[path_parts != ""]

    if (length(path_parts) >= 3) {
      app_name_validation <- validate_app_name(path_parts[3])
      if (!app_name_validation$valid) {
        return(list(
          status = 400L,
          headers = list("Content-Type" = "application/json"),
          body = paste0('{"error": "400 - Bad Request: ', app_name_validation$error, '"}')
        ))
      }
      app_name <- app_name_validation$sanitized

      result <- restart_app(app_name)
      if (result$success) {
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = toJSON(result, auto_unbox = TRUE)
        ))
      } else {
        return(list(
          status = 500L,
          headers = list("Content-Type" = "application/json"),
          body = toJSON(result, auto_unbox = TRUE)
        ))
      }
    }
  }

  # Shutdown endpoint
  if (path == "/api/shutdown" && method == "POST") {
    log_info("Shutdown requested via management API")

    # Create shutdown flag file
    shutdown_flag_file <- file.path(config$log_dir, "shutdown.flag")
    tryCatch(
      {
        writeLines("shutdown", shutdown_flag_file)
        return(list(
          status = 200L,
          headers = list("Content-Type" = "application/json"),
          body = '{"success": true, "message": "Shutdown initiated"}'
        ))
      },
      error = function(e) {
        return(list(
          status = 500L,
          headers = list("Content-Type" = "application/json"),
          body = paste0('{"success": false, "error": "', e$message, '"}')
        ))
      }
    )
  }

  # 404 for unknown paths
  return(list(
    status = 404L,
    headers = list("Content-Type" = "application/json"),
    body = '{"error": "404 - Not Found"}'
  ))
}

get_apps_status_json <- function() {
  apps_status <- list()

  for (app_config in config$apps) {
    app_name <- app_config$name
    process <- app_processes[[app_name]]

    status <- if (is.null(process)) {
      "stopped"
    } else if (process$is_alive()) {
      "running"
    } else {
      "crashed"
    }

    # Count connections for this app
    app_connections <- 0
    for (conn in ws_connections) {
      if (!is.null(conn$app_name) && conn$app_name == app_name) {
        app_connections <- app_connections + 1
      }
    }

    apps_status[[app_name]] <- list(
      name = app_name,
      status = status,
      port = app_config$port,
      path = app_config$path,
      connections = app_connections,
      pid = if (!is.null(process) && process$is_alive()) process$get_pid() else NULL
    )
  }

  return(toJSON(apps_status, auto_unbox = TRUE))
}

get_connections_json <- function() {
  connections <- list()

  for (session_id in names(ws_connections)) {
    conn <- ws_connections[[session_id]]
    if (!is.null(conn)) {
      connections[[session_id]] <- list(
        session_id = session_id,
        app_name = conn$app_name %||% "unknown",
        client_ip = conn$client_ip %||% "unknown",
        client_dns = conn$client_dns %||% "unknown",
        user_agent = conn$user_agent %||% "unknown",
        connected_at = format(conn$created_at, "%Y-%m-%d %H:%M:%S"),
        last_activity = format(conn$last_activity, "%Y-%m-%d %H:%M:%S"),
        duration_seconds = as.numeric(difftime(Sys.time(), conn$created_at, units = "secs"))
      )
    }
  }

  return(toJSON(connections, auto_unbox = TRUE))
}

get_system_status_json <- function() {
  total_connections <- length(ws_connections)
  running_apps <- 0

  for (app_name in names(app_processes)) {
    process <- app_processes[[app_name]]
    if (!is.null(process) && process$is_alive()) {
      running_apps <- running_apps + 1
    }
  }

  status <- list(
    total_apps = length(config$apps),
    running_apps = running_apps,
    total_connections = total_connections,
    server_uptime = "N/A", # Could be enhanced with actual uptime tracking
    memory_usage = "N/A" # Could be enhanced with memory monitoring
  )

  return(toJSON(status, auto_unbox = TRUE))
}

restart_app <- function(app_name) {
  app_config <- get_app_config(app_name)
  if (is.null(app_config)) {
    return(list(success = FALSE, message = "App not found"))
  }

  log_info("Restarting app: {app_name}", app_name = app_name)

  tryCatch(
    {
      # Close existing connections for this app
      sessions_to_remove <- c()
      for (session_id in names(ws_connections)) {
        conn <- ws_connections[[session_id]]
        if (!is.null(conn) && !is.null(conn$app_name) && conn$app_name == app_name) {
          if (!is.null(conn$ws)) {
            tryCatch(conn$ws$close(), error = function(e) {})
          }
          sessions_to_remove <- c(sessions_to_remove, session_id)
        }
      }

      # Remove closed connections
      for (session_id in sessions_to_remove) {
        ws_connections[[session_id]] <<- NULL
        if (session_id %in% names(backend_connections)) {
          backend_connections[[session_id]] <<- NULL
        }
      }

      # Stop existing process
      process <- app_processes[[app_name]]
      if (!is.null(process) && process$is_alive()) {
        process$kill()
        Sys.sleep(1)
        if (process$is_alive()) {
          process$kill_tree()
        }
      }

      # Start new process
      start_app(app_config)

      return(list(success = TRUE, message = paste("App", app_name, "restarted successfully")))
    },
    error = function(e) {
      log_error("Failed to restart app {app_name}: {error}", app_name = app_name, error = e$message)
      return(list(success = FALSE, message = paste("Failed to restart app:", e$message)))
    }
  )
}

start_management_server <- function() {
  management_port <- config$management_port %||% 3839

  log_info("Starting management server on localhost:{management_port}", management_port = management_port)

  # Start the management server (always on localhost for security)
  server <- startServer(
    host = "127.0.0.1",
    port = management_port,
    app = list(
      call = handle_management_request
    )
  )

  return(server)
}

generate_management_html <- function() {
  html <- '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Shiny Server Management</title>
    <style>
        :root {
            --bg-color: #ffffff;
            --text-color: #333333;
            --card-bg: #f8f9fa;
            --border-color: #dee2e6;
            --link-color: #007bff;
            --link-hover: #0056b3;
            --surface-color: #f5f5f5;
            --success-bg: #d4edda;
            --success-text: #155724;
            --warning-bg: #fff3cd;
            --warning-text: #856404;
            --error-bg: #f8d7da;
            --error-text: #721c24;
            --muted-text: #6c757d;
            --table-header-bg: #f8f9fa;
        }

        @media (prefers-color-scheme: dark) {
            :root {
                --bg-color: #1a1a1a;
                --text-color: #e0e0e0;
                --card-bg: #2d2d2d;
                --border-color: #404040;
                --link-color: #4dabf7;
                --link-hover: #339af0;
                --surface-color: #121212;
                --success-bg: #1e3a2e;
                --success-text: #4ade80;
                --warning-bg: #3a2e1e;
                --warning-text: #fbbf24;
                --error-bg: #3a1e1e;
                --error-text: #f87171;
                --muted-text: #9ca3af;
                --table-header-bg: #374151;
            }
        }

        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
            background-color: var(--surface-color);
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
            background: var(--card-bg);
            border: 1px solid var(--border-color);
            padding: 20px;
            border-radius: 12px;
            margin-bottom: 20px;
            transition: all 0.3s;
        }
        .header-content {
            display: flex;
            justify-content: space-between;
            align-items: center;
            flex-wrap: wrap;
            gap: 15px;
        }
        .header h1 {
            background: linear-gradient(135deg, var(--link-color), var(--link-hover));
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
            margin: 0;
        }
        .header p {
            margin: 5px 0 0 0;
            opacity: 0.8;
        }
        .header-actions {
            display: flex;
            gap: 10px;
        }
        .shutdown-btn {
            background-color: var(--error-bg);
            color: var(--error-text);
            border: 1px solid var(--border-color);
            padding: 10px 20px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 14px;
            font-weight: 600;
            transition: all 0.2s;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }
        .shutdown-btn:hover {
            background-color: #dc3545;
            color: white;
            transform: translateY(-1px);
            box-shadow: 0 4px 8px rgba(220, 53, 69, 0.3);
        }
        .shutdown-btn:disabled {
            background-color: var(--muted-text);
            color: var(--text-color);
            cursor: not-allowed;
            opacity: 0.6;
            transform: none;
            box-shadow: none;
        }
        .section {
            background: var(--card-bg);
            border: 1px solid var(--border-color);
            padding: 20px;
            border-radius: 12px;
            margin-bottom: 20px;
            transition: all 0.3s;
        }
        .status-grid {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 15px;
            margin-bottom: 20px;
        }
        .status-card {
            background: var(--bg-color);
            border: 1px solid var(--border-color);
            padding: 15px;
            border-radius: 8px;
            border-left: 4px solid var(--link-color);
            transition: all 0.3s;
        }
        .status-card h3 {
            margin: 0 0 10px 0;
            font-size: 16px;
            color: var(--text-color);
            opacity: 0.8;
        }
        .status-card .value {
            font-size: 24px;
            font-weight: bold;
            color: var(--link-color);
        }
        .app-card {
            border: 1px solid var(--border-color);
            border-radius: 8px;
            padding: 15px;
            margin-bottom: 15px;
            background: var(--bg-color);
            transition: all 0.3s;
        }
        .app-card:hover {
            transform: translateY(-2px);
            box-shadow: 0 4px 20px rgba(0, 0, 0, 0.1);
        }
        .app-header {
            display: flex;
            justify-content: space-between;
            align-items: center;
            margin-bottom: 10px;
        }
        .app-name {
            font-size: 18px;
            font-weight: bold;
            color: var(--text-color);
        }
        .status-badge {
            padding: 4px 12px;
            border-radius: 20px;
            font-size: 12px;
            font-weight: bold;
            text-transform: uppercase;
        }
        .status-running {
            background-color: var(--success-bg);
            color: var(--success-text);
        }
        .status-stopped {
            background-color: var(--error-bg);
            color: var(--error-text);
        }
        .status-crashed {
            background-color: var(--warning-bg);
            color: var(--warning-text);
        }
        .app-details {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
            gap: 10px;
            margin-bottom: 15px;
            font-size: 14px;
        }
        .app-detail {
            color: var(--muted-text);
        }
        .restart-btn {
            background-color: var(--warning-bg);
            color: var(--warning-text);
            border: 1px solid var(--border-color);
            padding: 8px 16px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
            transition: all 0.2s;
        }
        .restart-btn:hover {
            background-color: var(--link-color);
            color: white;
            transform: translateY(-1px);
        }
        .restart-btn:disabled {
            background-color: var(--muted-text);
            color: var(--text-color);
            cursor: not-allowed;
            opacity: 0.6;
        }
        .connections-table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 15px;
        }
        .connections-table th,
        .connections-table td {
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid var(--border-color);
        }
        .connections-table th {
            background-color: var(--table-header-bg);
            font-weight: 600;
            color: var(--text-color);
        }
        .connections-table td {
            color: var(--text-color);
        }
        .connections-table .user-agent {
            max-width: 200px;
            overflow: hidden;
            text-overflow: ellipsis;
            white-space: nowrap;
            font-family: "Courier New", monospace;
            font-size: 12px;
            cursor: help;
        }
        .connections-table .user-agent:hover {
            white-space: normal;
            word-break: break-all;
            max-width: none;
            background-color: var(--card-bg);
            position: relative;
            z-index: 1;
            box-shadow: 0 2px 8px rgba(0,0,0,0.2);
            padding: 8px;
            border-radius: 4px;
        }
        .loading {
            text-align: center;
            padding: 20px;
            color: var(--muted-text);
        }
        .auto-refresh {
            font-size: 12px;
            color: var(--muted-text);
            text-align: right;
            margin-top: 10px;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <div class="header-content">
                <div>
                    <h1>Shiny Server Management</h1>
                    <p>Monitor and manage your Shiny applications</p>
                </div>
                <div class="header-actions">
                    <button class="shutdown-btn" onclick="shutdownServer()">
                        Shutdown Server
                    </button>
                </div>
            </div>
        </div>

        <div class="section">
            <h2>System Overview</h2>
            <div class="status-grid" id="systemStatus">
                <div class="loading">Loading system status...</div>
            </div>
        </div>

        <div class="section">
            <h2>Applications</h2>
            <div id="appsContainer">
                <div class="loading">Loading applications...</div>
            </div>
        </div>

        <div class="section">
            <h2>Active Connections</h2>
            <div id="connectionsContainer">
                <div class="loading">Loading connections...</div>
            </div>
            <div class="auto-refresh">Auto-refreshing every 5 seconds</div>
        </div>
    </div>

    <script>
        let refreshInterval;

        function formatDuration(seconds) {
            const hours = Math.floor(seconds / 3600);
            const minutes = Math.floor((seconds % 3600) / 60);
            const secs = Math.floor(seconds % 60);

            if (hours > 0) {
                return `${hours}h ${minutes}m ${secs}s`;
            } else if (minutes > 0) {
                return `${minutes}m ${secs}s`;
            } else {
                return `${secs}s`;
            }
        }

        function updateSystemStatus() {
            fetch("/api/status")
                .then(response => response.json())
                .then(data => {
                    const container = document.getElementById("systemStatus");
                    container.innerHTML =
                        "<div class=\\"status-card\\">" +
                            "<h3>Total Apps</h3>" +
                            "<div class=\\"value\\">" + data.total_apps + "</div>" +
                        "</div>" +
                        "<div class=\\"status-card\\">" +
                            "<h3>Running Apps</h3>" +
                            "<div class=\\"value\\">" + data.running_apps + "</div>" +
                        "</div>" +
                        "<div class=\\"status-card\\">" +
                            "<h3>Active Connections</h3>" +
                            "<div class=\\"value\\">" + data.total_connections + "</div>" +
                        "</div>";
                })
                .catch(error => {
                    console.error("Error fetching system status:", error);
                });
        }

        function updateApps() {
            fetch("/api/apps")
                .then(response => response.json())
                .then(data => {
                    const container = document.getElementById("appsContainer");
                    container.innerHTML = "";

                    Object.values(data).forEach(app => {
                        const appDiv = document.createElement("div");
                        appDiv.className = "app-card";
                        appDiv.innerHTML =
                            "<div class=\\"app-header\\">" +
                                "<div class=\\"app-name\\">" + app.name + "</div>" +
                                "<span class=\\"status-badge status-" + app.status + "\\">" + app.status + "</span>" +
                            "</div>" +
                            "<div class=\\"app-details\\">" +
                                "<div class=\\"app-detail\\"><strong>Port:</strong> " + app.port + "</div>" +
                                "<div class=\\"app-detail\\"><strong>Connections:</strong> " + app.connections + "</div>" +
                                "<div class=\\"app-detail\\"><strong>Path:</strong> " + app.path + "</div>" +
                                "<div class=\\"app-detail\\"><strong>PID:</strong> " + (app.pid || "N/A") + "</div>" +
                            "</div>" +
                            "<button class=\\"restart-btn\\" onclick=\\"restartApp(\'" + app.name + "\')\\">Restart Application</button>";
                        container.appendChild(appDiv);
                    });
                })
                .catch(error => {
                    console.error("Error fetching apps:", error);
                });
        }

        function updateConnections() {
            fetch("/api/connections")
                .then(response => response.json())
                .then(data => {
                    const container = document.getElementById("connectionsContainer");

                    if (Object.keys(data).length === 0) {
                        container.innerHTML = "<p>No active connections</p>";
                        return;
                    }

                    let tableHTML =
                        "<table class=\\"connections-table\\">" +
                            "<thead>" +
                                "<tr>" +
                                    "<th>App</th>" +
                                    "<th>Client IP</th>" +
                                    "<th>DNS Name</th>" +
                                    "<th>User Agent</th>" +
                                    "<th>Connected</th>" +
                                    "<th>Duration</th>" +
                                    "<th>Last Activity</th>" +
                                "</tr>" +
                            "</thead>" +
                            "<tbody>";

                    Object.values(data).forEach(conn => {
                        tableHTML +=
                            "<tr>" +
                                "<td>" + conn.app_name + "</td>" +
                                "<td>" + conn.client_ip + "</td>" +
                                "<td>" + conn.client_dns + "</td>" +
                                "<td class=\\"user-agent\\">" + conn.user_agent + "</td>" +
                                "<td>" + conn.connected_at + "</td>" +
                                "<td>" + formatDuration(conn.duration_seconds) + "</td>" +
                                "<td>" + conn.last_activity + "</td>" +
                            "</tr>";
                    });

                    tableHTML += "</tbody></table>";
                    container.innerHTML = tableHTML;
                })
                .catch(error => {
                    console.error("Error fetching connections:", error);
                });
        }

        function restartApp(appName) {
            const button = event.target;
            button.disabled = true;
            button.textContent = "Restarting...";

            fetch("/api/apps/" + appName + "/restart", {
                method: "POST"
            })
            .then(response => response.json())
            .then(data => {
                if (data.success) {
                    alert("App " + appName + " restarted successfully");
                } else {
                    alert("Failed to restart " + appName + ": " + data.message);
                }
                button.disabled = false;
                button.textContent = "Restart Application";
                updateApps();
            })
            .catch(error => {
                console.error("Error restarting app:", error);
                alert("Error restarting " + appName + ": " + error.message);
                button.disabled = false;
                button.textContent = "Restart Application";
            });
        }

        function shutdownServer() {
            const confirmed = confirm(
                "Are you sure you want to shutdown the entire Shiny server? " +
                "This will stop all applications and the management interface. " +
                "You will need to restart the server manually to continue using it."
            );

            if (!confirmed) {
                return;
            }

            const button = document.querySelector(".shutdown-btn");
            button.disabled = true;
            button.textContent = "Shutting down...";

            if (refreshInterval) {
                clearInterval(refreshInterval);
            }

            fetch("/api/shutdown", {
                method: "POST"
            })
            .then(response => response.json())
            .then(data => {
                if (data.success) {
                    alert("Server shutdown initiated successfully. The page will become unresponsive as the server stops.");
                    document.body.innerHTML =
                        "<div style=\\"display: flex; justify-content: center; align-items: center; height: 100vh; text-align: center; background: var(--surface-color); color: var(--text-color);\\">" +
                            "<div>" +
                                "<h1 style=\\"color: var(--error-text); margin-bottom: 20px;\\">Server Shutdown</h1>" +
                                "<p style=\\"font-size: 18px; margin-bottom: 10px;\\">The Shiny server has been shut down successfully.</p>" +
                                "<p style=\\"color: var(--muted-text);\\">To restart the server, run the startup script from the command line.</p>" +
                            "</div>" +
                        "</div>";
                } else {
                    alert("Failed to shutdown server: " + data.message);
                    button.disabled = false;
                    button.textContent = "Shutdown Server";
                }
            })
            .catch(error => {
                console.error("Error shutting down server:", error);
                if (error.message.includes("fetch")) {
                    document.body.innerHTML =
                        "<div style=\\"display: flex; justify-content: center; align-items: center; height: 100vh; text-align: center; background: var(--surface-color); color: var(--text-color);\\">" +
                            "<div>" +
                                "<h1 style=\\"color: var(--error-text); margin-bottom: 20px;\\">Server Shutdown</h1>" +
                                "<p style=\\"font-size: 18px; margin-bottom: 10px;\\">The Shiny server has been shut down.</p>" +
                                "<p style=\\"color: var(--muted-text);\\">To restart the server, run the startup script from the command line.</p>" +
                            "</div>" +
                        "</div>";
                } else {
                    alert("Error shutting down server: " + error.message);
                    button.disabled = false;
                    button.textContent = "Shutdown Server";
                }
            });
        }

        function refreshAll() {
            updateSystemStatus();
            updateApps();
            updateConnections();
        }

        // Initial load
        refreshAll();

        // Auto-refresh every 5 seconds
        refreshInterval = setInterval(refreshAll, 5000);
    </script>
</body>
</html>'

  return(html)
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

  # Stop management server
  if (!is.null(management_server)) {
    tryCatch(
      {
        stopServer(management_server)
        management_server <<- NULL
        log_info("Management server stopped")
      },
      error = function(e) {
        log_error("Error stopping management server: {error}", error = e$message)
      }
    )
  }

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

  # Create a shutdown flag file to monitor for external shutdown requests
  shutdown_flag_file <- file.path(config$log_dir, "shutdown.flag")
  if (file.exists(shutdown_flag_file)) {
    file.remove(shutdown_flag_file)
  }

  # Add cleanup to remove shutdown flag
  on.exit(
    {
      if (file.exists(shutdown_flag_file)) {
        file.remove(shutdown_flag_file)
      }
    },
    add = TRUE
  )

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

  # Start management server
  management_server <<- start_management_server()

  # Set up future plan for async operations
  future::plan(future::multisession, workers = 4)

  # Keep the server running with event-driven approach
  tryCatch({
    # Use httpuv's service function which is more efficient than busy waiting
    # Run later tasks and then block until events occur
    while (TRUE) {
      # Check for shutdown flag file
      if (file.exists(shutdown_flag_file)) {
        log_info("Shutdown flag detected, initiating graceful shutdown")
        break
      }

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
