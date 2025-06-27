#!/usr/bin/env Rscript

# WebSocket-enabled Shiny Proxy Server
# This replaces the plumber-based proxy with full WebSocket support

library(httpuv)
library(websocket)
library(jsonlite)
library(callr)
library(later)
library(httr)

# Global variables
config <- NULL
app_processes <- list()
ws_connections <- list()
backend_connections <- list()

# Load configuration
load_config <- function() {
  if (file.exists("config.json")) {
    config <<- fromJSON("config.json", simplifyDataFrame = FALSE)
  } else {
    stop("config.json not found")
  }
}

# Logging function
log_message <- function(message, level = "INFO", app_name = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (!is.null(app_name)) {
    log_text <- sprintf("[%s] %s [%s]: %s", timestamp, level, app_name, message)
  } else {
    log_text <- sprintf("[%s] %s: %s", timestamp, level, message)
  }

  cat(log_text, "\n")

  # Write to log file
  log_file <- "logs/websocket_proxy.log"
  if (!dir.exists("logs")) dir.create("logs")
  write(log_text, file = log_file, append = TRUE)
}

# Start Shiny app process
start_app <- function(app_config) {
  app_name <- app_config$name
  app_port <- app_config$port
  app_path <- app_config$path

  log_message(paste("Starting app on port", app_port), app_name = app_name)

  # Prepare log files
  output_log <- paste0("logs/", app_name, "_output.log")
  error_log <- paste0("logs/", app_name, "_error.log")

  # Start the app process
  process <- r_bg(
    function(app_path, port, output_log, error_log) {
      # Redirect output to log files
      output_con <- file(output_log, open = "w")
      error_con <- file(error_log, open = "w")
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
    log_message("Successfully started", app_name = app_name)
    return(TRUE)
  } else {
    log_message("Failed to start", "ERROR", app_name = app_name)
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
        log_message("App died, restarting", "ERROR", app_name = app_name)

        # Clean up WebSocket connections for this app
        cleanup_app_connections(app_name)

        # Restart the app
        Sys.sleep(config$restart_delay)
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

  # Create session ID
  session_data <- paste(user_agent, remote_addr, timestamp, sep = "|")
  session_id <- digest::digest(session_data, algo = "md5")

  return(session_id)
}

# Create WebSocket connection to backend Shiny app
create_backend_connection <- function(app_name, session_id, client_ws) {
  app_config <- get_app_config(app_name)
  if (is.null(app_config)) {
    log_message(paste("App not found:", app_name), "ERROR")
    return(NULL)
  }

  backend_url <- paste0("ws://127.0.0.1:", app_config$port, "/websocket/")
  log_message(paste("Connecting to backend:", backend_url), app_name = app_name)

  # Create WebSocket connection to backend
  backend_ws <- WebSocket$new(backend_url)

  # Store connection info with ready state
  backend_connections[[session_id]] <<- list(
    app_name = app_name,
    ws = backend_ws,
    client_ws = client_ws,
    ready = FALSE,
    pending_messages = list()
  )

  # Handle messages from backend to client
  backend_ws$onMessage(function(event) {
    log_message(paste("Backend->Client:", substring(event$data, 1, 100)), app_name = app_name)
    client_ws$send(event$data)
  })

  # Handle backend connection events
  backend_ws$onOpen(function(event) {
    log_message("Backend connection opened", app_name = app_name)

    # Mark connection as ready
    if (session_id %in% names(backend_connections)) {
      backend_connections[[session_id]]$ready <<- TRUE

      # Send any pending messages
      pending <- backend_connections[[session_id]]$pending_messages
      if (length(pending) > 0) {
        log_message(paste("Sending", length(pending), "pending messages"), app_name = app_name)
        for (msg in pending) {
          backend_ws$send(msg)
        }
        backend_connections[[session_id]]$pending_messages <<- list()
      }
    }
  })

  backend_ws$onClose(function(event) {
    log_message("Backend connection closed", app_name = app_name)
    backend_connections[[session_id]] <<- NULL
  })

  backend_ws$onError(function(event) {
    log_message(paste("Backend connection error:", event$message), "ERROR", app_name = app_name)
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
        <a href="/proxy/%s/" class="proxy-link">Open via Proxy</a>
        <a href="http://127.0.0.1:%d" class="direct-link" target="_blank">Open Direct</a>
      </div>
    </div>', app_config$name, app_config$name, app_config$port))
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
      text-transform: capitalize;
    }

    .app-links {
      display: flex;
      gap: 12px;
      flex-wrap: wrap;
    }

    .app-links a {
      padding: 8px 16px;
      border-radius: 6px;
      text-decoration: none;
      font-weight: 500;
      transition: all 0.2s;
      flex: 1;
      text-align: center;
      min-width: 120px;
    }

    .proxy-link {
      background-color: var(--link-color);
      color: white;
    }

    .proxy-link:hover {
      background-color: var(--link-hover);
    }

    .direct-link {
      background-color: transparent;
      color: var(--link-color);
      border: 1px solid var(--link-color);
    }

    .direct-link:hover {
      background-color: var(--link-color);
      color: white;
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

    <div class="info">
      <h4>Usage Information</h4>
      <p><strong>Proxy Links:</strong> Full WebSocket support with session management</p>
      <p><strong>Direct Links:</strong> Bypass proxy for debugging or direct access</p>
      <p><strong>Features:</strong> Automatic app restart, health monitoring, and session affinity</p>
    </div>
  </div>
</body>
</html>', app_cards)

  return(html)
}

# HTTP request handler
handle_http_request <- function(req) {
  path <- req$PATH_INFO
  log_message(paste("HTTP request:", path))

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
      app_name <- path_parts[2]
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
        if (!is.null(req$QUERY_STRING) && req$QUERY_STRING != "") {
          target_url <- paste0(target_url, "?", req$QUERY_STRING)
        }

        # Forward the request
        tryCatch(
          {
            method <- req$REQUEST_METHOD
            log_message(paste("Forwarding", method, "to", target_url), app_name = app_name)

            # Make the request (simplified without custom headers for now)
            if (method == "GET") {
              response <- GET(target_url)
            } else if (method == "POST") {
              # Handle POST data
              body <- req$rook.input$read_lines()
              response <- POST(target_url, body = body)
            } else {
              # Handle other methods
              response <- VERB(method, target_url)
            }

            # Get response headers safely
            response_headers <- response$headers
            content_type <- if (!is.null(response_headers) && "content-type" %in% names(response_headers)) {
              response_headers[["content-type"]]
            } else {
              "text/html"
            }

            # Return response
            return(list(
              status = status_code(response),
              headers = list("Content-Type" = content_type),
              body = rawToChar(content(response, "raw"))
            ))
          },
          error = function(e) {
            log_message(paste("Proxy error:", e$message), "ERROR", app_name = app_name)
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
  log_message("WebSocket connection opened")

  # Generate session ID
  session_id <- generate_session_id(ws$request)

  # Determine which app this WebSocket is for based on the request path
  request_path <- ws$request$PATH_INFO
  app_name <- NULL

  if (startsWith(request_path, "/proxy/")) {
    path_parts <- strsplit(request_path, "/")[[1]]
    path_parts <- path_parts[path_parts != ""]
    if (length(path_parts) >= 2) {
      app_name <- path_parts[2]
    }
  }

  # Default to first app if no specific routing
  if (is.null(app_name) && length(config$apps) > 0) {
    app_name <- config$apps[[1]]$name
  }

  log_message(paste("WebSocket routed to app:", app_name))

  # Store client WebSocket with app info
  ws_connections[[session_id]] <<- list(ws = ws, app_name = app_name)

  ws$onMessage(function(binary, message) {
    log_message(paste("Client message:", substring(message, 1, 100)))

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
              backend_conn$ws$send(message)
            } else {
              # Queue message for when connection is ready
              backend_connections[[session_id]]$pending_messages <<-
                append(backend_connections[[session_id]]$pending_messages, message)
              log_message("Queued message for pending connection", app_name = app_name)
            }
          }
        }
      },
      error = function(e) {
        log_message(paste("WebSocket message error:", e$message), "ERROR")
      }
    )
  })

  ws$onClose(function() {
    log_message("WebSocket connection closed")

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
  log_message("Starting WebSocket-enabled proxy server on port 3838")

  # Start the httpuv server
  server <- startServer(
    host = "127.0.0.1",
    port = 3838,
    app = list(
      call = handle_http_request,
      onWSOpen = handle_websocket
    )
  )

  return(server)
}

# Main function
main <- function() {
  # Load configuration
  load_config()

  log_message("Starting WebSocket Shiny Server process manager")

  # Start all apps
  for (app_config in config$apps) {
    start_app(app_config)
  }

  # Start health monitoring
  log_message("Starting health monitor")
  later::later(function() {
    health_check()
    later::later(function() {
      health_check()
    }, 10)
  }, 5)

  # Start proxy server
  proxy_server <- start_proxy_server()

  # Keep the server running
  while (TRUE) {
    later::run_now()
    Sys.sleep(0.1)
  }
}

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Run if called directly
if (sys.nframe() == 0) {
  main()
}
