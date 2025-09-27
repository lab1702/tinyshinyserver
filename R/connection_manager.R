# Connection Manager Module
# Manages WebSocket connections and session handling

library(websocket)
library(logger)

# Connection Manager Class
ConnectionManager <- setRefClass("ConnectionManager",
  fields = list(
    config = "ANY",
    process_manager = "ANY"  # Reference to process manager for immediate stops
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
        log_debug("Backend->Client message for {app_name}: {data}",
          app_name = app_name, data = substring(event$data, 1, 100)
        )
        client_ws$send(event$data)
      })

      # Handle backend connection events
      backend_ws$onOpen(function(event) {
        log_info("Backend connection opened for {app_name}", app_name = app_name)

        # Mark connection as ready
        conn_info <- config$get_backend_connection(session_id)
        if (!is.null(conn_info)) {
          conn_info$ready <- TRUE
          config$add_backend_connection(session_id, conn_info)

          # Send any pending messages
          pending <- conn_info$pending_messages
          if (length(pending) > 0) {
            log_info("Sending {count} pending messages for app {app_name}",
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
        log_info("Backend connection closed for app {app_name}", app_name = app_name)
        config$remove_backend_connection(session_id)
      })

      backend_ws$onError(function(event) {
        log_error("Backend connection error for app {app_name}: {error}",
          app_name = app_name, error = event$message
        )
      })

      return(backend_ws)
    },
    handle_client_message = function(session_id, message, app_name) {
      "Handle a message from a WebSocket client"

      # Validate message
      message_validation <- validate_ws_message(message, config$MAX_MESSAGE_SIZE)
      if (!message_validation$valid) {
        log_error("Invalid WebSocket message: {error}", error = message_validation$error)
        return(FALSE)
      }
      validated_message <- message_validation$sanitized

      log_debug("Client message: {message}", message = substring(validated_message, 1, 100))

      # Update last activity timestamp
      conn_info <- config$get_ws_connection(session_id)
      if (!is.null(conn_info)) {
        conn_info$last_activity <- Sys.time()
        config$add_ws_connection(session_id, conn_info)
      }

      tryCatch(
        {
          if (!is.null(app_name)) {
            # Get or create backend connection
            backend_conn <- config$get_backend_connection(session_id)
            if (is.null(backend_conn)) {
              create_backend_connection(app_name, session_id, conn_info$ws)
              backend_conn <- config$get_backend_connection(session_id)
            }

            # Forward message to backend
            if (!is.null(backend_conn) && !is.null(backend_conn$ws)) {
              if (backend_conn$ready) {
                backend_conn$ws$send(validated_message)
                # Update last activity timestamp
                backend_conn$last_activity <- Sys.time()
                config$add_backend_connection(session_id, backend_conn)
              } else {
                # Queue message for when connection is ready (with size limit)
                current_pending <- backend_conn$pending_messages
                if (length(current_pending) >= config$MAX_PENDING_MESSAGES) {
                  # Drop oldest messages to make room
                  current_pending <- tail(current_pending, config$MAX_PENDING_MESSAGES - 1)
                  log_warn("Pending queue full, dropped oldest messages for app {app_name}", app_name = app_name)
                }
                backend_conn$pending_messages <- append(current_pending, validated_message)
                config$add_backend_connection(session_id, backend_conn)
                log_debug("Queued message for pending connection ({count}/{max}) for app {app_name}",
                  count = length(current_pending) + 1, max = config$MAX_PENDING_MESSAGES, app_name = app_name
                )
              }
            }
          }
          return(TRUE)
        },
        error = function(e) {
          log_error("WebSocket message error: {error}", error = e$message)
          return(FALSE)
        }
      )
    },
    add_client_connection = function(session_id, ws, app_name, client_ip, user_agent) {
      "Add a new client WebSocket connection"

      config$add_ws_connection(session_id, list(
        ws = ws,
        app_name = app_name,
        client_ip = client_ip,
        user_agent = user_agent,
        last_activity = Sys.time(),
        created_at = Sys.time()
      ))

      log_info("WebSocket connection added for app {app_name} from {client_ip}",
        app_name = app_name, client_ip = client_ip
      )
    },
    remove_client_connection = function(session_id) {
      "Remove a client WebSocket connection and cleanup"

      log_info("WebSocket connection closed for session {session_id}", session_id = session_id)

      # Get the app name before removing the connection
      conn_info <- config$get_ws_connection(session_id)
      app_name <- if (!is.null(conn_info)) conn_info$app_name else NULL

      # Clean up client connection
      config$remove_ws_connection(session_id)

      # Decrement connection count for this app
      if (!is.null(app_name)) {
        decrement_connection_count(app_name)
      }

      # Clean up backend connection
      backend_conn <- config$get_backend_connection(session_id)
      if (!is.null(backend_conn)) {
        if (!is.null(backend_conn$ws)) {
          tryCatch(backend_conn$ws$close(), error = function(e) {})
        }
        config$remove_backend_connection(session_id)
      }
    },
    get_connection_stats = function() {
      "Get connection statistics"

      ws_connections <- config$get_all_ws_connections()
      backend_connections <- config$get_all_backend_connections()

      # Count connections by app
      app_connections <- list()
      for (conn in ws_connections) {
        app_name <- conn$app_name %||% "unknown"
        if (is.null(app_connections[[app_name]])) {
          app_connections[[app_name]] <- 0
        }
        app_connections[[app_name]] <- app_connections[[app_name]] + 1
      }

      return(list(
        total_ws_connections = length(ws_connections),
        total_backend_connections = length(backend_connections),
        connections_by_app = app_connections
      ))
    },
    get_connections_info = function() {
      "Get detailed connection information for management interface"

      connections <- list()

      for (session_id in names(config$get_all_ws_connections())) {
        conn <- config$get_ws_connection(session_id)
        if (!is.null(conn)) {
          connections[[session_id]] <- list(
            session_id = session_id,
            app_name = conn$app_name %||% "unknown",
            client_ip = conn$client_ip %||% "unknown",
            user_agent = conn$user_agent %||% "unknown",
            connected_at = format(conn$created_at, "%Y-%m-%d %H:%M:%S"),
            last_activity = format(conn$last_activity, "%Y-%m-%d %H:%M:%S"),
            duration_seconds = as.numeric(difftime(Sys.time(), conn$created_at, units = "secs"))
          )
        }
      }

      return(connections)
    },
    decrement_connection_count = function(app_name) {
      "Check WebSocket connections and stop app immediately if needed"
      
      # Use actual WebSocket connection count instead of our custom counter
      # This ensures we only stop when there are no active WebSocket connections
      ws_count <- 0
      for (conn in config$get_all_ws_connections()) {
        if (!is.null(conn$app_name) && conn$app_name == app_name) {
          ws_count <- ws_count + 1
        }
      }
      
      log_debug("WebSocket connections for {app_name}: {count}", app_name = app_name, count = ws_count)
      
      # If no WebSocket connections remain and we have a process manager reference, immediately stop non-resident apps
      if (ws_count == 0 && !is.null(process_manager)) {
        app_config <- config$get_app_config(app_name)
        if (!is.null(app_config) && !app_config$resident) {
          log_info("No WebSocket connections remain for non-resident app {app_name}, stopping immediately", app_name = app_name)
          process_manager$stop_app_immediately(app_name)
        }
      }
    },
  )
)

# Create connection manager factory function
create_connection_manager <- function(server_config, process_manager = NULL) {
  return(ConnectionManager$new(server_config, process_manager))
}
