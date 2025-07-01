# Configuration Management Module
# Centralizes configuration loading, validation, and state management

library(jsonlite)

# Configuration class to encapsulate global state
ShinyServerConfig <- setRefClass("ShinyServerConfig",
  fields = list(
    # Configuration data
    config = "list",

    # Runtime state
    app_processes = "list",
    ws_connections = "list",
    backend_connections = "list",
    management_server = "ANY",

    # Constants
    MAX_PENDING_MESSAGES = "numeric",
    CONNECTION_TIMEOUT_MINUTES = "numeric",
    CLEANUP_INTERVAL_SECONDS = "numeric",
    MAX_PATH_LENGTH = "numeric",
    MAX_QUERY_LENGTH = "numeric",
    MAX_MESSAGE_SIZE = "numeric",
    ALLOWED_HTTP_METHODS = "character"
  ),
  methods = list(
    initialize = function() {
      # Initialize constants
      MAX_PENDING_MESSAGES <<- 100
      CONNECTION_TIMEOUT_MINUTES <<- 30
      CLEANUP_INTERVAL_SECONDS <<- 300
      MAX_PATH_LENGTH <<- 1000
      MAX_QUERY_LENGTH <<- 2048
      MAX_MESSAGE_SIZE <<- 1048576
      ALLOWED_HTTP_METHODS <<- c("GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS")

      # Initialize runtime state
      app_processes <<- list()
      ws_connections <<- list()
      backend_connections <<- list()
      management_server <<- NULL

      # Load configuration
      load_config()
    },
    load_config = function(config_file = "config.json") {
      "Load and validate configuration from file"

      if (!file.exists(config_file)) {
        stop("Configuration file not found: ", config_file)
      }

      # Read with explicit UTF-8 encoding
      config_text <- readLines(config_file, encoding = "UTF-8", warn = FALSE)
      parsed_config <- fromJSON(paste(config_text, collapse = "\n"), simplifyDataFrame = FALSE)

      # Validate configuration
      validation_result <- validate_config(parsed_config)
      if (!validation_result$valid) {
        stop("Configuration validation failed: ", validation_result$error)
      }

      config <<- validation_result$sanitized

      # Set defaults for optional fields
      config$proxy_port <<- config$proxy_port %||% 3838
      config$proxy_host <<- config$proxy_host %||% "127.0.0.1"
      config$management_port <<- config$management_port %||% 3839
      config$restart_delay <<- config$restart_delay %||% 5
      config$health_check_interval <<- config$health_check_interval %||% 10
      config$starting_port <<- config$starting_port %||% 3001

      # Auto-assign ports to apps
      assign_app_ports()

      return(config)
    },
    validate_config = function(config) {
      "Validate configuration structure and values"

      if (!is.list(config)) {
        return(list(valid = FALSE, error = "Config must be a list"))
      }

      # Validate required fields
      required_fields <- c("apps", "log_dir", "starting_port")
      for (field in required_fields) {
        if (!field %in% names(config)) {
          return(list(valid = FALSE, error = paste("Missing required field:", field)))
        }
      }

      # Validate starting_port (now required)
      if (!is.numeric(config$starting_port) || length(config$starting_port) != 1 ||
        config$starting_port < 1 || config$starting_port > 65535) {
        return(list(valid = FALSE, error = "Invalid starting_port: must be a number between 1 and 65535"))
      }

      # Check if there's enough port range for all apps
      num_apps <- length(config$apps)
      reserved_ports <- c()
      if ("proxy_port" %in% names(config)) reserved_ports <- c(reserved_ports, config$proxy_port)
      if ("management_port" %in% names(config)) reserved_ports <- c(reserved_ports, config$management_port)

      # Calculate maximum port that might be needed
      max_possible_port <- config$starting_port + num_apps - 1 + length(reserved_ports)
      if (max_possible_port > 65535) {
        return(list(valid = FALSE, error = sprintf(
          "Not enough port range: starting_port %d with %d apps could exceed port 65535",
          config$starting_port, num_apps
        )))
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

        # Check required app fields (port is no longer required)
        app_required <- c("name", "path")
        for (field in app_required) {
          if (!field %in% names(app)) {
            return(list(valid = FALSE, error = paste("App", i, "missing field:", field)))
          }
        }

        # Validate app name
        if (!is.character(app$name) || length(app$name) != 1) {
          return(list(valid = FALSE, error = paste("App", i, "name must be a string")))
        }

        if (!grepl("^[a-zA-Z0-9_-]+$", app$name)) {
          return(list(valid = FALSE, error = paste("App", i, "name contains invalid characters")))
        }

        if (nchar(app$name) > 50) {
          return(list(valid = FALSE, error = paste("App", i, "name too long")))
        }

        # Validate path
        if (!is.character(app$path) || length(app$path) != 1) {
          return(list(valid = FALSE, error = paste("App", i, "path must be a string")))
        }
      }

      # Validate optional fields
      if ("proxy_port" %in% names(config)) {
        if (!is.numeric(config$proxy_port) || length(config$proxy_port) != 1 ||
          config$proxy_port < 1 || config$proxy_port > 65535) {
          return(list(valid = FALSE, error = "Invalid proxy_port"))
        }
      }

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
    },
    get_app_config = function(app_name) {
      "Get configuration for a specific app by name"

      for (app_config in config$apps) {
        if (app_config$name == app_name) {
          return(app_config)
        }
      }
      return(NULL)
    },
    get_proxy_host = function() {
      "Get proxy host, converting localhost to 127.0.0.1 for httpuv compatibility"

      host <- config$proxy_host %||% "127.0.0.1"
      if (host == "localhost") {
        host <- "127.0.0.1"
      }
      return(host)
    },

    # State management methods
    add_app_process = function(app_name, process) {
      "Add a process to the tracking list"
      app_processes[[app_name]] <<- process
    },
    remove_app_process = function(app_name) {
      "Remove a process from tracking"
      app_processes[[app_name]] <<- NULL
    },
    get_app_process = function(app_name) {
      "Get a tracked process by app name"
      return(app_processes[[app_name]])
    },
    add_ws_connection = function(session_id, connection_info) {
      "Add a WebSocket connection to tracking"
      ws_connections[[session_id]] <<- connection_info
    },
    remove_ws_connection = function(session_id) {
      "Remove a WebSocket connection from tracking"
      ws_connections[[session_id]] <<- NULL
    },
    get_ws_connection = function(session_id) {
      "Get WebSocket connection info by session ID"
      return(ws_connections[[session_id]])
    },
    add_backend_connection = function(session_id, connection_info) {
      "Add a backend connection to tracking"
      backend_connections[[session_id]] <<- connection_info
    },
    remove_backend_connection = function(session_id) {
      "Remove a backend connection from tracking"
      backend_connections[[session_id]] <<- NULL
    },
    get_backend_connection = function(session_id) {
      "Get backend connection info by session ID"
      return(backend_connections[[session_id]])
    },
    get_all_ws_connections = function() {
      "Get all WebSocket connections"
      return(ws_connections)
    },
    get_all_backend_connections = function() {
      "Get all backend connections"
      return(backend_connections)
    },
    get_all_app_processes = function() {
      "Get all app processes"
      return(app_processes)
    },
    assign_app_ports = function() {
      "Auto-assign ports to apps starting from starting_port"

      starting_port <- config$starting_port
      # Create a set of reserved ports for O(1) lookup
      reserved_ports <- c(config$proxy_port, config$management_port)
      reserved_set <- new.env(hash = TRUE, parent = emptyenv())
      for (port in reserved_ports) {
        if (!is.null(port)) {
          assign(as.character(port), TRUE, envir = reserved_set)
        }
      }

      current_port <- starting_port

      for (i in seq_along(config$apps)) {
        # Skip reserved ports efficiently
        while (exists(as.character(current_port), envir = reserved_set)) {
          current_port <- current_port + 1

          # Safety check to prevent infinite loop
          if (current_port > 65535) {
            stop(sprintf(
              "Port assignment failed: exceeded maximum port 65535 while assigning port for app '%s'",
              config$apps[[i]]$name
            ))
          }
        }

        # Assign port to app
        config$apps[[i]]$port <<- current_port

        # Move to next port
        current_port <- current_port + 1
      }

      # Validate no port conflicts
      validate_port_assignments()
    },
    validate_port_assignments = function() {
      "Validate that all assigned ports are unique and don't conflict"

      all_ports <- c()

      # Collect all app ports
      for (app in config$apps) {
        if (!is.null(app$port)) {
          all_ports <- c(all_ports, app$port)
        }
      }

      # Add system ports
      all_ports <- c(all_ports, config$proxy_port, config$management_port)

      # Check for duplicates
      if (length(all_ports) != length(unique(all_ports))) {
        duplicates <- all_ports[duplicated(all_ports)]

        # Find which apps/services are using duplicate ports
        port_usage <- list()
        for (app in config$apps) {
          if (app$port %in% duplicates) {
            port_usage[[as.character(app$port)]] <- c(port_usage[[as.character(app$port)]], paste("app", app$name))
          }
        }
        if (config$proxy_port %in% duplicates) {
          port_usage[[as.character(config$proxy_port)]] <- c(port_usage[[as.character(config$proxy_port)]], "proxy")
        }
        if (config$management_port %in% duplicates) {
          port_usage[[as.character(config$management_port)]] <- c(port_usage[[as.character(config$management_port)]], "management")
        }

        conflict_details <- sapply(names(port_usage), function(port) {
          sprintf("port %s used by: %s", port, paste(port_usage[[port]], collapse = " and "))
        })

        stop(sprintf(
          "Port conflict detected! %s",
          paste(conflict_details, collapse = "; ")
        ))
      }

      # Log port assignments
      log_info("Port assignments:")
      for (app in config$apps) {
        log_info("  App '{name}' -> port {port}", name = app$name, port = app$port)
      }
      log_info("  Proxy -> port {port}", port = config$proxy_port)
      log_info("  Management -> port {port}", port = config$management_port)
    }
  )
)

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Create global configuration instance
# This will be used by other modules
create_server_config <- function() {
  return(ShinyServerConfig$new())
}
