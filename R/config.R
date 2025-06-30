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

      return(config)
    },
    validate_config = function(config) {
      "Validate configuration structure and values"

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
        if (!is.character(app$name) || length(app$name) != 1) {
          return(list(valid = FALSE, error = paste("App", i, "name must be a string")))
        }

        if (!grepl("^[a-zA-Z0-9_-]+$", app$name)) {
          return(list(valid = FALSE, error = paste("App", i, "name contains invalid characters")))
        }

        if (nchar(app$name) > 50) {
          return(list(valid = FALSE, error = paste("App", i, "name too long")))
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
