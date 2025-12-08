# Configuration Management Module
# Centralizes configuration loading, validation, and state management

# Configuration class to encapsulate global state
ShinyServerConfig <- setRefClass("ShinyServerConfig",
  fields = list(
    # Configuration data
    config = "list",

    # Runtime state
    app_processes = "list",
    ws_connections = "list",
    backend_connections = "list",
    # Connection count cache: Maintains O(1) lookup performance
    # Rationale: Direct iteration is O(n) - at 100 connections this is 90x slower
    # Complexity: Adds defensive checks to add/remove operations
    # Validation: validate_connection_count_consistency() detects/fixes corruption
    # Benchmark: tests/benchmark_connection_count.R shows performance gains
    app_connection_counts = "environment",
    app_startup_state = "environment", # Track app startup progress (starting/ready)
    management_server = "ANY",

    # Constants
    MAX_PENDING_MESSAGES = "numeric",
    CONNECTION_TIMEOUT_MINUTES = "numeric",
    CLEANUP_INTERVAL_SECONDS = "numeric",
    MAX_PATH_LENGTH = "numeric",
    MAX_QUERY_LENGTH = "numeric",
    MAX_MESSAGE_SIZE = "numeric",
    ALLOWED_HTTP_METHODS = "character",
    APP_STARTUP_TIMEOUT_SECONDS = "numeric"
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
      APP_STARTUP_TIMEOUT_SECONDS <<- 30

      # Initialize runtime state
      app_processes <<- list()
      ws_connections <<- list()
      backend_connections <<- list()
      app_connection_counts <<- new.env(hash = TRUE, parent = emptyenv())
      app_startup_state <<- new.env(hash = TRUE, parent = emptyenv())
      management_server <<- NULL

      # Initialize empty config (to be loaded via load_config)
      config <<- list()
    },
    load_config = function(config_file = "config.json") {
      "Load and validate configuration from file"

      # Resolve config file path (normalize for absolute paths)
      config_file <- normalizePath(config_file, mustWork = TRUE)

      if (!file.exists(config_file)) {
        stop("Configuration file not found: ", config_file)
      }

      # Read with explicit UTF-8 encoding
      config_text <- readLines(config_file, encoding = "UTF-8", warn = FALSE)
      parsed_config <- jsonlite::fromJSON(paste(config_text, collapse = "\n"), simplifyDataFrame = FALSE)

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

      # Set default values for optional app fields
      for (i in seq_along(config$apps)) {
        if (!"resident" %in% names(config$apps[[i]])) {
          config$apps[[i]]$resident <<- FALSE
        }
      }

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

        # Validate optional resident field
        if ("resident" %in% names(app)) {
          if (!is.logical(app$resident) || length(app$resident) != 1) {
            return(list(valid = FALSE, error = paste("App", i, "resident field must be a single logical value (TRUE/FALSE)")))
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
      "Add a WebSocket connection to tracking with cache management

      Defensive programming notes:
      - Checks if connection already exists to prevent double-counting
      - Uses max(0, ...) to prevent negative counts from corruption
      - Only increments cache on NEW connections, not updates
      - Logs cache operations for debugging race conditions
      "

      # Validate input
      if (is.null(session_id) || is.null(connection_info)) {
        logger::log_error("add_ws_connection called with NULL session_id or connection_info")
        return(FALSE)
      }

      new_app_name <- connection_info$app_name

      # Check if this is a new connection or an update (critical for cache consistency)
      is_new_connection <- !(session_id %in% names(ws_connections))

      if (is_new_connection) {
        # New connection: add to tracking and increment count
        ws_connections[[session_id]] <<- connection_info

        if (!is.null(new_app_name)) {
          # Defensive: max(0, ...) prevents negative counts
          current <- if (exists(new_app_name, envir = app_connection_counts)) {
            max(0, get(new_app_name, envir = app_connection_counts))
          } else {
            0
          }
          new_count <- current + 1
          assign(new_app_name, new_count, envir = app_connection_counts)
          logger::log_debug("Cache: Added NEW connection for {app_name}, session={session_id}, count: {old} -> {new}",
            app_name = new_app_name, session_id = session_id, old = current, new = new_count
          )
        }
      } else {
        # Existing connection: update info but DO NOT change count
        # This prevents cache inconsistencies from repeated updates
        logger::log_debug("Cache: Updated existing connection for session={session_id}, count unchanged",
          session_id = session_id
        )
        ws_connections[[session_id]] <<- connection_info
      }

      return(TRUE)
    },
    remove_ws_connection = function(session_id) {
      "Remove a WebSocket connection from tracking with idempotent cache management

      Defensive programming notes:
      - Idempotent: safe to call multiple times with same session_id
      - Prevents double-decrement by checking existence first
      - Uses max(0, ...) to ensure count never goes negative
      - Returns FALSE if connection doesn't exist (for caller awareness)
      "

      # Validate input
      if (is.null(session_id)) {
        logger::log_error("remove_ws_connection called with NULL session_id")
        return(FALSE)
      }

      # IDEMPOTENT: Check if connection actually exists before removing
      # Critical: prevents double-decrement in concurrent/callback scenarios
      if (!(session_id %in% names(ws_connections))) {
        logger::log_debug("Cache: Connection {session_id} already removed, idempotent return",
          session_id = session_id
        )
        return(FALSE)
      }

      # Get connection info before removing (needed for cache update)
      conn_info <- ws_connections[[session_id]]

      # Safety check for NULL connection info
      if (is.null(conn_info)) {
        logger::log_warn("Cache: Connection {session_id} has NULL info, removing entry only",
          session_id = session_id
        )
        ws_connections[[session_id]] <<- NULL
        return(FALSE)
      }

      app_name <- conn_info$app_name

      # Remove the connection from tracking first
      ws_connections[[session_id]] <<- NULL

      # Update connection count cache if we have a valid app_name
      if (!is.null(app_name) && app_name != "") {
        current <- if (exists(app_name, envir = app_connection_counts)) {
          get(app_name, envir = app_connection_counts)
        } else {
          logger::log_warn("Cache: No count found for {app_name}, initializing to 0",
            app_name = app_name
          )
          0
        }

        # Ensure we never go negative
        new_count <- max(0, current - 1)
        assign(app_name, new_count, envir = app_connection_counts)

        logger::log_debug("Cache: Removed connection for {app_name}, session={session_id}, count: {old} -> {new}",
          app_name = app_name, session_id = session_id, old = current, new = new_count
        )
      }

      return(TRUE)
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
    get_app_connection_count = function(app_name) {
      "Get cached connection count for an app (O(1) lookup)"
      if (exists(app_name, envir = app_connection_counts)) {
        return(get(app_name, envir = app_connection_counts))
      }
      return(0)
    },
    validate_connection_count_consistency = function(fix_errors = TRUE) {
      "Validate and optionally fix connection count cache consistency

      This function exists because:
      1. Cache bugs have occurred in the past (race conditions, double counting)
      2. Provides safety net for detecting corruption early
      3. Auto-fix capability prevents prolonged inconsistency
      4. Called periodically by cleanup scheduler in tss_main.R

      Performance: O(n) iteration - only runs periodically, not on hot path
      "

      # Count actual connections per app
      actual_counts <- list()
      for (session_id in names(ws_connections)) {
        conn <- ws_connections[[session_id]]
        if (!is.null(conn) && !is.null(conn$app_name)) {
          app_name <- conn$app_name
          actual_counts[[app_name]] <- (actual_counts[[app_name]] %||% 0) + 1
        }
      }

      # Compare with cache
      inconsistencies <- list()
      all_app_names <- unique(c(names(actual_counts), ls(envir = app_connection_counts)))

      for (app_name in all_app_names) {
        actual <- actual_counts[[app_name]] %||% 0
        cached <- if (exists(app_name, envir = app_connection_counts)) {
          get(app_name, envir = app_connection_counts)
        } else {
          0
        }

        if (actual != cached) {
          inconsistencies[[app_name]] <- list(
            cached = cached,
            actual = actual,
            difference = cached - actual
          )

          logger::log_error("Connection count INCONSISTENCY for {app_name}: cached={cached}, actual={actual}, diff={diff}",
            app_name = app_name, cached = cached, actual = actual, diff = cached - actual
          )

          # Fix if requested
          if (fix_errors) {
            assign(app_name, actual, envir = app_connection_counts)
            logger::log_info("Fixed connection count for {app_name}: {cached} -> {actual}",
              app_name = app_name, cached = cached, actual = actual
            )
          }
        }
      }

      # Return validation result
      is_consistent <- length(inconsistencies) == 0
      return(list(
        consistent = is_consistent,
        inconsistencies = inconsistencies,
        total_apps_checked = length(all_app_names)
      ))
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
      max_attempts <- 1000 # Prevent infinite loops when many ports are in use

      for (i in seq_along(config$apps)) {
        attempts <- 0
        port_found <- FALSE

        # Find next available port
        while (!port_found && attempts < max_attempts) {
          # Skip reserved ports efficiently
          if (exists(as.character(current_port), envir = reserved_set)) {
            current_port <- current_port + 1
            attempts <- attempts + 1
            next
          }

          # Check if port is actually available (not in use by other processes)
          # Note: is_port_available returns TRUE if port is in use, FALSE if available
          # This is backwards from what you'd expect!
          if (!is_port_available("127.0.0.1", current_port)) {
            # Port is available (not in use)
            port_found <- TRUE
          } else {
            # Port is in use, try next one
            logger::log_debug("Port {port} is in use by another process, trying next port",
              port = current_port
            )
            current_port <- current_port + 1
            attempts <- attempts + 1
          }

          # Safety check to prevent infinite loop
          if (current_port > 65535) {
            stop(sprintf(
              "Port assignment failed: exceeded maximum port 65535 while assigning port for app '%s'",
              config$apps[[i]]$name
            ))
          }
        }

        if (!port_found) {
          stop(sprintf(
            "Port assignment failed: could not find available port for app '%s' after %d attempts",
            config$apps[[i]]$name, max_attempts
          ))
        }

        # Assign port to app
        config$apps[[i]]$port <<- current_port
        logger::log_debug("Assigned port {port} to app '{app_name}'",
          port = current_port, app_name = config$apps[[i]]$name
        )

        # Move to next port for next app
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
      logger::log_info("Port assignments:")
      for (app in config$apps) {
        logger::log_info("  App '{name}' -> port {port}", name = app$name, port = app$port)
      }
      logger::log_info("  Proxy -> port {port}", port = config$proxy_port)
      logger::log_info("  Management -> port {port}", port = config$management_port)
    },

    # App startup state tracking methods
    set_app_starting = function(app_name) {
      "Mark an app as starting up"
      assign(app_name, list(
        state = "starting",
        started_at = Sys.time()
      ), envir = app_startup_state)
    },
    set_app_ready = function(app_name) {
      "Mark an app as ready to receive requests"
      if (exists(app_name, envir = app_startup_state)) {
        rm(list = app_name, envir = app_startup_state)
      }
    },
    get_app_startup_state = function(app_name) {
      "Get the startup state of an app (NULL if ready, list if starting)"
      if (exists(app_name, envir = app_startup_state)) {
        startup_info <- get(app_name, envir = app_startup_state)

        # Check if startup has timed out
        elapsed <- as.numeric(difftime(Sys.time(), startup_info$started_at,
                                       units = "secs"))
        if (elapsed > APP_STARTUP_TIMEOUT_SECONDS) {
          # Startup timed out, remove state
          rm(list = app_name, envir = app_startup_state)
          return(list(state = "timeout", elapsed = elapsed))
        }

        return(c(startup_info, list(elapsed = elapsed)))
      }
      return(NULL)
    },
    is_app_starting = function(app_name) {
      "Check if an app is currently starting up"
      startup_state <- get_app_startup_state(app_name)
      return(!is.null(startup_state) && startup_state$state == "starting")
    }
  )
)

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Create global configuration instance
# This will be used by other modules
create_server_config <- function(config_file = "config.json") {
  config_instance <- ShinyServerConfig$new()
  config_instance$load_config(config_file)
  return(config_instance)
}
