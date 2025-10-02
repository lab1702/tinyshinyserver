# Process Management Module
# Handles Shiny app process lifecycle, health monitoring, and cleanup

# Process Manager Class
ProcessManager <- setRefClass("ProcessManager",
  fields = list(
    config = "ANY"
  ),
  methods = list(
    initialize = function(server_config) {
      config <<- server_config
    },
    start_app = function(app_config) {
      "Start a Shiny application process"

      app_name <- app_config$name
      app_port <- app_config$port
      app_path <- normalizePath(app_config$path, mustWork = FALSE)

      logger::log_info("Starting app {app_name} on port {app_port}", app_name = app_name, app_port = app_port)

      # Determine app type - prioritize traditional Shiny apps over Rmd
      has_app_r <- file.exists(file.path(app_path, "app.R"))
      has_ui_server <- file.exists(file.path(app_path, "ui.R")) && file.exists(file.path(app_path, "server.R"))
      has_traditional_shiny <- has_app_r || has_ui_server

      rmd_files <- list.files(app_path, pattern = "\\.Rmd$", full.names = TRUE)
      qmd_files <- list.files(app_path, pattern = "\\.qmd$", full.names = TRUE)
      is_rmd_app <- !has_traditional_shiny && length(rmd_files) > 0
      is_qmd_app <- !has_traditional_shiny && !is_rmd_app && length(qmd_files) > 0

      if (is_qmd_app) {
        logger::log_info("Detected Quarto document for {app_name}, using quarto::quarto_serve", app_name = app_name)
      } else if (is_rmd_app) {
        logger::log_info("Detected R Markdown app for {app_name}, using rmarkdown::run", app_name = app_name)
      } else if (has_traditional_shiny) {
        logger::log_info("Detected traditional Shiny app for {app_name}", app_name = app_name)
      }

      # Prepare log files
      output_log <- file.path(config$config$log_dir, paste0(app_name, "_output.log"))
      error_log <- file.path(config$config$log_dir, paste0(app_name, "_error.log"))

      # Start the app process
      process <- callr::r_bg(
        function(app_path, port, output_log, error_log, is_rmd_app, rmd_files, is_qmd_app, qmd_files) {
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

          if (is_qmd_app && length(qmd_files) > 0) {
            # Load quarto library with error handling
            tryCatch(
              {
                requireNamespace("quarto", quietly = TRUE)
              },
              error = function(e) {
                stop("quarto package required. Install with: install.packages('quarto')")
              }
            )

            # Use the first .qmd file found (use absolute path for security)
            qmd_file <- qmd_files[1]

            # Validate that the qmd file exists and is readable
            if (!file.exists(qmd_file)) {
              stop("Quarto document not found: ", qmd_file)
            }

            # Start the Quarto app using quarto::quarto_serve
            quarto::quarto_serve(
              input = qmd_file,
              render = TRUE,
              port = port,
              host = "127.0.0.1",
              browse = FALSE
            )
          } else if (is_rmd_app && length(rmd_files) > 0) {
            # Load rmarkdown library with error handling
            tryCatch(
              {
                requireNamespace("rmarkdown", quietly = TRUE)
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
          rmd_files = rmd_files,
          is_qmd_app = is_qmd_app,
          qmd_files = qmd_files
        )
      )

      config$add_app_process(app_name, process)

      # Wait for app to start
      Sys.sleep(3)

      if (process$is_alive()) {
        logger::log_info("Successfully started app {app_name}", app_name = app_name)
        return(TRUE)
      } else {
        logger::log_error("Failed to start app {app_name}", app_name = app_name)
        config$remove_app_process(app_name)
        return(FALSE)
      }
    },
    start_app_on_demand = function(app_name) {
      "Start a non-resident app on demand if not already running"

      app_config <- config$get_app_config(app_name)
      if (is.null(app_config)) {
        logger::log_error("Cannot start app on demand: {app_name} not found in configuration", app_name = app_name)
        return(FALSE)
      }

      # Check if app is already running
      process <- config$get_app_process(app_name)
      if (!is.null(process) && is_process_alive(process)) {
        logger::log_debug("App {app_name} already running on demand", app_name = app_name)
        return(TRUE)
      }

      logger::log_info("Starting app {app_name} on demand", app_name = app_name)

      # Start the app
      success <- start_app(app_config)
      if (success) {
        logger::log_info("Successfully started app {app_name} on demand", app_name = app_name)
      } else {
        logger::log_error("Failed to start app {app_name} on demand", app_name = app_name)
      }

      return(success)
    },
    restart_app = function(app_name) {
      "Restart a specific application"

      app_config <- config$get_app_config(app_name)
      if (is.null(app_config)) {
        return(list(success = FALSE, message = "App not found"))
      }

      logger::log_info("Restarting app: {app_name}", app_name = app_name)

      tryCatch(
        {
          # Close existing connections for this app
          cleanup_app_connections(app_name)

          # Stop existing process
          process <- config$get_app_process(app_name)
          if (!is.null(process) && is_process_alive(process)) {
            kill_process_safely(process)
          }
          config$remove_app_process(app_name)

          # Wait a moment before restarting
          Sys.sleep(config$config$restart_delay %||% 5)

          # Start new process
          start_app(app_config)

          return(list(success = TRUE, message = paste("App", app_name, "restarted successfully")))
        },
        error = function(e) {
          logger::log_error("Failed to restart app {app_name}: {error}", app_name = app_name, error = e$message)
          return(list(success = FALSE, message = paste("Failed to restart app:", e$message)))
        }
      )
    },
    stop_app = function(app_name) {
      "Stop a specific application"

      logger::log_info("Stopping app: {app_name}", app_name = app_name)

      process <- config$get_app_process(app_name)
      if (!is.null(process)) {
        success <- kill_process_safely(process)
        config$remove_app_process(app_name)

        if (success) {
          logger::log_info("Successfully stopped app {app_name}", app_name = app_name)
          return(list(success = TRUE, message = paste("App", app_name, "stopped successfully")))
        } else {
          return(list(success = FALSE, message = paste("Failed to stop app", app_name)))
        }
      }

      return(list(success = FALSE, message = paste("App", app_name, "not found or already stopped")))
    },
    stop_app_immediately = function(app_name) {
      "Stop a non-resident app immediately when connections reach zero"

      app_config <- config$get_app_config(app_name)
      if (is.null(app_config)) {
        logger::log_error("Cannot stop unknown app: {app_name}", app_name = app_name)
        return(FALSE)
      }

      # Only allow immediate stops for non-resident apps
      if (app_config$resident) {
        logger::log_debug("Not stopping resident app: {app_name}", app_name = app_name)
        return(FALSE)
      }

      logger::log_info("Immediately stopping non-resident app {app_name} (no active connections)", app_name = app_name)
      result <- stop_app(app_name)
      return(result$success)
    },
    health_check = function() {
      "Check health of all applications and restart if necessary"

      for (app_config in config$config$apps) {
        app_name <- app_config$name
        process <- config$get_app_process(app_name)

        if (!is.null(process)) {
          if (!is_process_alive(process)) {
            logger::log_error("App {app_name} died, restarting", app_name = app_name)

            # Clean up connections for this app
            cleanup_app_connections(app_name)

            # Remove dead process
            config$remove_app_process(app_name)

            # Only restart if it's a resident app
            if (app_config$resident) {
              Sys.sleep(config$config$restart_delay %||% 5)
              start_app(app_config)
            } else {
              logger::log_info("Non-resident app {app_name} died, will start on next request", app_name = app_name)
            }
          }
        } else {
          # App not running - only start if it's resident
          if (app_config$resident) {
            logger::log_info("Resident app {app_name} not running, starting", app_name = app_name)
            start_app(app_config)
          } else {
            logger::log_debug("Non-resident app {app_name} is stopped (normal state)", app_name = app_name)
          }
        }
      }
    },
    cleanup_app_connections = function(app_name) {
      "Clean up WebSocket connections for a specific app"

      # Find and close backend connections for this app
      for (session_id in names(config$get_all_backend_connections())) {
        conn_info <- config$get_backend_connection(session_id)
        if (!is.null(conn_info) && conn_info$app_name == app_name) {
          if (!is.null(conn_info$ws)) {
            tryCatch(conn_info$ws$close(), error = function(e) {})
          }
          config$remove_backend_connection(session_id)
        }
      }

      # Find and close client connections for this app
      sessions_to_remove <- c()
      for (session_id in names(config$get_all_ws_connections())) {
        conn_info <- config$get_ws_connection(session_id)
        if (!is.null(conn_info) && !is.null(conn_info$app_name) && conn_info$app_name == app_name) {
          if (!is.null(conn_info$ws)) {
            tryCatch(conn_info$ws$close(), error = function(e) {})
          }
          sessions_to_remove <- c(sessions_to_remove, session_id)
        }
      }

      # Remove closed connections
      for (session_id in sessions_to_remove) {
        config$remove_ws_connection(session_id)
      }
    },
    cleanup_stale_connections = function() {
      "Clean up stale connections based on timeout"

      current_time <- Sys.time()
      timeout_threshold <- current_time - (config$CONNECTION_TIMEOUT_MINUTES * 60)
      connections_cleaned <- 0

      # Clean up stale backend connections
      for (session_id in names(config$get_all_backend_connections())) {
        conn_info <- config$get_backend_connection(session_id)

        # Check if connection has timestamp and is stale
        if (!is.null(conn_info$last_activity) && conn_info$last_activity < timeout_threshold) {
          if (!is.null(conn_info$ws)) {
            tryCatch(conn_info$ws$close(), error = function(e) {})
          }
          config$remove_backend_connection(session_id)
          connections_cleaned <- connections_cleaned + 1
        }
      }

      # Clean up stale client connections
      for (session_id in names(config$get_all_ws_connections())) {
        conn_info <- config$get_ws_connection(session_id)

        # Check if connection has timestamp and is stale
        if (!is.null(conn_info$last_activity) && conn_info$last_activity < timeout_threshold) {
          config$remove_ws_connection(session_id)
          connections_cleaned <- connections_cleaned + 1
        }
      }

      if (connections_cleaned > 0) {
        logger::log_info("Cleaned up {connections_cleaned} stale connections", connections_cleaned = connections_cleaned)
      }

      # Log current connection counts for monitoring
      backend_count <- length(config$get_all_backend_connections())
      ws_count <- length(config$get_all_ws_connections())
      logger::log_debug("Active connections - Backend: {backend_count}, WebSocket: {ws_count}",
        backend_count = backend_count, ws_count = ws_count
      )

      return(connections_cleaned)
    },
    cleanup_dead_processes = function() {
      "Clean up dead processes from tracking"

      processes_cleaned <- 0
      app_names <- names(config$get_all_app_processes())

      for (app_name in app_names) {
        process <- config$get_app_process(app_name)
        if (!is.null(process) && !is_process_alive(process)) {
          config$remove_app_process(app_name)
          processes_cleaned <- processes_cleaned + 1
          logger::log_info("Removed dead process for app {app_name}", app_name = app_name)
        }
      }

      if (processes_cleaned > 0) {
        logger::log_info("Cleaned up {processes_cleaned} dead processes", processes_cleaned = processes_cleaned)
      }

      return(processes_cleaned)
    },
    get_app_status = function(app_name) {
      "Get status of a specific app"

      process <- config$get_app_process(app_name)
      app_config <- config$get_app_config(app_name)

      if (is.null(app_config)) {
        return(NULL)
      }

      status <- if (is.null(process)) {
        if (app_config$resident) {
          "stopped" # Resident app should be running
        } else {
          "dormant" # Non-resident app is normally stopped
        }
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

      return(list(
        name = app_name,
        status = status,
        resident = app_config$resident,
        port = app_config$port,
        path = app_config$path,
        connections = app_connections,
        pid = if (!is.null(process) && is_process_alive(process)) process$get_pid() else NULL
      ))
    },
    get_all_app_status = function() {
      "Get status of all apps"

      apps_status <- list()

      # Sort apps alphabetically by name
      sorted_apps <- config$config$apps[order(sapply(config$config$apps, function(app) app$name))]

      for (app_config in sorted_apps) {
        app_name <- app_config$name
        status <- get_app_status(app_name)
        if (!is.null(status)) {
          apps_status[[app_name]] <- status
        }
      }

      return(apps_status)
    },
    get_app_connection_count = function(app_name) {
      "Get the current connection count for a specific app"

      app_connections <- 0
      for (conn in config$get_all_ws_connections()) {
        if (!is.null(conn$app_name) && conn$app_name == app_name) {
          app_connections <- app_connections + 1
        }
      }

      return(app_connections)
    },
    stop_all_apps = function() {
      "Stop all running applications"

      logger::log_info("Stopping all applications...")

      for (app_name in names(config$get_all_app_processes())) {
        stop_app(app_name)
      }

      # Clean up all connections
      for (session_id in names(config$get_all_backend_connections())) {
        conn_info <- config$get_backend_connection(session_id)
        if (!is.null(conn_info) && !is.null(conn_info$ws)) {
          tryCatch(conn_info$ws$close(), error = function(e) {})
        }
      }

      # Clear all connection tracking
      config$backend_connections <<- list()
      config$ws_connections <<- list()

      logger::log_info("All applications stopped")
    }
  )
)

# Create process manager factory function
create_process_manager <- function(server_config) {
  return(ProcessManager$new(server_config))
}
