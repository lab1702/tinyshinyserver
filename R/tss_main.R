# Main Application Entry Point (packaged)
# Defines the TinyShinyServer class and related methods for package use.

# NOTE:
# In the package context, all modules under R/ are sourced automatically by R's
# package loader, so we do not call source("R/...") here.

# Main Application Class
TinyShinyServer <- setRefClass("TinyShinyServer",
  fields = list(
    config = "ANY",
    process_manager = "ANY",
    template_manager = "ANY",
    connection_manager = "ANY",
    proxy_server = "ANY",
    management_server = "ANY",
    is_shutting_down = "logical",
    cleanup_in_progress = "logical"
  ),
  methods = list(
    initialize = function(config_file = "config.json") {
      "Initialize the server with all components"

      # Initialize configuration with the provided config file
      config <<- create_server_config(config_file)

      # Initialize logging
      setup_logging(config$config$log_dir)
      config$log_port_assignments()

      # Initialize other components
      process_manager <<- create_process_manager(config)
      template_manager <<- create_template_manager(
        base_url = config$config$base_path, title = config$config$title
      )
      connection_manager <<- create_connection_manager(config, process_manager)
      proxy_server <<- NULL
      management_server <<- NULL
      is_shutting_down <<- FALSE
      cleanup_in_progress <<- FALSE

      logger::log_info("Tiny Shiny Server initialized")
    },
    start = function() {
      "Start the complete server system"

      # Cover failures before run_event_loop installs its own shutdown guard.
      on.exit({
        if (!is_shutting_down) shutdown()
      }, add = TRUE)

      logger::log_info("Starting Tiny Shiny Server")
      logger::log_info("Press Ctrl-C to shutdown gracefully")

      # Start all Shiny applications
      start_all_apps()

      # Start monitoring services
      start_monitoring_services()

      # Start HTTP servers
      start_servers()

      # Set up async processing
      setup_async_processing()

      # Main event loop
      run_event_loop()
    },
    start_all_apps = function() {
      "Start all configured resident Shiny applications"

      logger::log_info("Starting resident applications...")

      for (app_config in config$config$apps) {
        if (app_config$resident) {
          if (is_process_alive(config$get_app_process(app_config$name))) {
            logger::log_info("Resident app already running: {app_name}", app_name = app_config$name)
            next
          }
          success <- process_manager$start_app(app_config)
          if (!success) {
            logger::log_error("Failed to start resident app: {app_name}", app_name = app_config$name)
          }
        } else {
          logger::log_info("Skipping non-resident app: {app_name} (will start on-demand)", app_name = app_config$name)
        }
      }
    },
    start_monitoring_services = function() {
      "Start health monitoring and cleanup services"

      logger::log_info("Starting monitoring services")

      # Health check scheduler
      schedule_health_check <- function() {
        if (!is_shutting_down) {
          tryCatch(process_manager$health_check(), error = function(e) {
            logger::log_error("Health check failed: {error}", error = conditionMessage(e))
          })
          if (!is_shutting_down) {
            later::later(schedule_health_check, config$config$health_check_interval %||% 10)
          }
        }
      }
      later::later(schedule_health_check, 5)

      # Cleanup scheduler
      # NOTE: Running cleanup in main thread instead of future to avoid race conditions
      # Future-based cleanup runs in a separate process and modifications don't affect main process
      schedule_cleanup <- function() {
        if (!is_shutting_down) {
          # Simple lock to prevent concurrent cleanup
          if (cleanup_in_progress) {
            logger::log_debug("Cleanup already in progress, skipping this cycle")
            later::later(schedule_cleanup, config$CLEANUP_INTERVAL_SECONDS)
            return()
          }

          cleanup_in_progress <<- TRUE

          # Ensure flag is always reset, even if interrupted or error occurs
          on.exit(
            {
              cleanup_in_progress <<- FALSE
            },
            add = TRUE
          )

          tryCatch(
            {
              # Run cleanup directly in main thread
              process_manager$cleanup_stale_connections()
              process_manager$cleanup_dead_processes()

              # Validate connection count consistency and auto-fix errors
              validation_result <- config$validate_connection_count_consistency(fix_errors = TRUE)
              if (!validation_result$consistent) {
                logger::log_warn("Connection count cache had inconsistencies, fixed {count} apps",
                  count = length(validation_result$inconsistencies)
                )
              }
            },
            error = function(e) {
              logger::log_error("Error during cleanup: {error}", error = e$message)
            }
          )

          later::later(schedule_cleanup, config$CLEANUP_INTERVAL_SECONDS)
        }
      }
      later::later(schedule_cleanup, config$CLEANUP_INTERVAL_SECONDS)
    },
    start_servers = function() {
      "Start the HTTP proxy and management servers"

      # Start proxy server
      proxy_host <- config$get_proxy_host()
      proxy_port <- config$config$proxy_port %||% 3838

      logger::log_info("Starting proxy server on http://{proxy_host}:{proxy_port}{base_path}/",
        proxy_host = proxy_host, proxy_port = proxy_port, base_path = config$config$base_path
      )

      proxy_server <<- httpuv::startServer(
        host = proxy_host,
        port = proxy_port,
        app = list(
          onHeaders = function(req) {
            reject_large_request_body(req, (config$config$max_request_size_mb %||% 100) * 1024^2)
          },
          call = function(req) handle_http_request(req, config, template_manager, connection_manager, process_manager),
          onWSOpen = function(ws) handle_websocket_connection(ws, config, connection_manager, process_manager)
        )
      )

      # Start management server
      management_port <- config$config$management_port %||% 3839

      logger::log_info("Starting management server on http://127.0.0.1:{management_port}",
        management_port = management_port
      )

      management_server <<- httpuv::startServer(
        host = "127.0.0.1",
        port = management_port,
        app = list(
          # Management requests carry no body.
          onHeaders = function(req) reject_large_request_body(req, 65536),
          call = function(req) handle_management_request(req, config, process_manager, template_manager)
        )
      )
    },
    setup_async_processing = function() {
      "Configure async processing for better performance"

      # NOTE: Removed future::multisession to avoid race conditions
      # The cleanup scheduler now runs in the main thread for safety
      # If async processing is needed in the future, ensure proper synchronization
      logger::log_info("Async processing: using sequential mode to avoid race conditions")
    },
    run_event_loop = function() {
      "Main event loop with proper error handling"

      tryCatch({
        while (TRUE) {
          # Check for a shutdown requested through the management API
          if (isTRUE(config$shutdown_requested)) {
            logger::log_info("Shutdown requested, initiating graceful shutdown")
            break
          }

          # Reload between requests: stopping app processes blocks
          if (isTRUE(config$reload_requested)) {
            config$field("reload_requested", FALSE)
            tryCatch(reload(), error = function(e) {
              config$field("last_reload", list(success = FALSE, message = conditionMessage(e)))
              logger::log_error("Configuration reload failed: {error}", error = conditionMessage(e))
            })
          }

          # Process pending tasks
          later::run_now()

          # Service HTTP events
          httpuv::service()
        }
      }, interrupt = function(e) {
        logger::log_info("Interrupt received, shutting down...")
        shutdown()
      }, error = function(e) {
        logger::log_error("Server error: {error}", error = e$message)
        shutdown()
      }, finally = {
        shutdown()
      })
    },
    reload = function() {
      "Stop all apps, apply the configuration file again, and start the resident apps"

      reload_check <- config$read_reload_config()
      if (!reload_check$valid) {
        config$field("last_reload", list(success = FALSE, message = reload_check$error))
        logger::log_error("Configuration reload rejected: {error}", error = reload_check$error)
        return(invisible(reload_check))
      }

      logger::log_info("Reloading configuration from {file}", file = config$config_file)

      # Close every client socket so browsers show that their session ended
      for (session_id in names(config$get_all_ws_connections())) {
        connection_manager$close_client_connection(session_id)
      }
      process_manager$stop_all_apps()
      config$reset_app_state()

      # An app that did not stop still holds its port, so keep the ports it was given
      surviving <- names(config$get_all_app_processes())
      if (length(surviving) > 0) {
        error <- paste("Could not stop apps:", paste(surviving, collapse = ", "))
        logger::log_error("Configuration reload failed, keeping the previous configuration: {error}", error = error)
        config$field("last_reload", list(success = FALSE, message = error))
        start_all_apps()
        return(invisible(list(valid = FALSE, error = error)))
      }

      # Assign ports only now, so the stopped apps' ports can be reused
      old_config <- config$config
      config$field("config", reload_check$config)
      result <- tryCatch(
        {
          config$assign_app_ports()
          list(valid = TRUE)
        },
        error = function(e) {
          logger::log_error("Configuration reload failed, keeping the previous configuration: {error}",
            error = conditionMessage(e)
          )
          config$field("config", old_config)
          list(valid = FALSE, error = conditionMessage(e))
        }
      )

      if (!identical(config$config$log_dir, old_config$log_dir)) {
        setup_logging(config$config$log_dir)
      }
      template_manager$field("server_title", config$config$title)
      template_manager$field("base_url", config$config$base_path %||% "")
      config$log_port_assignments()

      start_all_apps()
      if (result$valid) logger::log_info("Configuration reloaded")
      config$field("last_reload", list(
        success = result$valid,
        message = if (result$valid) "Configuration reloaded" else result$error
      ))
      invisible(result)
    },
    shutdown = function() {
      "Gracefully shutdown the entire server"

      # Set shutdown flag to prevent monitoring services from restarting apps
      is_shutting_down <<- TRUE

      logger::log_info("Shutting down Tiny Shiny Server...")

      # Stop management server
      if (!is.null(management_server)) {
        tryCatch(
          {
            httpuv::stopServer(management_server)
            management_server <<- NULL
            logger::log_info("Management server stopped")
          },
          error = function(e) {
            logger::log_error("Error stopping management server: {error}", error = e$message)
          }
        )
      }

      # Stop proxy server
      if (!is.null(proxy_server)) {
        tryCatch(
          {
            httpuv::stopServer(proxy_server)
            proxy_server <<- NULL
            logger::log_info("Proxy server stopped")
          },
          error = function(e) {
            logger::log_error("Error stopping proxy server: {error}", error = e$message)
          }
        )
      }

      # Stop all applications
      if (!is.null(process_manager)) {
        process_manager$stop_all_apps()
      }

      logger::log_info("Server shutdown complete")
      # NOTE: Intentionally not calling quit() here to avoid killing the host R session
    },
    get_server_status = function() {
      "Get comprehensive server status"

      total_connections <- length(config$get_all_ws_connections())
      running_apps <- 0

      for (app_name in names(config$get_all_app_processes())) {
        process <- config$get_app_process(app_name)
        if (!is.null(process) && is_process_alive(process)) {
          running_apps <- running_apps + 1
        }
      }

      return(list(
        total_apps = length(config$config$apps),
        running_apps = running_apps,
        total_connections = total_connections,
        proxy_host = config$get_proxy_host(),
        proxy_port = config$config$proxy_port %||% 3838,
        management_port = config$config$management_port %||% 3839,
        log_dir = config$config$log_dir
      ))
    }
  )
)
