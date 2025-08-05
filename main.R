#!/usr/bin/env Rscript

# Main Application Entry Point
# Orchestrates all modules and manages server lifecycle

# Load required libraries
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

# Source all module files in correct order
source("R/validation.R") # No dependencies
source("R/utils.R") # Depends on validation
source("R/config.R") # Depends on validation
source("R/template_manager.R") # Depends on validation and utils
source("R/connection_manager.R") # Depends on validation, utils, config
source("R/process_manager.R") # Depends on validation, utils, config
source("R/handlers.R") # Depends on validation, utils, config
source("R/management_api.R") # Depends on all above

# Main Application Class
TinyShinyServer <- setRefClass("TinyShinyServer",
  fields = list(
    config = "ANY",
    process_manager = "ANY",
    template_manager = "ANY",
    connection_manager = "ANY",
    proxy_server = "ANY",
    management_server = "ANY"
  ),
  methods = list(
    initialize = function(config_file = "config.json") {
      "Initialize the server with all components"

      # Initialize configuration
      config <<- create_server_config()

      # Initialize logging
      setup_logging(config$config$log_dir)

      # Initialize other components
      process_manager <<- create_process_manager(config)
      template_manager <<- create_template_manager()
      connection_manager <<- create_connection_manager(config)

      log_info("Tiny Shiny Server initialized")
    },
    start = function() {
      "Start the complete server system"

      log_info("Starting Tiny Shiny Server")
      log_info("Press Ctrl-C to shutdown gracefully")

      # Create shutdown flag monitoring
      setup_shutdown_monitoring()

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
      "Start all configured Shiny applications"

      log_info("Starting applications...")

      for (app_config in config$config$apps) {
        success <- process_manager$start_app(app_config)
        if (!success) {
          log_error("Failed to start app: {app_name}", app_name = app_config$name)
        }
      }
    },
    start_monitoring_services = function() {
      "Start health monitoring and cleanup services"

      log_info("Starting monitoring services")

      # Health check scheduler
      schedule_health_check <- function() {
        process_manager$health_check()
        later::later(schedule_health_check, config$config$health_check_interval %||% 10)
      }
      later::later(schedule_health_check, 5)

      # Cleanup scheduler
      schedule_cleanup <- function() {
        future::future(
          {
            list(
              stale_cleanup = process_manager$cleanup_stale_connections(),
              process_cleanup = process_manager$cleanup_dead_processes()
            )
          },
          seed = NULL
        )
        later::later(schedule_cleanup, config$CLEANUP_INTERVAL_SECONDS)
      }
      later::later(schedule_cleanup, config$CLEANUP_INTERVAL_SECONDS)
    },
    start_servers = function() {
      "Start the HTTP proxy and management servers"

      # Start proxy server
      proxy_host <- config$get_proxy_host()
      proxy_port <- config$config$proxy_port %||% 3838

      log_info("Starting proxy server on http://{proxy_host}:{proxy_port}",
        proxy_host = proxy_host, proxy_port = proxy_port
      )

      proxy_server <<- httpuv::startServer(
        host = proxy_host,
        port = proxy_port,
        app = list(
          call = function(req) handle_http_request(req, config, template_manager, connection_manager),
          onWSOpen = function(ws) handle_websocket_connection(ws, config, connection_manager)
        )
      )

      # Start management server
      management_port <- config$config$management_port %||% 3839

      log_info("Starting management server on http://127.0.0.1:{management_port}",
        management_port = management_port
      )

      management_server <<- httpuv::startServer(
        host = "127.0.0.1",
        port = management_port,
        app = list(
          call = function(req) handle_management_request(req, config, process_manager, template_manager)
        )
      )
    },
    setup_async_processing = function() {
      "Configure async processing for better performance"

      future::plan(future::multisession, workers = 4)
    },
    setup_shutdown_monitoring = function() {
      "Set up shutdown flag monitoring"

      shutdown_flag_file <- file.path(config$config$log_dir, "shutdown.flag")
      if (file.exists(shutdown_flag_file)) {
        file.remove(shutdown_flag_file)
      }

      # Cleanup shutdown flag on exit
      on.exit(
        {
          if (file.exists(shutdown_flag_file)) {
            file.remove(shutdown_flag_file)
          }
        },
        add = TRUE
      )
    },
    run_event_loop = function() {
      "Main event loop with proper error handling"

      shutdown_flag_file <- file.path(config$config$log_dir, "shutdown.flag")

      tryCatch({
        while (TRUE) {
          # Check for shutdown flag
          if (file.exists(shutdown_flag_file)) {
            log_info("Shutdown flag detected, initiating graceful shutdown")
            break
          }

          # Process pending tasks
          later::run_now()

          # Service HTTP events
          httpuv::service()
        }
      }, interrupt = function(e) {
        log_info("Interrupt received, shutting down...")
        shutdown()
      }, error = function(e) {
        log_error("Server error: {error}", error = e$message)
        shutdown()
      }, finally = {
        shutdown()
      })
    },
    shutdown = function() {
      "Gracefully shutdown the entire server"

      log_info("Shutting down Tiny Shiny Server...")

      # Stop management server
      if (!is.null(management_server)) {
        tryCatch(
          {
            httpuv::stopServer(management_server)
            management_server <<- NULL
            log_info("Management server stopped")
          },
          error = function(e) {
            log_error("Error stopping management server: {error}", error = e$message)
          }
        )
      }

      # Stop proxy server
      if (!is.null(proxy_server)) {
        tryCatch(
          {
            httpuv::stopServer(proxy_server)
            proxy_server <<- NULL
            log_info("Proxy server stopped")
          },
          error = function(e) {
            log_error("Error stopping proxy server: {error}", error = e$message)
          }
        )
      }

      # Stop all applications
      if (!is.null(process_manager)) {
        process_manager$stop_all_apps()
      }

      log_info("Server shutdown complete")
      quit(status = 0)
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

# Main entry point function
main <- function(config_file = "config.json") {
  "Main application entry point"

  # Validate arguments
  if (!file.exists(config_file)) {
    cat("Error: Configuration file not found:", config_file, "\n")
    quit(status = 1)
  }

  # Create and start server
  server <- TinyShinyServer$new(config_file)
  server$start()
}

# Run if called directly
if (sys.nframe() == 0) {
  # Parse command line arguments
  args <- commandArgs(trailingOnly = TRUE)
  config_file <- if (length(args) > 0) args[1] else "config.json"

  main(config_file)
}
