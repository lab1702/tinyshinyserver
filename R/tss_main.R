# Main Application Entry Point (packaged)
# Defines the TinyShinyServer class and related methods for package use.

# NOTE:
# In the package context, all modules under R/ are sourced automatically by R's
# package loader, so we do not call source("R/...") here.

# How long the server keeps serving after a shutdown is requested through the
# management API, so that httpuv can finish sending the answer to that request
SHUTDOWN_RESPONSE_SECONDS <- 0.5

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
    shutdown_complete = "logical", # A shutdown that was interrupted runs again
    # Cancels the pending health check, so that a reload that changes
    # health_check_interval applies it at once; NULL before monitoring starts
    cancel_health_check = "ANY"
  ),
  methods = list(
    initialize = function(config_file = "config.json") {
      "Initialize the server with all components"

      # Initialize configuration with the provided config file
      config <<- create_server_config(config_file)

      # A server started with the ports of one that is running stops here,
      # before it writes to the server log that the other one may be
      # writing, under port assignments that are not the other's
      ports_in_use()

      # Initialize logging
      setup_logging(config$config$log_dir, config$config$log_level %||% "INFO")
      config$log_port_assignments()
      config$log_ignored_fields()
      config$log_unusable_apps()

      # Initialize other components
      process_manager <<- create_process_manager(config)
      template_manager <<- create_template_manager(
        base_url = config$config$base_path, title = config$config$title
      )
      connection_manager <<- create_connection_manager(config, process_manager)
      proxy_server <<- NULL
      management_server <<- NULL
      is_shutting_down <<- FALSE
      shutdown_complete <<- FALSE
      cancel_health_check <<- NULL

      logger::log_info("Tiny Shiny Server initialized")
    },
    start = function() {
      "Start the complete server system"

      # Its shutdown is done and is not repeated, so it could not stop a
      # second run's servers and apps
      if (isTRUE(shutdown_complete)) {
        stop("This server has been shut down; call start_tss() to start a new one", call. = FALSE)
      }

      # Cover failures before run_event_loop installs its own shutdown guard,
      # and finish a shutdown that a second Ctrl-C interrupted. A completed
      # shutdown does nothing when called again.
      on.exit(shutdown(), add = TRUE)

      logger::log_info("Starting Tiny Shiny Server")
      logger::log_info("Press Ctrl-C to shutdown gracefully")

      # Bind the proxy and management ports first. If another server already
      # holds them, this one stops here, before it starts app processes or
      # rotates the app logs that the other server is writing. Requests are
      # served only once the event loop runs. The log says why it stopped.
      tryCatch(start_servers(), error = function(e) {
        logger::log_error("Could not start the server: {error}", error = conditionMessage(e))
        stop(e)
      })

      # Start all Shiny applications
      start_all_apps()

      # Start monitoring services
      start_monitoring_services()

      # Main event loop
      run_event_loop()
    },
    ports_in_use = function() {
      "Stop with an error when the proxy or management port is in use"

      ports <- list(
        list(name = "proxy", host = config$get_proxy_host(), port = config$config$proxy_port %||% 3838),
        list(name = "management", host = "127.0.0.1", port = config$config$management_port %||% 3839)
      )
      for (server in ports) {
        if (is_port_in_use(server$host, server$port, loopback = TRUE)) {
          # A port that nothing answers on could not be opened for another
          # reason, such as a port below 1024 without the right to use it, or
          # an IPv6 address on a machine without IPv6. Saying it is in use
          # would send the user looking for a server that does not exist.
          answered <- loopback_port_accepts(server$port) || loopback_port_accepts(server$port, address = "::1")
          reason <- if (answered) {
            "is already in use by another program, perhaps another server"
          } else {
            sprintf(
              "could not be opened at %s; another program may hold it, or the port or address is not available to this user or on this machine",
              server$host
            )
          }
          stop(sprintf("The %s port %s %s; nothing was started", server$name, server$port, reason), call. = FALSE)
        }
      }
      invisible(FALSE)
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
          # An app that cannot start, for example because its log files cannot
          # be opened, must not keep the remaining apps from starting; health
          # checks retry it
          success <- tryCatch(process_manager$start_app(app_config), error = function(e) {
            logger::log_error("Error starting app {app_name}: {error}",
              app_name = app_config$name, error = conditionMessage(e))
            FALSE
          })
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

      # Health checks, first after 5 seconds
      schedule_health_check(5)

      # Cleanup scheduler. It runs on the event loop like every other callback,
      # so one cleanup always finishes before the next starts. Exited app
      # processes are removed by the health check above.
      schedule_cleanup <- function() {
        if (!is_shutting_down) {
          tryCatch(process_manager$cleanup_stale_connections(), error = function(e) {
            logger::log_error("Error during cleanup: {error}", error = e$message)
          })

          later::later(schedule_cleanup, config$CLEANUP_INTERVAL_SECONDS)
        }
      }
      later::later(schedule_cleanup, config$CLEANUP_INTERVAL_SECONDS)
    },
    schedule_health_check = function(delay) {
      "Run a health check after delay seconds, and every health_check_interval seconds after it, in place of any check already pending"

      if (is.function(cancel_health_check)) cancel_health_check()
      cancel_health_check <<- later::later(function() {
        cancel_health_check <<- NULL
        if (is_shutting_down) return()
        tryCatch(process_manager$health_check(), error = function(e) {
          logger::log_error("Health check failed: {error}", error = conditionMessage(e))
        })
        if (!is_shutting_down) {
          schedule_health_check(config$config$health_check_interval %||% 10)
        }
      }, delay)
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
            rejected <- reject_large_request_body(req, (config$config$max_request_size_mb %||% 100) * 1024^2)
            if (is.null(rejected)) NULL else without_head_body(req, rejected)
          },
          # httpuv queues each request's handler on the event loop, and
          # stopServer() leaves those already queued, which then run after
          # the shutdown, in the R session's own event loop. They would start
          # app processes that nothing stops. Requests that arrive while the
          # server shuts down are refused too.
          call = note_interrupts(config, function(req) {
            if (isTRUE(is_shutting_down)) {
              return(without_head_body(req, create_error_response("The server is shutting down", 503)))
            }
            handle_http_request(req, config, template_manager, connection_manager, process_manager)
          }),
          onWSOpen = note_interrupts(config, function(ws) {
            if (isTRUE(is_shutting_down)) {
              tryCatch(ws$close(), error = function(e) NULL)
              return(invisible(NULL))
            }
            handle_websocket_connection(ws, config, connection_manager, process_manager)
          })
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
          onHeaders = function(req) {
            rejected <- reject_large_request_body(req, 65536)
            if (is.null(rejected)) NULL else management_response(req, rejected)
          },
          call = note_interrupts(config, function(req) {
            if (isTRUE(is_shutting_down)) {
              return(management_response(req, create_error_response("The server is shutting down", 503)))
            }
            handle_management_request(req, config, process_manager, template_manager)
          }),
          # The management server serves no WebSocket. Without this handler
          # httpuv would accept any handshake, whatever its Host or Origin,
          # and write an error to the console for each one.
          onWSOpen = function(ws) tryCatch(ws$close(), error = function(e) NULL)
        )
      )
    },
    run_event_loop = function() {
      "Main event loop with proper error handling"

      # When the management API asked for a shutdown, or NULL
      shutdown_requested_at <- NULL
      outcome <- tryCatch({
        while (TRUE) {
          # A Ctrl-C that a callback of httpuv or the websocket package
          # caught; see note_interrupts()
          if (isTRUE(config$interrupt_requested)) {
            logger::log_info("Interrupt received, shutting down...")
            break
          }

          # A shutdown requested through the management API. httpuv sends
          # the answer to that request on its own thread, and stopServer()
          # cuts off an answer that it has not finished sending, so the loop
          # runs on for a moment first, refusing new requests.
          if (isTRUE(config$shutdown_requested)) {
            if (is.null(shutdown_requested_at)) {
              logger::log_info("Shutdown requested, initiating graceful shutdown")
              is_shutting_down <<- TRUE
              # The wait below runs timers that are due, and a crashed app's
              # restart would start it only to stop it again, replacing the
              # log that holds the crash with the empty log of that run
              process_manager$cancel_pending_restarts()
              shutdown_requested_at <- Sys.time()
            }
            # A clock set back ends the wait rather than prolonging it
            waited <- as.numeric(difftime(Sys.time(), shutdown_requested_at, units = "secs"))
            if (waited < 0 || waited >= SHUTDOWN_RESPONSE_SECONDS) {
              break
            }
            later::run_now()
            httpuv::service(50)
            next
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
      }, error = function(e) {
        logger::log_error("Server error: {error}", error = e$message)
        e
      }, finally = {
        # The one shutdown for every way out of the loop
        shutdown()
      })
      # The servers and apps have stopped. An error ends start_tss() with an
      # error too, so that a script run by Rscript exits with a failure status
      # and a service manager can restart it.
      if (inherits(outcome, "error")) {
        stop("The server stopped after an error: ", conditionMessage(outcome), call. = FALSE)
      }
      invisible(NULL)
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

      # Browsers are shown that their session ended, except those of an app
      # that cannot be stopped, which keeps its users
      process_manager$stop_all_apps()

      # An app that did not stop still holds its port, so keep the ports it
      # was given, and the state of its process
      surviving <- names(config$get_all_app_processes())
      config$reset_app_state(keep = surviving)
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

      log_problem <- NULL
      if (!identical(config$config$log_dir, old_config$log_dir) ||
        !identical(config$config$log_level, old_config$log_level)) {
        # The new log_dir was checked before any app stopped. If it has
        # become unwritable since, keep the previous log_dir, for the app logs
        # and render records as well as the server log, rather than leave the
        # reload half applied, but use the new log level, and say so.
        log_problem <- tryCatch(
          {
            setup_logging(config$config$log_dir, config$config$log_level %||% "INFO")
            NULL
          },
          error = function(e) {
            logger::log_threshold(config$config$log_level %||% "INFO", namespace = "tinyshinyserver")
            logger::log_warn("Keeping the previous log_dir: {error}", error = conditionMessage(e))
            kept <- config$config
            kept$log_dir <- old_config$log_dir
            config$field("config", kept)
            conditionMessage(e)
          }
        )
      }
      template_manager$field("server_title", config$config$title)
      template_manager$field("base_url", config$config$base_path %||% "")
      config$log_port_assignments()
      # The next health check would otherwise come after the previous interval
      if (is.function(cancel_health_check) &&
        !identical(config$config$health_check_interval, old_config$health_check_interval)) {
        schedule_health_check(config$config$health_check_interval %||% 10)
      }

      start_all_apps()
      message <- result$error
      if (result$valid) {
        logger::log_info("Configuration reloaded")
        config$log_ignored_fields()
        config$log_unusable_apps()
        # The dashboard shows this message, so name any option that had no effect
        message <- "Configuration reloaded"
        if (length(config$ignored_fields) > 0) {
          message <- paste0(
            message, "; ignored unrecognized fields: ",
            paste(config$ignored_fields, collapse = ", ")
          )
        }
        if (!is.null(log_problem)) {
          message <- paste0(message, "; kept the previous log_dir: ", log_problem)
        }
      }
      config$field("last_reload", list(success = result$valid, message = message))
      invisible(result)
    },
    shutdown = function() {
      "Gracefully shutdown the entire server"

      if (isTRUE(shutdown_complete)) {
        return(invisible(NULL))
      }

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

      shutdown_complete <<- TRUE
      # An app that could not be stopped keeps its port until this R session ends
      surviving <- names(config$get_all_app_processes())
      if (length(surviving) > 0) {
        logger::log_error("Server shutdown complete, but these apps are still running: {apps}",
          apps = paste(surviving, collapse = ", ")
        )
      } else {
        logger::log_info("Server shutdown complete")
      }
      # NOTE: Intentionally not calling quit() here to avoid killing the host R session
    }
  )
)
