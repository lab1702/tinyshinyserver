# Management API Module
# Handles management interface requests and API endpoints

# Management request handler
handle_management_request <- function(req, config, process_manager, template_manager) {
  "Handle management interface requests"

  # Management pages carry one-click restart controls, so never allow them
  # to be framed by another site
  response <- handle_management_request_unframed(req, config, process_manager, template_manager)
  response$headers[["X-Frame-Options"]] <- "DENY"
  response$headers[["Content-Security-Policy"]] <- "frame-ancestors 'none'"
  response
}

handle_management_request_unframed <- function(req, config, process_manager, template_manager) {
  "Validate and route a management request"

  # The management server listens on loopback only. Rejecting other Host
  # names stops DNS-rebinding pages from becoming same-origin with it.
  if (!is_loopback_host_header(req$HTTP_HOST)) {
    return(create_error_response("Invalid Host header", 403))
  }

  # Validate inputs
  validation_result <- validate_request_inputs(
    req$PATH_INFO %||% "/",
    req$REQUEST_METHOD,
    req$QUERY_STRING
  )

  if (!validation_result$valid) {
    return(validation_result$response)
  }

  path <- validation_result$path
  method <- validation_result$method

  logger::log_debug("Management {method} {path}", method = method, path = path)

  # Route management requests
  return(route_management_request(path, method, req, config, process_manager, template_manager))
}

is_loopback_host_header <- function(host) {
  "Whether a Host header is absent or names a loopback address"

  if (is.null(host) || identical(host, "")) {
    return(TRUE)
  }
  host <- tolower(trimws(host))
  host <- sub("^(\\[[^]]*\\]):[0-9]+$", "\\1", host)
  host <- sub("^([^:]+):[0-9]+$", "\\1", host)
  host %in% c("localhost", "127.0.0.1", "[::1]")
}

route_management_request <- function(path, method, req, config, process_manager, template_manager) {
  "Route management requests to appropriate handlers"

  # A custom header makes browser mutations require a same-origin request.
  # Cross-origin preflights are deliberately not granted CORS permission.
  if (method == "POST" && !identical(req$HTTP_X_TINYSHINYSERVER_REQUEST, "management")) {
    return(create_error_response("Missing or invalid X-TinyShinyServer-Request header", 403))
  }

  # Management dashboard
  if (path == "/" && method == "GET") {
    return(handle_management_dashboard(template_manager))
  }

  # API endpoints
  if (path == "/api/apps" && method == "GET") {
    return(handle_management_apps_api(process_manager))
  }

  if (path == "/api/connections" && method == "GET") {
    return(handle_management_connections_api(config))
  }

  if (path == "/api/status" && method == "GET") {
    return(handle_management_status_api(config))
  }

  # App restart endpoint
  if (startsWith(path, "/api/apps/") && endsWith(path, "/restart") && method == "POST") {
    return(handle_app_restart(path, process_manager))
  }

  # Configuration reload endpoint
  if (path == "/api/reload" && method == "POST") {
    return(handle_config_reload(config))
  }

  # Shutdown endpoint
  if (path == "/api/shutdown" && method == "POST") {
    return(handle_server_shutdown(config))
  }

  # Static files for management interface
  if (startsWith(path, "/templates/")) {
    return(template_manager$serve_static_file(gsub("^/templates/", "", path)))
  }

  # 404 for unknown paths
  return(create_error_response("Not Found", 404))
}

handle_management_dashboard <- function(template_manager) {
  "Handle management dashboard page requests"

  tryCatch(
    {
      html <- template_manager$generate_management_page()
      return(create_html_response(html))
    },
    error = function(e) {
      logger::log_error("Error generating management page: {error}", error = e$message)
      return(create_error_response("Internal Server Error", 500))
    }
  )
}

handle_management_apps_api <- function(process_manager) {
  "Handle management API requests for app status"

  tryCatch(
    {
      apps_status <- process_manager$get_all_app_status()
      return(create_json_response(apps_status))
    },
    error = function(e) {
      logger::log_error("Error getting app status for management: {error}", error = e$message)
      return(create_error_response("Internal Server Error", 500))
    }
  )
}

handle_management_connections_api <- function(config) {
  "Handle management API requests for connection info"

  tryCatch(
    {
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

      return(create_json_response(connections))
    },
    error = function(e) {
      logger::log_error("Error getting connections for management: {error}", error = e$message)
      return(create_error_response("Internal Server Error", 500))
    }
  )
}

handle_management_status_api <- function(config) {
  "Handle management API requests for system status"

  tryCatch(
    {
      total_connections <- length(config$get_all_ws_connections())
      running_apps <- 0

      for (app_name in names(config$get_all_app_processes())) {
        process <- config$get_app_process(app_name)
        if (!is.null(process) && is_process_alive(process)) {
          running_apps <- running_apps + 1
        }
      }

      status <- list(
        total_apps = length(config$config$apps),
        running_apps = running_apps,
        total_connections = total_connections,
        server_uptime = "N/A", # Could be enhanced with actual uptime tracking
        memory_usage = "N/A" # Could be enhanced with memory monitoring
      )
      # Requested reloads run after their response, so report the outcome here
      if (length(config$last_reload) > 0) {
        status$last_reload <- config$last_reload
      }

      return(create_json_response(status))
    },
    error = function(e) {
      logger::log_error("Error getting system status for management: {error}", error = e$message)
      return(create_error_response("Internal Server Error", 500))
    }
  )
}

handle_app_restart <- function(path, process_manager) {
  "Handle app restart requests"

  # Extract app name from path
  path_parts <- strsplit(path, "/")[[1]]
  path_parts <- path_parts[path_parts != ""]

  if (length(path_parts) < 3) {
    return(create_error_response("Invalid restart path", 400))
  }

  app_name_validation <- validate_app_name(path_parts[3])
  if (!app_name_validation$valid) {
    return(create_error_response(paste("Invalid app name:", app_name_validation$error), 400))
  }

  app_name <- app_name_validation$sanitized

  tryCatch(
    {
      # Check app status first - don't restart dormant apps
      app_status <- process_manager$get_app_status(app_name)
      if (!is.null(app_status) && app_status$status == "dormant") {
        return(create_json_response(list(
          success = FALSE,
          message = "Cannot restart dormant app. Dormant apps start automatically when accessed."
        )))
      }

      result <- process_manager$restart_app(app_name)

      if (result$success) {
        return(create_json_response(result))
      } else {
        return(list(
          status = 500,
          headers = list("Content-Type" = "application/json"),
          body = jsonlite::toJSON(result, auto_unbox = TRUE)
        ))
      }
    },
    error = function(e) {
      logger::log_error("Error restarting app {app_name}: {error}", app_name = app_name, error = e$message)
      return(create_error_response(paste("Failed to restart app:", e$message), 500))
    }
  )
}

handle_server_shutdown <- function(config) {
  "Handle server shutdown requests"

  logger::log_info("Shutdown requested via management API")

  # Signal only this server's event loop; a file in log_dir would also stop
  # other instances sharing that directory
  config$shutdown_requested <- TRUE
  create_json_response(list(
    success = TRUE,
    message = "Shutdown initiated"
  ))
}

handle_config_reload <- function(config) {
  "Handle requests to reload the configuration and restart all apps"

  # Check the file now so a mistake is reported before any app is stopped
  reload_check <- config$read_reload_config()
  if (!reload_check$valid) {
    logger::log_warn("Configuration reload rejected: {error}", error = reload_check$error)
    return(create_json_response(list(success = FALSE, message = reload_check$error), status = 400))
  }

  logger::log_info("Configuration reload requested via management API")

  # Stopping apps blocks, so the event loop reloads after this response is sent
  config$field("last_reload", list())
  config$reload_requested <- TRUE
  create_json_response(list(
    success = TRUE,
    message = "Reloading configuration and restarting all apps"
  ))
}
