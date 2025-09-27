# Management API Module
# Handles management interface requests and API endpoints

library(jsonlite)
library(logger)

# Management request handler
handle_management_request <- function(req, config, process_manager, template_manager) {
  "Handle management interface requests"

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

  log_debug("Management {method} {path}", method = method, path = path)

  # Route management requests
  return(route_management_request(path, method, req, config, process_manager, template_manager))
}

route_management_request <- function(path, method, req, config, process_manager, template_manager) {
  "Route management requests to appropriate handlers"

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
      log_error("Error generating management page: {error}", error = e$message)
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
      log_error("Error getting app status for management: {error}", error = e$message)
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
      log_error("Error getting connections for management: {error}", error = e$message)
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

      return(create_json_response(status))
    },
    error = function(e) {
      log_error("Error getting system status for management: {error}", error = e$message)
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
      log_error("Error restarting app {app_name}: {error}", app_name = app_name, error = e$message)
      return(create_error_response(paste("Failed to restart app:", e$message), 500))
    }
  )
}

handle_server_shutdown <- function(config) {
  "Handle server shutdown requests"

  log_info("Shutdown requested via management API")

  # Create shutdown flag file
  shutdown_flag_file <- file.path(config$config$log_dir, "shutdown.flag")

  tryCatch(
    {
      writeLines("shutdown", shutdown_flag_file)
      return(create_json_response(list(
        success = TRUE,
        message = "Shutdown initiated"
      )))
    },
    error = function(e) {
      log_error("Error creating shutdown flag: {error}", error = e$message)
      return(create_json_response(list(
        success = FALSE,
        error = e$message
      ), 500))
    }
  )
}

# Additional management utilities
get_server_health <- function(config, process_manager) {
  "Get comprehensive server health information"

  health_info <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    server_status = "running"
  )

  # Check app health
  app_health <- list()
  for (app_config in config$config$apps) {
    app_status <- process_manager$get_app_status(app_config$name)
    app_health[[app_config$name]] <- app_status
  }
  health_info$apps <- app_health

  # Check connection health
  connection_stats <- list(
    total_connections = length(config$get_all_ws_connections()),
    backend_connections = length(config$get_all_backend_connections())
  )
  health_info$connections <- connection_stats

  # Check system resources (basic)
  tryCatch(
    {
      # This could be enhanced with actual system monitoring
      health_info$system <- list(
        memory_usage = "N/A",
        cpu_usage = "N/A",
        disk_space = "N/A"
      )
    },
    error = function(e) {
      health_info$system <- list(error = "Could not get system info")
    }
  )

  return(health_info)
}

create_management_response <- function(data, status = 200, message = NULL) {
  "Create a standardized management API response"

  response_data <- list(
    status = if (status >= 200 && status < 300) "success" else "error",
    data = data
  )

  if (!is.null(message)) {
    response_data$message <- message
  }

  response_data$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  return(list(
    status = status,
    headers = list(
      "Content-Type" = "application/json",
      "Cache-Control" = "no-cache"
    ),
    body = jsonlite::toJSON(response_data, auto_unbox = TRUE)
  ))
}
