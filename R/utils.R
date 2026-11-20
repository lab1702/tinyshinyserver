# Utility Functions Module
# Common utility functions used across the application

# Null coalescing operator (used across all modules)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Helper functions for connection tracking
get_client_ip <- function(req) {
  "Extract client IP address from request headers"

  remote_addr <- req$REMOTE_ADDR
  has_remote_addr <- !is.null(remote_addr) && remote_addr != ""

  # Forwarding headers are client-controlled, so trust them only when the
  # request arrives from a reverse proxy on this machine
  if (has_remote_addr && remote_addr %in% c("127.0.0.1", "::1", "::ffff:127.0.0.1")) {
    forwarded_for <- req$HTTP_X_FORWARDED_FOR
    if (!is.null(forwarded_for) && forwarded_for != "") {
      # The last entry is the one added by the proxy itself
      ips <- trimws(strsplit(forwarded_for, ",")[[1]])
      ips <- ips[ips != ""]
      if (length(ips) > 0) {
        return(ips[length(ips)])
      }
    }

    real_ip <- req$HTTP_X_REAL_IP
    if (!is.null(real_ip) && real_ip != "") {
      return(trimws(real_ip))
    }
  }

  if (has_remote_addr) {
    return(remote_addr)
  }

  return("unknown")
}


# Generate session ID
generate_session_id <- function(req) {
  "Generate secure session ID from request information"

  user_agent <- if (is.null(req$HTTP_USER_AGENT)) "" else req$HTTP_USER_AGENT
  remote_addr <- if (is.null(req$REMOTE_ADDR)) "" else req$REMOTE_ADDR
  timestamp <- as.character(Sys.time())

  # Add cryptographically secure random entropy (32 bytes = 256 bits)
  random_salt <- paste(as.character(openssl::rand_bytes(32)), collapse = "")

  # Create session ID with SHA-256
  session_data <- paste(user_agent, remote_addr, timestamp, random_salt, sep = "|")
  session_id <- digest::digest(session_data, algo = "sha256")

  return(session_id)
}

# Logging setup
setup_logging <- function(log_dir, log_level = "INFO") {
  "Initialize logging system with file and console output"

  # Fail now: the appender drops file write errors, so an unwritable
  # log_dir would otherwise lose the server log without notice
  log_dir_error <- check_log_dir(log_dir)
  if (!is.null(log_dir_error)) {
    stop(log_dir_error, call. = FALSE)
  }

  # Configure only this package's logger namespace so the caller's own
  # logger settings are left untouched
  logger::log_threshold(log_level, namespace = "tinyshinyserver")
  # Package messages use glue placeholders, whatever the global formatter is
  logger::log_formatter(logger::formatter_glue, namespace = "tinyshinyserver")

  # Configure file appender with same format as console
  log_file <- file.path(log_dir, "server.log")

  # Use tee appender to log to both console and file
  logger::log_appender(safe_tee_appender(log_file), namespace = "tinyshinyserver")

  # Log initialization message
  logger::log_info("Logging system initialized with file output to {log_file}", log_file = log_file)
}

# appender_tee writes the console line first. A log file that can no longer
# be written, for example after log_dir is removed, must not raise an error:
# every error handler and shutdown() log, so the servers and apps would be
# left running.
safe_tee_appender <- function(log_file) {
  tee <- logger::appender_tee(log_file)
  function(lines) {
    tryCatch(suppressWarnings(tee(lines)), error = function(e) invisible(NULL))
  }
}

check_log_dir <- function(log_dir) {
  "Create a log directory and check that the server log can be written there"

  log_file <- file.path(log_dir, "server.log")
  writable <- tryCatch(
    suppressWarnings({
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE)
      }
      close(file(log_file, open = "a"))
      TRUE
    }),
    error = function(e) FALSE
  )

  if (writable) NULL else paste("Cannot write the server log to", log_file)
}

# Process management helpers
is_process_alive <- function(process) {
  "Check if a process is still running"

  if (is.null(process)) {
    return(FALSE)
  }

  tryCatch(
    {
      return(process$is_alive())
    },
    error = function(e) {
      return(FALSE)
    }
  )
}

process_ps_handle <- function(process) {
  if (is.function(process$as_ps_handle)) process$as_ps_handle() else ps::ps_handle(process$get_pid())
}

# Resident memory in bytes of a process (by default this R session), or NULL
# when it cannot be read. Only the process itself is measured: listing its
# children would walk the whole process table on every dashboard refresh.
process_memory_bytes <- function(process = NULL) {
  if (!ps::ps_is_supported()) return(NULL)
  tryCatch(
    {
      handle <- if (is.null(process)) ps::ps_handle() else process_ps_handle(process)
      as.numeric(ps::ps_memory_info(handle)[["rss"]])
    },
    error = function(e) NULL
  )
}

# Memory and uptime of a live app process for the management API. Values that
# cannot be read are left out, because jsonlite writes NA as "NA" and NULL as {}.
process_usage <- function(process) {
  if (!is_process_alive(process)) return(list())
  usage <- list()
  usage$memory_bytes <- process_memory_bytes(process)
  usage$uptime_seconds <- tryCatch(
    as.numeric(difftime(Sys.time(), process$get_start_time(), units = "secs")),
    error = function(e) NULL
  )
  usage
}

kill_process_safely <- function(process) {
  "Safely terminate a process"

  if (is.null(process)) {
    return(TRUE)
  }

  tryCatch(
    {
      # processx tracks descendants by inherited markers, even after parent exit.
      # Tree members can exit between being listed and being signalled.
      # Where ps cannot list processes, kill_tree() only raises an error, so
      # the app's own process is stopped below and its children are not found.
      if (is.function(process$kill_tree) && ps::ps_is_supported()) {
        tryCatch(process$kill_tree(), no_such_process = function(e) NULL)
      }
      if (is_process_alive(process)) process$kill()
      # Signal delivery can precede process exit, particularly on macOS.
      # Wait briefly for termination, but still report a surviving process.
      if (is.function(process$wait)) process$wait(timeout = 1000)
      return(!is_process_alive(process))
    },
    error = function(e) {
      logger::log_error("Error terminating process: {error}", error = e$message)
      return(FALSE)
    }
  )
}

# HTTP response helpers
create_json_response <- function(data, status = 200) {
  "Create a standardized JSON response"

  return(list(
    status = status,
    headers = list("Content-Type" = "application/json"),
    body = jsonlite::toJSON(data, auto_unbox = TRUE)
  ))
}

create_html_response <- function(html, status = 200) {
  "Create a standardized HTML response"

  return(list(
    status = status,
    headers = list("Content-Type" = "text/html"),
    body = html
  ))
}

create_error_response <- function(message, status = 500) {
  "Create a standardized error response"

  return(list(
    status = status,
    headers = list("Content-Type" = "application/json"),
    body = jsonlite::toJSON(list(error = paste(status, "-", message)), auto_unbox = TRUE)
  ))
}

create_redirect_response <- function(location, query_string = NULL) {
  "Create a permanent redirect that keeps the method, body, and query string"

  query <- query_string %||% ""
  if (query != "" && !startsWith(query, "?")) query <- paste0("?", query)
  list(status = 308L, headers = list(Location = paste0(location, query)), body = "")
}

# A reverse proxy serving the apps under base_path may pass the prefix through
# or strip it, so requests are accepted either way. validate_base_path() keeps
# the prefix from overlapping the server's own routes.
strip_base_path <- function(path, base_path) {
  if (is.null(path) || !nzchar(base_path %||% "")) return(path)
  if (startsWith(path, paste0(base_path, "/"))) {
    return(substring(path, nchar(base_path) + 1L))
  }
  path
}

create_503_response <- function(message, retry_after_seconds = 3) {
  "Create a 503 Service Unavailable response with Retry-After header"

  return(list(
    status = 503,
    headers = list(
      "Content-Type" = "application/json",
      "Retry-After" = as.character(retry_after_seconds)
    ),
    body = jsonlite::toJSON(list(
      error = paste("503 -", message),
      retry_after_seconds = retry_after_seconds,
      message = "Please retry this request after the specified delay"
    ), auto_unbox = TRUE)
  ))
}

# Network utilities
# Whether a managed app process listens on a port. Shiny, R Markdown and
# Quarto apps all serve from the R process itself. The proxy forwards cookies
# and credentials, so a successful TCP probe must not trust another program
# that has taken an app's port. Only that process's own sockets are listed:
# walking the process table would block the event loop on every check.
# Where ps cannot list sockets, the probe alone decides.
process_owns_port <- function(process, port) {
  if (!ps::ps_is_supported()) return(TRUE)
  tryCatch({
    sockets <- ps::ps_connections(process_ps_handle(process))
    any(sockets$state %in% "CONN_LISTEN" & sockets$lport %in% port)
  }, error = function(e) FALSE)
}

is_port_in_use <- function(host, port) {
  "Check if a port is in use (an app could not listen on it)"

  # Listen on the port briefly, as an app will. Connecting to it instead
  # waits about a second for each free port on Windows, where a refused
  # loopback connection is retried, which held up startup and every reload.
  tryCatch(
    {
      server <- httpuv::startServer(host, port, list(), quiet = TRUE)
      httpuv::stopServer(server)
      FALSE
    },
    error = function(e) TRUE
  )
}
