# Utility Functions Module
# Common utility functions used across the application

# Null coalescing operator (used across all modules)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# Apps in alphabetical order of their names, as the pages and the API list them
sort_apps_by_name <- function(apps) {
  apps[order(vapply(apps, function(app) app$name, character(1)))]
}

# Evaluate a request handler's response, answering an error that the handler
# did not anticipate with HTTP 500 and no detail. httpuv would otherwise send
# the client the R error message, which can name files on the server; the
# message goes to the server log instead.
respond_safely <- function(response) {
  tryCatch(response, error = function(e) {
    tryCatch(
      logger::log_error("Unexpected error while answering a request: {error}", error = conditionMessage(e)),
      error = function(e) NULL
    )
    create_error_response("Internal Server Error", 500)
  })
}

# A response to HEAD has the status and headers of the response to GET, but no
# body. httpuv sends whatever body a response holds, and a client that reuses
# the connection would read those bytes as the start of the next response.
# An empty body is no answer either: httpuv compresses it for a client that
# accepts gzip and sends the result.
without_head_body <- function(req, response) {
  if (!identical(req$REQUEST_METHOD, "HEAD")) {
    return(response)
  }
  strip <- function(response) {
    discard_response_body(response$body)
    response["body"] <- list(NULL)
    response
  }
  if (promises::is.promise(response)) promises::then(response, strip) else strip(response)
}

# A request's headers under the names the client sent, in lower case. httpuv
# also stores every header as HTTP_<NAME> with "-" written as "_", where a
# header that a client spells with an underscore, such as "Content_Length" or
# "X_Forwarded_User", takes the place of the real one. Decisions about a
# request therefore read req$HEADERS; the HTTP_ entries serve only a request
# object that has none.
request_headers <- function(req) {
  headers <- req[["HEADERS"]]
  if (!is.null(headers)) {
    return(headers)
  }
  fields <- grep("^HTTP_", names(req), value = TRUE)
  headers <- vapply(fields, function(field) paste(req[[field]], collapse = ","), character(1))
  names(headers) <- tolower(gsub("_", "-", substring(fields, 6), fixed = TRUE))
  # httpuv copies these two from their HTTP_ entries
  for (field in c("CONTENT_LENGTH", "CONTENT_TYPE")) {
    name <- tolower(sub("_", "-", field, fixed = TRUE))
    if (!is.null(req[[field]]) && !name %in% names(headers)) {
      headers[[name]] <- paste(req[[field]], collapse = ",")
    }
  }
  headers
}

# A value from a request as it is written to the log. Control characters,
# with which a client could move the cursor of a terminal that follows the log
# or overwrite a line, become "?". That includes the C1 controls, such as
# U+009B, which some terminals read as the start of an escape sequence, and
# every byte that is not part of valid UTF-8, so the log stays valid UTF-8. A
# long value is cut short, between characters, at max_bytes bytes.
log_value <- function(value, max_bytes = 200) {
  if (is.null(value) || length(value) == 0) {
    return("(none)")
  }
  value <- as.character(value)[1]
  if (is.na(value)) {
    return("NA")
  }
  # The bytes are read as UTF-8 whatever encoding the string is marked with,
  # since a client sends bytes and not text in a declared encoding
  value <- rawToChar(charToRaw(value))
  Encoding(value) <- "UTF-8"
  value <- iconv(value, "UTF-8", "UTF-8", sub = "?")
  if (is.na(value)) {
    return("?")
  }
  value <- gsub("[\\x{01}-\\x{1F}\\x{7F}-\\x{9F}]", "?", value, perl = TRUE)
  if (nchar(value, type = "bytes") > max_bytes) {
    # No character is shorter than a byte, so the first max_bytes characters
    # hold every one that can be kept
    chars <- strsplit(substr(value, 1, max_bytes), "", fixed = TRUE)[[1]]
    kept <- cumsum(nchar(chars, type = "bytes")) <= max_bytes
    value <- paste0(paste(chars[kept], collapse = ""), "...")
  }
  value
}

request_header <- function(req, name) {
  "The value of one request header, named in lower case, or NULL when the request has none"

  headers <- request_headers(req)
  value <- headers[names(headers) == name]
  if (length(value) == 0) NULL else value[[1]]
}

# A forwarded client address as an IPv4 or IPv6 address, possibly in
# brackets or with a port, or NULL for any other value
as_ip_address <- function(value) {
  if (is.null(value) || length(value) != 1 || is.na(value) || !is_ascii(value)) {
    return(NULL)
  }
  value <- trimws(value)
  ipv4 <- "^([0-9]{1,3}\\.){3}[0-9]{1,3}(:[0-9]{1,5})?$"
  ipv6 <- "^\\[?[0-9A-Fa-f:.]*:[0-9A-Fa-f:.]*(%[0-9A-Za-z._-]+)?\\]?(:[0-9]{1,5})?$"
  if (nchar(value) <= 64 && (grepl(ipv4, value) || grepl(ipv6, value))) value else NULL
}

# Helper functions for connection tracking
get_client_ip <- function(req) {
  "Extract client IP address from request headers"

  remote_addr <- req$REMOTE_ADDR
  has_remote_addr <- !is.null(remote_addr) && remote_addr != ""

  # Forwarding headers are client-controlled, so trust them only when the
  # request arrives from a reverse proxy on this machine, and only for a
  # value that is an IP address: the address is shown on the dashboard and
  # written to the log, where other text could forge a line
  if (has_remote_addr && remote_addr %in% c("127.0.0.1", "::1", "::ffff:127.0.0.1")) {
    forwarded_for <- request_header(req, "x-forwarded-for")
    if (!is.null(forwarded_for) && forwarded_for != "") {
      # The last entry is the one added by the proxy itself
      ips <- strsplit(forwarded_for, ",", fixed = TRUE, useBytes = TRUE)[[1]]
      ip <- if (length(ips) > 0) as_ip_address(ips[length(ips)]) else NULL
      if (!is.null(ip)) {
        return(ip)
      }
    }

    real_ip <- as_ip_address(request_header(req, "x-real-ip"))
    if (!is.null(real_ip)) {
      return(real_ip)
    }
  }

  if (has_remote_addr) {
    return(remote_addr)
  }

  return("unknown")
}


# Session IDs only key the server's own connection tables. No request carries
# one or is looked up by one (the management API merely lists them), so they
# must be unique but need not be secret. A counter guarantees that without
# touching the caller's random number stream, as sample() would.
session_counter <- new.env(parent = emptyenv())
session_counter$last <- 0

generate_session_id <- function() {
  "Generate a session ID that no other connection in this R session has had"

  session_counter$last <- session_counter$last + 1
  sprintf("ws-%.0f", session_counter$last)
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

# A path as an absolute path, without resolving links as normalizePath()
# does. It need not exist.
absolute_path <- function(path) {
  path <- path.expand(path)
  if (!grepl("^([A-Za-z]:)?[/\\\\]", path)) {
    path <- file.path(getwd(), path)
  }
  path
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
  usage$uptime_seconds <- process_uptime_seconds(process)
  usage
}

# Seconds since a process started, or NULL when that cannot be read. The
# start time is read from the clock, so a clock set back since then, as after a
# virtual machine snapshot is restored, makes the time unknown: a negative
# uptime would count as a process that has only just started for as long as
# the clock was set back.
process_uptime_seconds <- function(process) {
  uptime <- tryCatch(
    as.numeric(difftime(Sys.time(), process$get_start_time(), units = "secs")),
    error = function(e) NULL
  )
  if (length(uptime) != 1 || is.na(uptime) || uptime < 0) {
    return(NULL)
  }
  uptime
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

create_method_not_allowed_response <- function(allowed = c("GET", "HEAD")) {
  "Create the HTTP 405 response of a route that serves only some methods"

  response <- create_error_response("Method Not Allowed", 405)
  response$headers[["Allow"]] <- paste(allowed, collapse = ", ")
  response
}

# The redirect is temporary: its target includes base_path, which a reload can
# change, and browsers keep a permanent redirect (301 or 308) without asking
# again, so they would go on to the old prefix's URL, which no longer exists.
create_redirect_response <- function(location, query_string = NULL) {
  "Create a temporary redirect that keeps the method, body, and query string"

  query <- query_string %||% ""
  if (query != "" && !startsWith(query, "?")) query <- paste0("?", query)
  list(status = 307L, headers = list(Location = paste0(location, query)), body = "")
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

is_port_in_use <- function(host, port, loopback = FALSE) {
  "Check if a port is in use (a server could not listen on it, or local clients would reach another program there)"

  # Listen on the port briefly, as an app will. Connecting to it instead
  # waits about a second for each free port on Windows, where a refused
  # loopback connection is retried, which held up startup and every reload.
  # A port that cannot be opened for another reason, such as a port below
  # 1024 without the right to use it or an address this machine lacks, is
  # also reported: no server could listen on it either.
  in_use <- tryCatch(
    {
      server <- httpuv::startServer(host, port, list(), quiet = TRUE)
      httpuv::stopServer(server)
      FALSE
    },
    error = function(e) TRUE
  )
  if (in_use) {
    return(TRUE)
  }
  # On Windows and macOS, a program that listens on every interface does not
  # keep an app from listening on 127.0.0.1 at the same port, and would then
  # lose its loopback connections to the app. Such a program accepts a
  # connection at once, so the wait for a free port is kept short. On Linux,
  # it keeps the app from listening, which the check above finds.
  #
  # The proxy and management servers are opened in a browser as
  # "localhost", which may reach either loopback address, so for them
  # (loopback = TRUE) a program on 127.0.0.1 or [::1] is found whatever
  # address the server listens on: listening on 127.0.0.1 does not keep a
  # program from [::1], nor, on Windows, does listening on every interface
  # keep one from 127.0.0.1. A machine without IPv6 refuses the [::1]
  # connection at once.
  addresses <- if (isTRUE(loopback)) {
    c("127.0.0.1", "::1")
  } else if (identical(host, "127.0.0.1") && !identical(Sys.info()[["sysname"]], "Linux")) {
    "127.0.0.1"
  } else {
    character()
  }
  for (address in addresses) {
    if (loopback_port_accepts(port, address = address)) {
      return(TRUE)
    }
  }
  FALSE
}

# Whether something accepts a TCP connection on a loopback address (127.0.0.1
# or ::1) at a port within timeout_ms milliseconds
loopback_port_accepts <- function(port, timeout_ms = 100, address = "127.0.0.1") {
  handle <- new_backend_handle(connect_only = TRUE, forbid_reuse = TRUE, connecttimeout_ms = timeout_ms)
  authority <- if (grepl(":", address, fixed = TRUE)) paste0("[", address, "]") else address
  tryCatch(
    {
      curl::curl_fetch_memory(paste0("http://", authority, ":", port, "/"), handle = handle)
      TRUE
    },
    error = function(e) FALSE
  )
}
