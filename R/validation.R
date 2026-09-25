# Input Validation Module
# Centralizes all input validation and sanitization functions

# Input validation functions
validate_path <- function(path, max_length = 1000) {
  "Validate and sanitize file paths"

  if (is.null(path) || !is.character(path) || length(path) != 1) {
    return(list(valid = FALSE, error = "Invalid path type"))
  }

  if (nchar(path) > max_length) {
    return(list(valid = FALSE, error = "Path too long"))
  }

  # Check for null bytes
  if (grepl("\\x00", path, useBytes = TRUE)) {
    return(list(valid = FALSE, error = "Path contains null bytes"))
  }

  # Check for path traversal attempts (only dangerous patterns)
  if (grepl("\\.\\.[\\/\\\\]", path) || grepl("[\\/\\\\]\\.\\.", path)) {
    return(list(valid = FALSE, error = "Path traversal detected"))
  }

  # Check for invalid characters (only reject very dangerous control chars)
  # ASCII 1-8: SOH, STX, ETX, EOT, ENQ, ACK, BEL, BS
  # ASCII 14-31: SO, SI, DLE, DC1-4, NAK, SYN, ETB, CAN, EM, SUB, ESC, FS, GS, RS, US
  # ASCII 127: DEL
  if (grepl("[\001-\010\016-\037\177]", path)) {
    return(list(valid = FALSE, error = "Path contains invalid characters"))
  }

  return(list(valid = TRUE, sanitized = path))
}

validate_http_method <- function(method, allowed_methods = c("GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS")) {
  "Validate HTTP request methods"

  if (is.null(method) || !is.character(method) || length(method) != 1) {
    return(list(valid = FALSE, error = "Invalid method type"))
  }

  method <- toupper(trimws(method))

  if (!method %in% allowed_methods) {
    return(list(valid = FALSE, error = "HTTP method not allowed"))
  }

  return(list(valid = TRUE, sanitized = method))
}

validate_query_string <- function(query_string, max_length = 8192) {
  "Validate URL query strings"

  if (is.null(query_string)) {
    return(list(valid = TRUE, sanitized = ""))
  }

  if (!is.character(query_string) || length(query_string) != 1) {
    return(list(valid = FALSE, error = "Invalid query string type"))
  }

  # httpuv includes the leading question mark, which does not count toward the limit.
  if (nchar(sub("^[?]", "", query_string)) > max_length) {
    return(list(valid = FALSE, error = "Query string too long"))
  }

  # Check for null bytes
  if (grepl("\\x00", query_string, useBytes = TRUE)) {
    return(list(valid = FALSE, error = "Query string contains null bytes"))
  }

  # Basic URL encoding validation
  if (grepl("%[^0-9A-Fa-f]", query_string) || grepl("%[0-9A-Fa-f][^0-9A-Fa-f]", query_string)) {
    return(list(valid = FALSE, error = "Invalid URL encoding"))
  }

  return(list(valid = TRUE, sanitized = query_string))
}

validate_ws_message <- function(message, max_size = 1048576) {
  "Validate WebSocket messages"

  if (is.null(message)) {
    return(list(valid = FALSE, error = "Message is null"))
  }

  # httpuv represents binary frames as raw vectors, including empty frames.
  if (is.raw(message)) {
    if (length(message) > max_size) {
      return(list(valid = FALSE, error = "Message too large"))
    }
    return(list(valid = TRUE, sanitized = message))
  }

  if (!is.character(message) || length(message) != 1) {
    return(list(valid = FALSE, error = "Invalid message type"))
  }

  if (nchar(message, type = "bytes") > max_size) {
    return(list(valid = FALSE, error = "Message too large"))
  }

  # Check for null bytes
  if (grepl("\\x00", message, useBytes = TRUE)) {
    return(list(valid = FALSE, error = "Message contains null bytes"))
  }

  return(list(valid = TRUE, sanitized = message))
}

# First path segments the proxy routes itself; a base path starting with one
# would make prefixed and unprefixed requests indistinguishable
RESERVED_BASE_PATH_SEGMENTS <- c("proxy", "api", "templates", "health")

validate_base_path <- function(base_path, max_length = 100) {
  "Validate the public URL prefix and normalize it to \"\" or \"/a/b\" without a trailing slash"

  if (!is.character(base_path) || length(base_path) != 1 || is.na(base_path)) {
    return(list(valid = FALSE, error = "base_path must be a string"))
  }
  if (base_path %in% c("", "/")) {
    return(list(valid = TRUE, sanitized = ""))
  }
  if (!startsWith(base_path, "/")) {
    return(list(valid = FALSE, error = "base_path must start with /"))
  }

  base_path <- sub("/$", "", base_path)
  if (nchar(base_path) > max_length) {
    return(list(valid = FALSE, error = paste("base_path must be at most", max_length, "characters")))
  }

  # The path is written into page scripts and cookie paths, so allow only
  # unreserved URL characters
  segments <- strsplit(substring(base_path, 2), "/", fixed = TRUE)[[1]]
  if (length(segments) == 0 || any(!grepl("^[A-Za-z0-9._~-]+$", segments)) ||
    any(segments %in% c(".", ".."))) {
    return(list(valid = FALSE, error = paste(
      "base_path segments may contain only letters, digits, and . _ ~ -,",
      "and may not be empty, . or .."
    )))
  }
  if (tolower(segments[1]) %in% RESERVED_BASE_PATH_SEGMENTS) {
    return(list(valid = FALSE, error = paste0(
      "base_path may not start with /", segments[1], ", which the server already uses"
    )))
  }

  list(valid = TRUE, sanitized = base_path)
}

validate_app_name <- function(app_name, max_length = 50) {
  "Validate application names"

  if (is.null(app_name) || !is.character(app_name) || length(app_name) != 1) {
    return(list(valid = FALSE, error = "Invalid app name type"))
  }

  # Allow only alphanumeric, hyphen, underscore
  if (!grepl("^[a-zA-Z0-9_-]+$", app_name)) {
    return(list(valid = FALSE, error = "App name contains invalid characters"))
  }

  if (nchar(app_name) > max_length) {
    return(list(valid = FALSE, error = "App name too long"))
  }

  return(list(valid = TRUE, sanitized = app_name))
}

validate_ip_address <- function(ip) {
  "Validate IP addresses"

  if (is.null(ip) || !is.character(ip) || length(ip) != 1) {
    return(list(valid = FALSE, error = "Invalid IP type"))
  }

  # Check for localhost and known safe IPs
  safe_ips <- c("127.0.0.1", "::1", "localhost", "unknown")
  if (ip %in% safe_ips) {
    return(list(valid = TRUE, sanitized = ip))
  }

  # Basic IPv4 validation
  ipv4_pattern <- "^(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"
  if (grepl(ipv4_pattern, ip)) {
    return(list(valid = TRUE, sanitized = ip))
  }

  # Basic IPv6 validation (simplified)
  ipv6_pattern <- "^([0-9a-fA-F]{1,4}:){7}[0-9a-fA-F]{1,4}$|^::1$|^::$"
  if (grepl(ipv6_pattern, ip)) {
    return(list(valid = TRUE, sanitized = ip))
  }

  return(list(valid = FALSE, error = "Invalid IP address format"))
}

validate_port <- function(port) {
  "Validate port numbers"

  if (is.null(port)) {
    return(list(valid = FALSE, error = "Port is null"))
  }

  if (!is.numeric(port) || length(port) != 1) {
    return(list(valid = FALSE, error = "Port must be a number"))
  }

  if (!is.finite(port) || port != floor(port)) {
    return(list(valid = FALSE, error = "Port must be a finite integer"))
  }

  if (port < 1 || port > 65535) {
    return(list(valid = FALSE, error = "Port must be between 1 and 65535"))
  }

  return(list(valid = TRUE, sanitized = as.integer(port)))
}

validate_session_id <- function(session_id) {
  "Validate session IDs"

  if (is.null(session_id) || !is.character(session_id) || length(session_id) != 1) {
    return(list(valid = FALSE, error = "Invalid session ID type"))
  }

  # Session IDs should be hexadecimal strings (SHA-256 output)
  if (!grepl("^[a-fA-F0-9]{64}$", session_id)) {
    return(list(valid = FALSE, error = "Invalid session ID format"))
  }

  return(list(valid = TRUE, sanitized = session_id))
}

validate_json_input <- function(json_string) {
  "Validate JSON input strings"

  if (is.null(json_string) || !is.character(json_string) || length(json_string) != 1) {
    return(list(valid = FALSE, error = "Invalid JSON input type"))
  }

  # Try to parse JSON
  tryCatch(
    {
      parsed <- jsonlite::fromJSON(json_string, simplifyDataFrame = FALSE)
      return(list(valid = TRUE, sanitized = json_string, parsed = parsed))
    },
    error = function(e) {
      return(list(valid = FALSE, error = paste("Invalid JSON:", e$message)))
    }
  )
}

html_escape <- function(text) {
  "Escape HTML characters for safe display"

  if (is.null(text) || !is.character(text)) {
    return("")
  }

  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub("\"", "&quot;", text)
  text <- gsub("'", "&#39;", text)

  return(text)
}

sanitize_for_logging <- function(text, max_length = 100) {
  "Sanitize text for safe logging"

  if (is.null(text) || !is.character(text)) {
    return("(null)")
  }

  # Truncate if too long
  if (nchar(text) > max_length) {
    text <- paste0(substring(text, 1, max_length), "...")
  }

  # Remove potentially dangerous characters
  text <- gsub("[\001-\037\177]", "?", text)

  return(text)
}

# Validation result helper
create_validation_error <- function(error_message, status_code = 400) {
  "Create a standardized validation error response"

  return(list(
    valid = FALSE,
    error = error_message,
    status_code = status_code,
    response = list(
      status = status_code,
      headers = list("Content-Type" = "application/json"),
      body = jsonlite::toJSON(list(error = paste(status_code, "-", error_message)), auto_unbox = TRUE)
    )
  ))
}

# Batch validation helper
validate_request_inputs <- function(path, method, query_string = NULL) {
  "Validate common HTTP request inputs in batch"

  # Validate path
  path_result <- validate_path(path)
  if (!path_result$valid) {
    return(create_validation_error(paste("Invalid path:", path_result$error)))
  }

  # Validate method
  method_result <- validate_http_method(method)
  if (!method_result$valid) {
    return(create_validation_error(paste("Invalid method:", method_result$error), 405))
  }

  # Validate query string if provided
  if (!is.null(query_string)) {
    query_result <- validate_query_string(query_string)
    if (!query_result$valid) {
      return(create_validation_error(paste("Invalid query:", query_result$error)))
    }
    query_string <- query_result$sanitized
  }

  return(list(
    valid = TRUE,
    path = path_result$sanitized,
    method = method_result$sanitized,
    query_string = query_string
  ))
}
