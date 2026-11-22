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

  # Check for path traversal: ".." as a whole path segment. Names that only
  # start or end with two dots, such as "/..hidden" or "/notes../", are not
  # parent-directory references.
  if (grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)", path)) {
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

# The methods the proxy and the management server recognize at all. Routes
# may accept fewer of them.
ALLOWED_HTTP_METHODS <- c("GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS")

validate_http_method <- function(method, allowed_methods = ALLOWED_HTTP_METHODS) {
  "Validate HTTP request methods"

  if (is.null(method) || !is.character(method) || length(method) != 1) {
    return(list(valid = FALSE, error = "Invalid method type"))
  }

  # Method names are case-sensitive, so "get" is not GET
  if (!method %in% allowed_methods) {
    return(list(valid = FALSE, error = "HTTP method not supported"))
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

  # The query is forwarded to the app verbatim, so its percent-encoding is the
  # app's to interpret: a literal "%" as in "?discount=50%&x=1" is not rejected.
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
  # unreserved URL characters. The pattern covers the whole path, because
  # strsplit() drops a trailing empty segment such as the one in "/a//".
  segments <- strsplit(substring(base_path, 2), "/", fixed = TRUE)[[1]]
  if (!grepl("^(/[A-Za-z0-9._~-]+)+$", base_path) || any(segments %in% c(".", ".."))) {
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

  if (is.null(app_name) || !is.character(app_name) || length(app_name) != 1 || is.na(app_name)) {
    return(list(valid = FALSE, error = "App name must be a string"))
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

# Validation result helper
create_validation_error <- function(error_message, status_code = 400) {
  "Create a standardized validation error response"

  return(list(
    valid = FALSE,
    error = error_message,
    status_code = status_code,
    response = create_error_response(error_message, status_code)
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
  # A method that the server serves on no resource is answered with 501, which
  # needs no Allow header; a route that serves other methods answers 405
  method_result <- validate_http_method(method)
  if (!method_result$valid) {
    return(create_validation_error(paste("Invalid method:", method_result$error), 501))
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
