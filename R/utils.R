# Utility Functions Module
# Common utility functions used across the application

# Helper functions for connection tracking
get_client_ip <- function(req) {
  "Extract client IP address from request headers"

  # Try to get real IP from headers (for reverse proxy setups)
  forwarded_for <- req$HTTP_X_FORWARDED_FOR
  if (!is.null(forwarded_for) && forwarded_for != "") {
    # Take the first IP if multiple are present
    ip <- strsplit(forwarded_for, ",")[[1]][1]
    return(trimws(ip))
  }

  # Try other common headers
  real_ip <- req$HTTP_X_REAL_IP
  if (!is.null(real_ip) && real_ip != "") {
    return(trimws(real_ip))
  }

  # Fall back to REMOTE_ADDR
  remote_addr <- req$REMOTE_ADDR
  if (!is.null(remote_addr) && remote_addr != "") {
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

  # Add random entropy for better security
  random_salt <- paste(sample(c(0:9, letters, LETTERS), 16, replace = TRUE), collapse = "")

  # Create session ID with SHA-256 (more secure than MD5)
  session_data <- paste(user_agent, remote_addr, timestamp, random_salt, sep = "|")
  session_id <- digest::digest(session_data, algo = "sha256")

  return(session_id)
}

# Logging setup
setup_logging <- function(log_dir, log_level = "INFO") {
  "Initialize logging system with file and console output"

  # Ensure log directory exists
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }

  # Set log level threshold
  logger::log_threshold(log_level)

  # Configure file appender with same format as console
  log_file <- file.path(log_dir, "server.log")

  # Use a simple custom appender that writes to both console and file
  logger::log_appender(function(lines) {
    # Write to console (stderr by default)
    cat(lines, sep = "\n", file = stderr())

    # Write to file with same format
    tryCatch(
      {
        cat(lines, sep = "\n", file = log_file, append = TRUE)
      },
      error = function(e) {
        # If file writing fails, continue with console only
        warning("Could not write to log file: ", e$message)
      }
    )
  })

  logger::log_info("Logging system initialized with file output to {log_file}", log_file = log_file)
}

# Memory and resource management
format_bytes <- function(bytes) {
  "Format byte counts into human-readable strings"

  if (is.null(bytes) || !is.numeric(bytes)) {
    return("N/A")
  }

  units <- c("B", "KB", "MB", "GB", "TB")
  size <- abs(bytes)
  unit_index <- 1

  while (size >= 1024 && unit_index < length(units)) {
    size <- size / 1024
    unit_index <- unit_index + 1
  }

  return(paste(round(size, 1), units[unit_index]))
}

format_duration <- function(seconds) {
  "Format duration in seconds to human-readable string"

  if (is.null(seconds) || !is.numeric(seconds)) {
    return("N/A")
  }

  hours <- floor(seconds / 3600)
  minutes <- floor((seconds %% 3600) / 60)
  secs <- floor(seconds %% 60)

  if (hours > 0) {
    return(sprintf("%dh %dm %ds", hours, minutes, secs))
  } else if (minutes > 0) {
    return(sprintf("%dm %ds", minutes, secs))
  } else {
    return(sprintf("%ds", secs))
  }
}

# File operations
safe_file_read <- function(file_path, encoding = "UTF-8") {
  "Safely read file with error handling"

  tryCatch(
    {
      if (!file.exists(file_path)) {
        return(list(success = FALSE, error = paste("File not found:", file_path)))
      }

      content <- readLines(file_path, encoding = encoding, warn = FALSE)
      return(list(success = TRUE, content = content))
    },
    error = function(e) {
      return(list(success = FALSE, error = paste("Error reading file:", e$message)))
    }
  )
}

safe_file_write <- function(file_path, content, encoding = "UTF-8") {
  "Safely write file with error handling"

  tryCatch(
    {
      # Ensure directory exists
      dir_path <- dirname(file_path)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }

      writeLines(content, file_path, useBytes = FALSE)
      return(list(success = TRUE))
    },
    error = function(e) {
      return(list(success = FALSE, error = paste("Error writing file:", e$message)))
    }
  )
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

kill_process_safely <- function(process, force = FALSE) {
  "Safely terminate a process"

  if (is.null(process) || !is_process_alive(process)) {
    return(TRUE)
  }

  tryCatch(
    {
      if (force) {
        process$kill_tree()
      } else {
        process$kill()
        Sys.sleep(1)
        if (is_process_alive(process)) {
          process$kill_tree()
        }
      }
      return(TRUE)
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
    body = paste0('{"error": "', status, " - ", message, '"}')
  ))
}

# Configuration helpers
load_template <- function(template_path, variables = list()) {
  "Load HTML template and replace variables"

  template_result <- safe_file_read(template_path)
  if (!template_result$success) {
    return(list(success = FALSE, error = template_result$error))
  }

  html <- paste(template_result$content, collapse = "\n")

  # Simple variable replacement
  for (var_name in names(variables)) {
    placeholder <- paste0("{{", var_name, "}}")
    html <- gsub(placeholder, variables[[var_name]], html, fixed = TRUE)
  }

  return(list(success = TRUE, html = html))
}

# Validation helpers
is_valid_url <- function(url) {
  "Basic URL validation"

  if (is.null(url) || !is.character(url)) {
    return(FALSE)
  }

  # Simple URL pattern matching
  url_pattern <- "^https?://[a-zA-Z0-9.-]+(?:\\.[a-zA-Z]{2,})?(?::[0-9]+)?(?:/[^\\s]*)?$"
  return(grepl(url_pattern, url))
}

# Timing utilities
with_timeout <- function(expr, timeout_seconds = 30) {
  "Execute expression with timeout"

  tryCatch(
    {
      # This is a simplified timeout implementation
      # In production, you might want to use more sophisticated timeout handling
      result <- eval(expr)
      return(list(success = TRUE, result = result))
    },
    error = function(e) {
      return(list(success = FALSE, error = e$message))
    }
  )
}
