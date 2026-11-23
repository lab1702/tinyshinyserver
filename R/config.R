# Configuration Management Module
# Centralizes configuration loading, validation, and state management

# The fields a configuration file may contain. Any other field, such as a
# misspelled option, has no effect and is reported in the server log.
KNOWN_CONFIG_FIELDS <- c(
  "apps", "log_dir", "starting_port", "proxy_port", "proxy_host", "management_port",
  "restart_delay", "health_check_interval", "max_request_size_mb", "max_response_size_mb", "title",
  "base_path", "idle_timeout_minutes", "log_level", "max_unanswered_mb"
)
KNOWN_APP_FIELDS <- c("name", "path", "resident", "appstart_timeout")

# The values of the log_level field, from most to least detailed
LOG_LEVELS <- c("DEBUG", "INFO", "WARN", "ERROR")

# The longest restart_delay, health_check_interval, and appstart_timeout: one day
MAX_TIMER_SECONDS <- 86400

# The largest app response, in megabytes, that the proxy forwards by default
DEFAULT_MAX_RESPONSE_SIZE_MB <- 1024

# How many megabytes an app may send a WebSocket session, by default, without
# a message from the browser in between, before the session is closed
DEFAULT_MAX_UNANSWERED_MB <- 256

duplicated_config_fields <- function(config) {
  "Describe the fields that a configuration gives more than once, at its top level or in an app"

  repeated <- function(x) {
    if (!is.list(x) || is.null(names(x))) character() else unique(names(x)[duplicated(names(x))])
  }
  duplicates <- dQuote(repeated(config), FALSE)
  apps <- config[["apps"]]
  if (is.list(apps)) {
    for (i in seq_along(apps)) {
      extra <- repeated(apps[[i]])
      if (length(extra) > 0) {
        duplicates <- c(duplicates, paste(dQuote(extra, FALSE), "in app", i))
      }
    }
  }
  duplicates
}

unrecognized_config_fields <- function(config) {
  "Describe the fields of a validated configuration that the server does not use"

  ignored <- dQuote(setdiff(names(config), KNOWN_CONFIG_FIELDS), FALSE)
  for (app in config$apps) {
    extra <- setdiff(names(app), KNOWN_APP_FIELDS)
    if (length(extra) > 0) {
      ignored <- c(ignored, paste(dQuote(extra, FALSE), "in app", dQuote(app$name, FALSE)))
    }
  }
  ignored
}

# Every string of a parsed configuration file, names among them
config_strings <- function(value) {
  strings <- names(value)
  if (is.list(value)) {
    strings <- c(strings, unlist(lapply(value, config_strings), use.names = FALSE))
  } else if (is.character(value)) {
    strings <- c(strings, value)
  }
  as.character(strings)
}

half_surrogate_message <- function(file) {
  paste0("Configuration file ", file, " holds a \\u escape of half a UTF-16 surrogate pair, which is not a character")
}

# Configuration class to encapsulate global state
ShinyServerConfig <- setRefClass("ShinyServerConfig",
  fields = list(
    # Configuration data
    config = "list",
    config_file = "character", # Absolute path, read again on reload
    ignored_fields = "character", # Unrecognized fields in the file as last read

    # Runtime state
    app_processes = "list",
    ws_connections = "list",
    backend_connections = "list",
    # The session IDs of each app's open WebSocket connections. An app's
    # connection count is the size of its set, so a session that is added or
    # removed twice cannot be counted twice.
    app_sessions = "environment",
    # When each session last carried a message. Kept apart from the connection
    # lists so that a message does not rewrite them.
    session_activity = "environment",
    # The same in seconds of proc.time()'s elapsed clock, which decides when a
    # session is idle: a system clock set back or forward does not move it
    session_activity_clock = "environment",
    # The bytes that the app has sent each session since the browser's last
    # message on it, which the proxy may still hold for a browser that does
    # not read them; see max_unanswered_mb
    session_unanswered = "environment",
    active_http_requests = "environment",
    # When, in seconds of proc.time()'s elapsed clock, a slow browser may
    # still be receiving the last large response of each app
    http_transfer_ends = "environment",
    pending_session_checks = "environment",
    app_startup_state = "environment", # Track app startup progress (starting/ready)
    verified_backends = "environment", # Process generations confirmed to own their app port
    start_problems = "environment", # Recent app_start_problem() results, by app name
    # When, in seconds of proc.time()'s elapsed clock, each app's start
    # problem was last logged for a request; see note_start_problem()
    start_problems_logged = "environment",
    # When each app's process last exited without being stopped, until the app
    # next listens on its port. An on-demand app is started again only once
    # restart_delay has passed, as a resident app is.
    start_failures = "environment",
    # The document that each app's latest process serves, as detect_app_type()
    # found it
    app_documents = "environment",
    shutdown_requested = "logical", # Set by the management API; read by the event loop
    reload_requested = "logical", # Set by the management API; read by the event loop
    # Set when a callback that catches interrupts received one; read by the event loop
    interrupt_requested = "logical",
    last_reload = "list", # Outcome of the latest reload; empty while one is pending
    started_at = "ANY", # When the server was created, for the reported uptime

    # Constants. Limits on request paths, query strings, and methods are the
    # defaults of the validation functions in validation.R.
    MAX_PENDING_MESSAGES = "numeric",
    # The most bytes of browser messages that wait for an app's socket to open
    MAX_PENDING_BYTES = "numeric",
    CONNECTION_TIMEOUT_MINUTES = "numeric", # Default of the idle_timeout_minutes field
    CLEANUP_INTERVAL_SECONDS = "numeric",
    MAX_MESSAGE_SIZE = "numeric",
    APP_STARTUP_TIMEOUT_SECONDS = "numeric",
    HTTP_SESSION_GRACE_SECONDS = "numeric",
    # A slow browser's download rate, and the longest download time, that the
    # idle check of an on-demand app allows for after a large response
    HTTP_TRANSFER_BYTES_PER_SECOND = "numeric",
    HTTP_TRANSFER_MAX_SECONDS = "numeric",
    # How long a browser's WebSocket waits for a busy app to complete the
    # handshake of the proxy's own socket to it
    WS_BACKEND_CONNECT_SECONDS = "numeric",
    # How long an app may keep starting: the readiness checks stop looking for
    # it then, and the idle check stops an on-demand app that nobody is
    # waiting for
    ON_DEMAND_START_LIMIT_SECONDS = "numeric",
    # Longest time a finding of whether an app directory can be served is
    # reused, for changes that leave the directory's modification time alone
    START_PROBLEM_MAX_AGE_SECONDS = "numeric",
    # How often the reason that an on-demand app cannot start is logged at
    # WARN for the requests that ask for it
    START_PROBLEM_LOG_SECONDS = "numeric"
  ),
  methods = list(
    initialize = function() {
      # Initialize constants
      MAX_PENDING_MESSAGES <<- 100
      MAX_PENDING_BYTES <<- 16 * 1024^2
      CONNECTION_TIMEOUT_MINUTES <<- 30
      CLEANUP_INTERVAL_SECONDS <<- 300
      MAX_MESSAGE_SIZE <<- 1048576
      APP_STARTUP_TIMEOUT_SECONDS <<- 30
      HTTP_SESSION_GRACE_SECONDS <<- 30
      HTTP_TRANSFER_BYTES_PER_SECOND <<- 128 * 1024
      HTTP_TRANSFER_MAX_SECONDS <<- 600
      WS_BACKEND_CONNECT_SECONDS <<- 120
      ON_DEMAND_START_LIMIT_SECONDS <<- 600
      START_PROBLEM_MAX_AGE_SECONDS <<- 30
      START_PROBLEM_LOG_SECONDS <<- 60

      # Initialize runtime state
      app_processes <<- list()
      ws_connections <<- list()
      backend_connections <<- list()
      app_sessions <<- new.env(hash = TRUE, parent = emptyenv())
      session_activity <<- new.env(hash = TRUE, parent = emptyenv())
      session_activity_clock <<- new.env(hash = TRUE, parent = emptyenv())
      session_unanswered <<- new.env(hash = TRUE, parent = emptyenv())
      active_http_requests <<- new.env(hash = TRUE, parent = emptyenv())
      http_transfer_ends <<- new.env(hash = TRUE, parent = emptyenv())
      pending_session_checks <<- new.env(hash = TRUE, parent = emptyenv())
      app_startup_state <<- new.env(hash = TRUE, parent = emptyenv())
      verified_backends <<- new.env(hash = TRUE, parent = emptyenv())
      start_problems <<- new.env(hash = TRUE, parent = emptyenv())
      start_problems_logged <<- new.env(hash = TRUE, parent = emptyenv())
      start_failures <<- new.env(hash = TRUE, parent = emptyenv())
      app_documents <<- new.env(hash = TRUE, parent = emptyenv())
      shutdown_requested <<- FALSE
      reload_requested <<- FALSE
      interrupt_requested <<- FALSE
      last_reload <<- list()
      started_at <<- Sys.time()

      # Initialize empty config (to be loaded via load_config)
      config <<- list()
      config_file <<- NA_character_
      ignored_fields <<- character()
    },
    load_config = function(config_file = "config.json") {
      "Load and validate configuration from file"

      # Keep the absolute path so a reload finds the file after setwd().
      # Links are not resolved: a reload reads the file that the path names
      # then, such as a mounted Kubernetes ConfigMap, whose link is pointed
      # at a new file when it is updated.
      config_file <<- absolute_path(config_file)
      config <<- read_config(config_file)

      # Auto-assign ports to apps
      assign_app_ports()

      return(config)
    },
    read_config = function(file) {
      "Read, validate, and fill in defaults for a configuration file without applying it"

      if (!file.exists(file)) {
        stop("Configuration file not found: ", file)
      }
      if (dir.exists(file)) {
        stop("Configuration file ", file, " is a directory; name the configuration file in it", call. = FALSE)
      }
      if (isTRUE(file.size(file) == 0)) {
        stop("Configuration file ", file, " is empty", call. = FALSE)
      }

      # JSON is UTF-8 text. A file saved in another encoding, as Windows
      # PowerShell 5.1 saves one in UTF-16 by default, is named as such.
      start <- readBin(file, "raw", 2)
      if (identical(start, as.raw(c(0xff, 0xfe))) || identical(start, as.raw(c(0xfe, 0xff)))) {
        stop("Configuration file ", file, " is saved as UTF-16; save it as UTF-8", call. = FALSE)
      }
      # A NUL character would be read as the end of its line or value: R
      # drops the rest of a line after a NUL byte, and jsonlite ends a string
      # at "\u0000". The file would then be read as something other than what
      # it says, as "apps/a\u0000x" would name the directory "apps/a".
      if (any(readBin(file, "raw", file.size(file)) == as.raw(0))) {
        stop("Configuration file ", file, " contains a NUL byte; save it as UTF-8 text", call. = FALSE)
      }
      config_text <- paste(readLines(file, encoding = "UTF-8", warn = FALSE), collapse = "\n")
      if (!validUTF8(config_text)) {
        stop("Configuration file ", file, " is not valid UTF-8 text; save it as UTF-8", call. = FALSE)
      }
      # An escaped backslash before "u0000", as in "\\u0000", is not one
      if (grepl("(^|[^\\\\])(\\\\\\\\)*\\\\u0000", config_text, useBytes = TRUE)) {
        stop("Configuration file ", file, " contains the NUL character \\u0000, which is not allowed", call. = FALSE)
      }
      # A byte-order mark is allowed, as other JSON readers allow it.
      # jsonlite::fromJSON() would read text that is not JSON as the name of
      # a file or as a URL, and load that instead.
      config_text <- sub(paste0("^", intToUtf8(0xFEFF)), "", config_text)
      # A \u escape of half a UTF-16 surrogate pair is no character. jsonlite
      # reads a first half alone as "?", which would change a path or title
      # without notice, and a second half alone as bytes that are not UTF-8.
      lone_first_half <- "(^|[^\\\\])(\\\\\\\\)*\\\\u[dD][89abAB][0-9a-fA-F]{2}(?!\\\\u[dD][c-fC-F][0-9a-fA-F]{2})"
      if (grepl(lone_first_half, config_text, perl = TRUE)) {
        stop(half_surrogate_message(file), call. = FALSE)
      }
      parsed_config <- tryCatch(
        jsonlite::parse_json(config_text, simplifyVector = TRUE, simplifyDataFrame = FALSE),
        error = function(e) {
          stop("Configuration file ", file, " is not valid JSON: ", conditionMessage(e), call. = FALSE)
        }
      )
      if (!all(validUTF8(config_strings(parsed_config)))) {
        stop(half_surrogate_message(file), call. = FALSE)
      }

      # Validate configuration
      validation_result <- validate_config(parsed_config)
      if (!validation_result$valid) {
        stop("Configuration validation failed: ", validation_result$error)
      }

      new_config <- validation_result$sanitized

      # A misspelled option would otherwise be dropped without a trace. The
      # server log may not be open yet, so log_ignored_fields() reports these.
      ignored_fields <<- unrecognized_config_fields(parsed_config)

      # Remove those fields, so that they have no effect: `$` matches a list's
      # names by prefix, and "proxy_port_old" would answer for proxy_port.
      new_config <- new_config[intersect(names(new_config), KNOWN_CONFIG_FIELDS)]
      new_config$apps <- lapply(new_config$apps, function(app) {
        app[intersect(names(app), KNOWN_APP_FIELDS)]
      })

      # Set defaults for optional fields
      new_config$proxy_port <- new_config$proxy_port %||% 3838
      new_config$proxy_host <- new_config$proxy_host %||% "127.0.0.1"
      new_config$management_port <- new_config$management_port %||% 3839
      new_config$restart_delay <- new_config$restart_delay %||% 5
      new_config$health_check_interval <- new_config$health_check_interval %||% 10
      new_config$starting_port <- new_config$starting_port %||% 3001
      new_config$title <- trimws(new_config$title %||% DEFAULT_SERVER_TITLE)
      new_config$max_request_size_mb <- new_config$max_request_size_mb %||% 100
      new_config$max_response_size_mb <- new_config$max_response_size_mb %||% DEFAULT_MAX_RESPONSE_SIZE_MB
      new_config$base_path <- new_config$base_path %||% ""
      new_config$idle_timeout_minutes <- new_config$idle_timeout_minutes %||% CONNECTION_TIMEOUT_MINUTES
      new_config$max_unanswered_mb <- new_config$max_unanswered_mb %||% DEFAULT_MAX_UNANSWERED_MB
      new_config$log_level <- toupper(new_config$log_level %||% "INFO")

      # Set default values for optional app fields
      for (i in seq_along(new_config$apps)) {
        new_config$apps[[i]]$appstart_timeout <- new_config$apps[[i]]$appstart_timeout %||% 2
        if (!"resident" %in% names(new_config$apps[[i]])) {
          new_config$apps[[i]]$resident <- FALSE
        }
      }

      # Two apps cannot render one document into its directory
      shared_problem <- shared_document_problem(new_config$apps)
      if (!is.null(shared_problem)) {
        stop("Configuration validation failed: ", shared_problem)
      }

      return(new_config)
    },
    read_reload_config = function() {
      "Read the configuration file again, rejecting changes that need a server restart"

      if (is.na(config_file)) {
        return(list(valid = FALSE, error = "No configuration file was loaded"))
      }

      new_config <- tryCatch(read_config(config_file), error = function(e) e)
      if (inherits(new_config, "error")) {
        return(list(valid = FALSE, error = conditionMessage(new_config)))
      }

      # The proxy and management servers keep listening where they started
      loopback <- function(host) if (identical(host, "localhost")) "127.0.0.1" else host
      changed <- c(
        proxy_host = !identical(loopback(new_config$proxy_host), loopback(config$proxy_host)),
        proxy_port = !identical(as.numeric(new_config$proxy_port), as.numeric(config$proxy_port)),
        management_port = !identical(as.numeric(new_config$management_port), as.numeric(config$management_port))
      )
      if (any(changed)) {
        return(list(valid = FALSE, error = paste(
          paste(names(changed)[changed], collapse = ", "),
          "changed;",
          "restart the server to apply"
        )))
      }

      # An unwritable log_dir would lose the server log and the app logs. The
      # directory is checked even when the file names the same one, which
      # may have become unwritable since the server started: the apps that
      # the reload stops could not start again.
      log_dir_error <- check_log_dir(new_config$log_dir)
      if (!is.null(log_dir_error)) {
        return(list(valid = FALSE, error = log_dir_error))
      }

      # Ports are assigned once the apps have stopped. Look for them now, with
      # the running apps' ports taken to be free, so that apps that cannot
      # all be given a port, because other programs hold the ports after
      # starting_port, are reported before any app is stopped.
      port_error <- tryCatch(
        {
          find_app_ports(new_config, free_ports = running_app_ports())
          NULL
        },
        error = function(e) conditionMessage(e)
      )
      if (!is.null(port_error)) {
        return(list(valid = FALSE, error = port_error))
      }

      list(valid = TRUE, config = new_config)
    },
    reset_app_state = function(keep = character()) {
      "Forget per-app runtime state after apps have stopped, except that of the apps named in keep, which still run"

      # app_sessions needs no reset: closing each connection removed it, and
      # an app that is still running keeps its sessions. Such an app keeps
      # its state too: among it, the document
      # that its process serves, without which other documents of an R
      # Markdown app would be served.
      kept <- function(state) {
        fresh <- new.env(hash = TRUE, parent = emptyenv())
        for (app_name in intersect(keep, ls(state, all.names = TRUE))) {
          assign(app_name, get(app_name, envir = state), envir = fresh)
        }
        fresh
      }
      pending_session_checks <<- kept(pending_session_checks)
      app_startup_state <<- kept(app_startup_state)
      verified_backends <<- kept(verified_backends)
      start_problems <<- kept(start_problems)
      start_failures <<- kept(start_failures)
      app_documents <<- kept(app_documents)
    },
    set_app_document = function(app_name, app) {
      "Record the document that an app's new process serves"
      assign(app_name, app, envir = app_documents)
    },
    get_app_document = function(app_name) {
      "The document that an app's latest process serves, or NULL"
      app_documents[[app_name]]
    },
    note_start_failure = function(app_name) {
      "Record that an app's process exited without being stopped"
      assign(app_name, Sys.time(), envir = start_failures)
    },
    clear_start_failure = function(app_name) {
      "Forget an app's last process failure"
      if (exists(app_name, envir = start_failures, inherits = FALSE)) {
        rm(list = app_name, envir = start_failures)
      }
    },
    start_backoff_seconds = function(app_name) {
      "Seconds until an app whose process failed may be started on demand again, or 0"
      failed_at <- start_failures[[app_name]]
      if (is.null(failed_at)) {
        return(0)
      }
      delay <- config$restart_delay %||% 5
      elapsed <- as.numeric(difftime(Sys.time(), failed_at, units = "secs"))
      # R has no clock that cannot be set back. A clock set back since the
      # failure would make the wait last as long as it was set back, so a
      # wait that would be longer than restart_delay is over.
      if (elapsed < 0) {
        return(0)
      }
      max(0, delay - elapsed)
    },
    cached_start_problem = function(app_config) {
      "Why an app's directory cannot be served, or NULL, looked up again only once the directory changes"

      # Every open landing page asks for each dormant app's status every 5
      # seconds, on the thread that serves every app. Whether a directory can
      # be served depends only on the names it holds, and adding, removing,
      # or renaming one changes the directory's modification time on most
      # file systems. A directory read within 2 seconds of that time is read
      # again, for file systems that record times in whole seconds or
      # coarser. Some changes leave the time alone, such as a fixed
      # permission or a file added on a FAT or exFAT drive, so no finding is
      # used for longer than START_PROBLEM_MAX_AGE_SECONDS.
      read_at <- Sys.time()
      path <- normalizePath(app_config$path, mustWork = FALSE)
      modified <- file.mtime(path)
      cached <- start_problems[[app_config$name]]
      # A finding from a time after now, which a clock set back gives, is as
      # old as the clock was set back
      fresh <- function(cached) {
        age <- as.numeric(difftime(read_at, cached$read_at, units = "secs"))
        age >= 0 && age < START_PROBLEM_MAX_AGE_SECONDS
      }
      if (!is.null(cached) && !is.na(modified) && identical(cached$path, path) &&
        identical(cached$modified, modified) &&
        as.numeric(difftime(cached$read_at, modified, units = "secs")) > 2 && fresh(cached)) {
        return(cached$problem)
      }
      problem <- app_start_problem(app_config)
      assign(app_config$name, list(
        path = path, modified = modified, read_at = read_at, problem = problem
      ), envir = start_problems)
      problem
    },
    note_start_problem = function(app_name, problem) {
      "Log why an on-demand app that a request asked for cannot start: at WARN once every START_PROBLEM_LOG_SECONDS for each app, otherwise at DEBUG"

      # A page, in any visitor's browser and on any website, can ask for such
      # an app in a loop, which would otherwise fill the server log
      now <- proc.time()[["elapsed"]]
      logged_at <- start_problems_logged[[app_name]]
      if (!is.null(logged_at) && now >= logged_at && now - logged_at < START_PROBLEM_LOG_SECONDS) {
        logger::log_debug("On-demand app {app_name} cannot start: {problem}", app_name = app_name, problem = problem)
        return(invisible(FALSE))
      }
      assign(app_name, now, envir = start_problems_logged)
      logger::log_warn("On-demand app {app_name} cannot start: {problem}", app_name = app_name, problem = problem)
      invisible(TRUE)
    },
    validate_config = function(config) {
      "Validate configuration structure and values"

      if (!is.list(config)) {
        return(list(valid = FALSE, error = "Config must be a list"))
      }

      # jsonlite keeps both copies of a field given twice, and every lookup
      # finds the first, where other JSON readers use the last. Neither
      # choice is what the file's author can be assumed to mean.
      duplicates <- duplicated_config_fields(config)
      if (length(duplicates) > 0) {
        return(list(valid = FALSE, error = paste("Duplicate fields:", paste(duplicates, collapse = ", "))))
      }

      # Validate required fields
      required_fields <- c("apps", "log_dir", "starting_port")
      for (field in required_fields) {
        if (!field %in% names(config)) {
          return(list(valid = FALSE, error = paste("Missing required field:", field)))
        }
      }

      # Validate all configured ports before using them in allocation arithmetic.
      for (field in intersect(c("starting_port", "proxy_port", "management_port"), names(config))) {
        port_validation <- validate_port(config[[field]])
        if (!port_validation$valid) {
          return(list(valid = FALSE, error = paste0("Invalid ", field, ": ", port_validation$error)))
        }
      }

      # Validate timer values before they reach the event-loop scheduler. A
      # timer of 1e12 seconds or more makes later::run_now() block, and with
      # it every request, so they are also bounded above.
      for (field in intersect(c("restart_delay", "health_check_interval"), names(config))) {
        value <- config[[field]]
        positive <- field == "health_check_interval"
        if (!is.numeric(value) || length(value) != 1 || !is.finite(value) ||
          value < 0 || (positive && value == 0) || value > MAX_TIMER_SECONDS) {
          bound <- if (positive) "positive" else "non-negative"
          return(list(valid = FALSE, error = paste(
            field, "must be a single", bound, "number of seconds, at most", MAX_TIMER_SECONDS
          )))
        }
      }

      for (field in intersect(c("max_request_size_mb", "max_response_size_mb"), names(config))) {
        value <- config[[field]]
        if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value <= 0) {
          return(list(valid = FALSE, error = paste(field, "must be a single positive finite number of megabytes")))
        }
      }

      # Zero keeps idle sessions open
      if ("idle_timeout_minutes" %in% names(config)) {
        value <- config$idle_timeout_minutes
        if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value < 0) {
          return(list(valid = FALSE, error = "idle_timeout_minutes must be a single non-negative finite number of minutes"))
        }
      }

      # Zero lets an app send a session any amount without a browser message
      if ("max_unanswered_mb" %in% names(config)) {
        value <- config$max_unanswered_mb
        if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value < 0) {
          return(list(valid = FALSE, error = "max_unanswered_mb must be a single non-negative finite number of megabytes"))
        }
      }

      if ("log_level" %in% names(config)) {
        level <- config$log_level
        if (!is.character(level) || length(level) != 1 || is.na(level) || !toupper(level) %in% LOG_LEVELS) {
          return(list(valid = FALSE, error = paste("log_level must be one of:", paste(LOG_LEVELS, collapse = ", "))))
        }
      }

      # Check if there's enough port range for all apps. `[[` matches the
      # optional names exactly, where `$` would accept "proxy_port_old".
      num_apps <- length(config$apps)
      reserved_ports <- unique(c(config[["proxy_port"]] %||% 3838, config[["management_port"]] %||% 3839))
      reserved_ports <- reserved_ports[reserved_ports >= config$starting_port & reserved_ports <= 65535]

      # Only reserved ports in the remaining allocation range consume capacity.
      max_possible_port <- config$starting_port + num_apps - 1 + length(reserved_ports)
      if (max_possible_port > 65535) {
        return(list(valid = FALSE, error = sprintf(
          "Not enough port range: starting_port %d with %d apps could exceed port 65535",
          config$starting_port, num_apps
        )))
      }

      # Validate apps array
      if (!is.list(config$apps) || length(config$apps) == 0) {
        return(list(valid = FALSE, error = "Apps must be a non-empty list"))
      }

      app_names <- character()
      # Validate each app configuration
      for (i in seq_along(config$apps)) {
        app <- config$apps[[i]]

        if (!is.list(app)) {
          return(list(valid = FALSE, error = paste("App", i, "must be a list")))
        }

        # Check required app fields (port is no longer required)
        app_required <- c("name", "path")
        for (field in app_required) {
          if (!field %in% names(app)) {
            return(list(valid = FALSE, error = paste("App", i, "missing field:", field)))
          }
        }

        if ("appstart_timeout" %in% names(app)) {
          if (!is.numeric(app$appstart_timeout) || length(app$appstart_timeout) != 1 ||
            !is.finite(app$appstart_timeout) || app$appstart_timeout <= 0 ||
            app$appstart_timeout > MAX_TIMER_SECONDS) {
            return(list(valid = FALSE, error = paste(
              "App", i, "appstart_timeout must be a single positive number of seconds, at most", MAX_TIMER_SECONDS
            )))
          }
        }

        # Validate optional resident field
        if ("resident" %in% names(app)) {
          if (!is.logical(app$resident) || length(app$resident) != 1 || is.na(app$resident)) {
            return(list(valid = FALSE, error = paste("App", i, "resident field must be a single logical value (TRUE/FALSE)")))
          }
        }

        # The rules that request routing applies to the name in a URL
        name_validation <- validate_app_name(app$name)
        if (!name_validation$valid) {
          return(list(valid = FALSE, error = paste0("App ", i, ": ", name_validation$error)))
        }

        # Names that differ only in case would share log files on
        # case-insensitive file systems (Windows, macOS)
        if (tolower(app$name) %in% tolower(app_names)) {
          return(list(valid = FALSE, error = paste(
            "Duplicate app name:", app$name, "(names must differ by more than letter case)"
          )))
        }
        app_names <- c(app_names, app$name)

        # Validate path
        if (!is.character(app$path) || length(app$path) != 1) {
          return(list(valid = FALSE, error = paste("App", i, "path must be a string")))
        }
        if (is.na(app$path) || !nzchar(trimws(app$path))) {
          return(list(valid = FALSE, error = paste("App", i, "path must not be empty")))
        }
      }

      # Validate optional fields
      if ("proxy_host" %in% names(config)) {
        if (!is.character(config$proxy_host) || length(config$proxy_host) != 1) {
          return(list(valid = FALSE, error = "proxy_host must be a string"))
        }

        # Allow common host values
        allowed_hosts <- c("localhost", "127.0.0.1", "0.0.0.0", "::1", "::")
        if (!config$proxy_host %in% allowed_hosts) {
          return(list(valid = FALSE, error = paste("proxy_host must be one of:", paste(allowed_hosts, collapse = ", "))))
        }
      }

      # The title is shown on the landing and management pages
      if ("title" %in% names(config)) {
        title <- config$title
        if (!is.character(title) || length(title) != 1 || is.na(title) ||
          !nzchar(trimws(title)) || nchar(trimws(title)) > 100) {
          return(list(valid = FALSE, error = "title must be a non-empty string of at most 100 characters"))
        }
      }

      if ("base_path" %in% names(config)) {
        base_path_validation <- validate_base_path(config$base_path)
        if (!base_path_validation$valid) {
          return(list(valid = FALSE, error = base_path_validation$error))
        }
      }

      # Validate log_dir
      if (!is.character(config$log_dir) || length(config$log_dir) != 1 ||
        is.na(config$log_dir) || !nzchar(trimws(config$log_dir))) {
        return(list(valid = FALSE, error = "log_dir must be a non-empty string"))
      }

      sanitized <- config
      if ("base_path" %in% names(config)) {
        sanitized$base_path <- validate_base_path(config$base_path)$sanitized
      }
      return(list(valid = TRUE, sanitized = sanitized))
    },
    get_app_config = function(app_name) {
      "Get configuration for a specific app by name"

      for (app_config in config$apps) {
        if (app_config$name == app_name) {
          return(app_config)
        }
      }
      return(NULL)
    },
    get_sorted_apps = function() {
      "Get apps sorted alphabetically by name"

      sort_apps_by_name(config$apps)
    },
    get_proxy_host = function() {
      "Get proxy host, converting localhost to 127.0.0.1 for httpuv compatibility"

      host <- config$proxy_host %||% "127.0.0.1"
      if (host == "localhost") {
        host <- "127.0.0.1"
      }
      return(host)
    },

    # State management methods
    add_app_process = function(app_name, process) {
      "Add a process to the tracking list"
      app_processes[[app_name]] <<- process
    },
    remove_app_process = function(app_name) {
      "Remove a process from tracking"
      app_processes[[app_name]] <<- NULL
      # Startup state belongs to the removed process generation.
      set_app_ready(app_name)
      if (exists(app_name, envir = verified_backends, inherits = FALSE)) {
        rm(list = app_name, envir = verified_backends)
      }
    },
    get_app_process = function(app_name) {
      "Get a tracked process by app name"
      return(app_processes[[app_name]])
    },
    app_backend_verified = function(app_name) {
      "Whether the app's live tracked process is the listener on its port"
      process <- get_app_process(app_name)
      port <- get_app_config(app_name)$port
      if (is.null(port) || !is_process_alive(process)) return(FALSE)
      if (identical(verified_backends[[app_name]], process)) return(TRUE)
      if (!process_owns_port(process, port)) return(FALSE)
      assign(app_name, process, envir = verified_backends)
      # An app that listens has started, whatever happened to its last process
      clear_start_failure(app_name)
      TRUE
    },
    add_ws_connection = function(session_id, connection_info) {
      "Track a WebSocket connection. Adding a session again replaces its details."

      if (is.null(session_id) || is.null(connection_info)) {
        logger::log_error("add_ws_connection called with NULL session_id or connection_info")
        return(FALSE)
      }

      # A session belongs to one app at a time
      forget_app_session(session_id)
      ws_connections[[session_id]] <<- connection_info
      app_name <- connection_info$app_name
      if (!is.null(app_name) && nzchar(app_name)) {
        assign(app_name, union(app_sessions[[app_name]], session_id), envir = app_sessions)
        logger::log_debug("Session {session_id} tracked for {app_name}: {count} open",
          session_id = session_id, app_name = app_name, count = get_app_connection_count(app_name)
        )
      }

      return(TRUE)
    },
    remove_ws_connection = function(session_id) {
      "Stop tracking a WebSocket connection. Returns FALSE for a session that is not tracked."

      if (is.null(session_id)) {
        logger::log_error("remove_ws_connection called with NULL session_id")
        return(FALSE)
      }

      for (state in list(session_activity, session_activity_clock, session_unanswered)) {
        if (exists(session_id, envir = state, inherits = FALSE)) {
          rm(list = session_id, envir = state)
        }
      }

      # Close callbacks may ask again for a session that has already gone
      if (!(session_id %in% names(ws_connections))) {
        return(FALSE)
      }

      forget_app_session(session_id)
      ws_connections[[session_id]] <<- NULL
      return(TRUE)
    },
    forget_app_session = function(session_id) {
      "Take a tracked session out of its app's set of open connections"

      app_name <- ws_connections[[session_id]]$app_name
      if (is.null(app_name) || !nzchar(app_name)) {
        return(invisible(NULL))
      }
      remaining <- setdiff(app_sessions[[app_name]], session_id)
      logger::log_debug("Session {session_id} no longer tracked for {app_name}: {count} open",
        session_id = session_id, app_name = app_name, count = length(remaining)
      )
      if (length(remaining) > 0) {
        assign(app_name, remaining, envir = app_sessions)
      } else if (exists(app_name, envir = app_sessions, inherits = FALSE)) {
        rm(list = app_name, envir = app_sessions)
      }
    },
    get_ws_connection = function(session_id) {
      "Get WebSocket connection info by session ID"
      return(ws_connections[[session_id]])
    },
    touch_session = function(session_id) {
      "Record that a session carried a message, in either direction"
      assign(session_id, Sys.time(), envir = session_activity)
      assign(session_id, proc.time()[["elapsed"]], envir = session_activity_clock)
    },
    session_idle_seconds = function(session_id, connection) {
      "How long a session has carried no message, or NULL for a record that does not say when it opened"
      clock <- session_activity_clock[[session_id]] %||% connection$opened_clock
      if (!is.null(clock)) {
        return(proc.time()[["elapsed"]] - clock)
      }
      # A record without the elapsed clock goes by the system clock
      last_activity <- get_session_activity(session_id, connection$last_activity)
      if (is.null(last_activity)) {
        return(NULL)
      }
      max(0, as.numeric(difftime(Sys.time(), last_activity, units = "secs")))
    },
    get_session_activity = function(session_id, default = NULL) {
      "When a session last carried a message, or a default for one that has carried none"
      if (exists(session_id, envir = session_activity, inherits = FALSE)) {
        return(get(session_id, envir = session_activity))
      }
      default
    },
    add_unanswered_bytes = function(session_id, bytes) {
      "Count bytes that an app sent a session, returning the total since the browser's last message"
      total <- (session_unanswered[[session_id]] %||% 0) + bytes
      assign(session_id, total, envir = session_unanswered)
      total
    },
    clear_unanswered_bytes = function(session_id) {
      "Record that the browser sent a message on a session"
      if (exists(session_id, envir = session_unanswered, inherits = FALSE)) {
        rm(list = session_id, envir = session_unanswered)
      }
    },
    add_backend_connection = function(session_id, connection_info) {
      "Add a backend connection to tracking"
      backend_connections[[session_id]] <<- connection_info
    },
    remove_backend_connection = function(session_id) {
      "Remove a backend connection from tracking"
      backend_connections[[session_id]] <<- NULL
    },
    get_backend_connection = function(session_id) {
      "Get backend connection info by session ID"
      return(backend_connections[[session_id]])
    },
    get_all_ws_connections = function() {
      "Get all WebSocket connections"
      return(ws_connections)
    },
    get_all_backend_connections = function() {
      "Get all backend connections"
      return(backend_connections)
    },
    get_all_app_processes = function() {
      "Get all app processes"
      return(app_processes)
    },
    get_app_connection_count = function(app_name) {
      "The number of open WebSocket connections for an app"
      length(app_sessions[[app_name]])
    },
    assign_app_ports = function() {
      "Auto-assign ports to apps starting from starting_port"

      ports <- find_app_ports(config)
      for (i in seq_along(config$apps)) {
        config$apps[[i]]$port <<- ports[[i]]
        logger::log_debug("Assigned port {port} to app '{app_name}'",
          port = ports[[i]], app_name = config$apps[[i]]$name
        )
      }

      # Validate no port conflicts
      validate_port_assignments()
    },
    running_app_ports = function() {
      "The ports of the apps whose processes are running, which they free when they stop"
      ports <- vapply(names(get_all_app_processes()), function(app_name) {
        as.numeric(get_app_config(app_name)$port %||% NA)
      }, numeric(1))
      ports[!is.na(ports)]
    },
    validate_port_assignments = function() {
      "Validate that all assigned ports are unique and don't conflict"

      all_ports <- c()

      # Collect all app ports
      for (app in config$apps) {
        if (!is.null(app$port)) {
          all_ports <- c(all_ports, app$port)
        }
      }

      # Add system ports
      all_ports <- c(all_ports, config$proxy_port, config$management_port)

      # Check for duplicates
      if (length(all_ports) != length(unique(all_ports))) {
        duplicates <- all_ports[duplicated(all_ports)]

        # Find which apps/services are using duplicate ports
        port_usage <- list()
        for (app in config$apps) {
          if (app$port %in% duplicates) {
            port_usage[[as.character(app$port)]] <- c(port_usage[[as.character(app$port)]], paste("app", app$name))
          }
        }
        if (config$proxy_port %in% duplicates) {
          port_usage[[as.character(config$proxy_port)]] <- c(port_usage[[as.character(config$proxy_port)]], "proxy")
        }
        if (config$management_port %in% duplicates) {
          port_usage[[as.character(config$management_port)]] <- c(port_usage[[as.character(config$management_port)]], "management")
        }

        conflict_details <- sapply(names(port_usage), function(port) {
          sprintf("port %s used by: %s", port, paste(port_usage[[port]], collapse = " and "))
        })

        stop(sprintf(
          "Port conflict detected! %s",
          paste(conflict_details, collapse = "; ")
        ))
      }
    },
    log_port_assignments = function() {
      "Log the assigned ports (after logging is set up, so they reach server.log)"
      logger::log_info("Port assignments:")
      for (app in config$apps) {
        logger::log_info("  App '{name}' -> port {port}", name = app$name, port = app$port)
      }
      logger::log_info("  Proxy -> port {port}", port = config$proxy_port)
      logger::log_info("  Management -> port {port}", port = config$management_port)
    },
    log_ignored_fields = function() {
      "Log the configuration fields that have no effect (after logging is set up)"
      if (length(ignored_fields) > 0) {
        logger::log_warn("Ignoring unrecognized configuration fields: {fields}",
          fields = paste(ignored_fields, collapse = ", ")
        )
      }
    },
    log_unusable_apps = function() {
      "Warn about on-demand apps that could not start (after logging is set up)"

      # A resident app reports the same problem when it is started, but an
      # on-demand app would only show it to its first visitor
      for (app in config$apps) {
        if (isTRUE(app$resident)) next
        problem <- app_start_problem(app)
        if (!is.null(problem)) {
          logger::log_warn("On-demand app {name} cannot start: {problem}", name = app$name, problem = problem)
        }
      }
    },

    # App startup state tracking methods
    set_app_starting = function(app_name) {
      "Mark an app as starting up"
      assign(app_name, list(
        state = "starting",
        started_at = Sys.time()
      ), envir = app_startup_state)
    },
    set_app_ready = function(app_name) {
      "Mark an app as ready to receive requests"
      if (exists(app_name, envir = app_startup_state)) {
        rm(list = app_name, envir = app_startup_state)
      }
    },
    set_app_start_timed_out = function(app_name) {
      "Mark an app that is still starting as having taken too long"
      if (exists(app_name, envir = app_startup_state)) {
        startup_info <- get(app_name, envir = app_startup_state)
        startup_info$state <- "timeout"
        assign(app_name, startup_info, envir = app_startup_state)
      }
    },
    get_app_startup_state = function(app_name) {
      "Get the startup state of an app (NULL if ready, list if starting or timed out)"
      if (exists(app_name, envir = app_startup_state)) {
        startup_info <- get(app_name, envir = app_startup_state)

        # A startup has timed out when the readiness checks gave up on it, or
        # when its time is up. Reading the state leaves it in place:
        # forward_request() reports the timeout and ends it, so a WebSocket
        # handshake that asks first cannot use it up.
        elapsed <- as.numeric(difftime(Sys.time(), startup_info$started_at,
          units = "secs"
        ))
        # R has no clock that cannot be set back. A clock set back during the
        # startup would keep it from timing out, and keep its requests
        # waiting, for as long as it was set back, so its window starts again.
        if (elapsed < 0) {
          startup_info$started_at <- Sys.time()
          assign(app_name, startup_info, envir = app_startup_state)
          elapsed <- 0
        }
        app_config <- get_app_config(app_name)
        startup_timeout <- max(APP_STARTUP_TIMEOUT_SECONDS, app_config$appstart_timeout %||% 0)
        if (identical(startup_info$state, "timeout") || elapsed > startup_timeout) {
          return(list(state = "timeout", elapsed = elapsed))
        }

        return(c(startup_info, list(elapsed = elapsed)))
      }
      return(NULL)
    },
    is_app_starting = function(app_name) {
      "Check if an app is currently starting up"
      startup_state <- get_app_startup_state(app_name)
      return(!is.null(startup_state) && startup_state$state == "starting")
    }
  )
)

# The port of each app in settings, counted up from starting_port past the
# proxy and management ports and past ports that another program holds. The
# ports in free_ports are taken to be free, as those of apps that are about to
# stop. Stops with an error when the apps' ports cannot all be found.
find_app_ports <- function(settings, free_ports = numeric()) {
  reserved <- c(settings$proxy_port, settings$management_port)
  current_port <- settings$starting_port
  max_attempts <- 1000 # Prevent infinite loops when many ports are in use
  ports <- numeric(length(settings$apps))

  for (i in seq_along(settings$apps)) {
    attempts <- 0
    port_found <- FALSE

    while (!port_found && attempts < max_attempts) {
      if (current_port %in% reserved) {
        current_port <- current_port + 1
        attempts <- attempts + 1
      } else if (current_port %in% free_ports || !is_port_in_use("127.0.0.1", current_port)) {
        port_found <- TRUE
      } else {
        logger::log_debug("Port {port} is in use by another process, trying next port",
          port = current_port
        )
        current_port <- current_port + 1
        attempts <- attempts + 1
      }

      if (current_port > 65535) {
        stop(sprintf(
          "Port assignment failed: exceeded maximum port 65535 while assigning port for app '%s'",
          settings$apps[[i]]$name
        ))
      }
    }

    if (!port_found) {
      stop(sprintf(
        "Port assignment failed: could not find available port for app '%s' after %d attempts",
        settings$apps[[i]]$name, max_attempts
      ))
    }

    ports[[i]] <- current_port
    current_port <- current_port + 1
  }

  ports
}

# Create global configuration instance
# This will be used by other modules
create_server_config <- function(config_file = "config.json") {
  config_instance <- ShinyServerConfig$new()
  config_instance$load_config(config_file)
  return(config_instance)
}
