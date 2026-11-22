# Process Management Module
# Handles Shiny app process lifecycle, health monitoring, and cleanup

# What an app directory holds, in the order start_app() prefers: a standard
# Shiny app, then an R Markdown document, then a Quarto document. A directory
# that cannot be served is an error here, so that no process is started only
# to exit again.
detect_app_type <- function(app_path) {
  if (!dir.exists(app_path)) {
    stop("App directory not found: ", app_path, call. = FALSE)
  }

  has_app_r <- file.exists(file.path(app_path, "app.R"))
  has_ui_server <- file.exists(file.path(app_path, "ui.R")) && file.exists(file.path(app_path, "server.R"))
  if (has_app_r || has_ui_server) {
    return(list(type = "shiny"))
  }

  rmd_file <- pick_document(app_path, "Rmd")
  if (!is.null(rmd_file)) {
    return(list(type = "rmd", file = rmd_file))
  }
  qmd_file <- pick_document(app_path, "qmd")
  if (!is.null(qmd_file)) {
    return(list(type = "qmd", file = qmd_file))
  }

  # shiny::runApp() also accepts these names in any letter case, and a
  # server.R whose page is www/index.html
  if (any(tolower(list.files(app_path)) %in% c("app.r", "server.r"))) {
    return(list(type = "shiny"))
  }

  # A directory that holds only partials would otherwise seem to hold a document
  partials <- list.files(app_path, pattern = "^_.*\\.(Rmd|qmd)$", ignore.case = TRUE)
  stop("No app.R, ui.R and server.R, .Rmd, or .qmd file found in ", app_path,
    if (length(partials) > 0) {
      paste0(" (", paste(partials, collapse = ", "),
        " not served: a name that starts with \"_\" marks a partial that other documents include)")
    },
    call. = FALSE
  )
}

# The document that serves an app directory, or NULL when it has none. The
# extension may be in any letter case, as rmarkdown and Quarto accept it. As in
# rmarkdown::run(), a name that starts with "_" is a partial that other
# documents include, an index or ui document is preferred, and otherwise a
# document whose front matter makes it a Shiny document. Next comes the
# extension's usual letter case, so that a child document such as
# "appendix.rmd" does not displace "report.Rmd". Ties go to alphabetical order.
pick_document <- function(app_path, extension) {
  files <- list.files(app_path, pattern = paste0("\\.", extension, "$"), ignore.case = TRUE)
  files <- files[!startsWith(files, "_")]
  if (length(files) == 0) {
    return(NULL)
  }
  stem <- tolower(sub("\\.[^.]*$", "", files))
  preferred <- stem %in% c("index", "ui")
  # Front matter is read only when it decides the choice
  shiny <- rep(FALSE, length(files))
  if (length(files) > 1 && !any(preferred)) {
    shiny <- vapply(file.path(app_path, files), is_shiny_document, logical(1), USE.NAMES = FALSE)
  }
  usual_case <- endsWith(files, paste0(".", extension))
  file.path(app_path, files[order(!preferred, !shiny, !usual_case, seq_along(files))[1]])
}

# Whether a document's front matter asks for a Shiny runtime, as
# rmarkdown::run() decides it: "runtime: shiny" or another runtime whose name
# starts with "shiny", or Quarto's "server: shiny". Front matter that cannot be
# read counts as none.
is_shiny_document <- function(file) {
  yaml <- read_front_matter(file)
  runtime <- yaml[["runtime"]]
  server <- yaml[["server"]]
  if (is.list(server)) server <- server[["type"]]
  is_string <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
  (is_string(runtime) && startsWith(runtime, "shiny")) || (is_string(server) && identical(server, "shiny"))
}

# Whether rmarkdown::run() serves a document as a prerendered Shiny document,
# as it decides it: "runtime: shinyrmd" or "shiny_prerendered", or Quarto's
# "server: shiny"
is_prerendered_document <- function(file) {
  yaml <- read_front_matter(file)
  server <- yaml[["server"]]
  if (is.list(server)) server <- server[["type"]]
  identical(yaml[["runtime"]], "shinyrmd") || identical(yaml[["runtime"]], "shiny_prerendered") ||
    identical(server, "shiny")
}

# A document's front matter, or an empty list when it cannot be read
read_front_matter <- function(file) {
  yaml <- tryCatch(rmarkdown::yaml_front_matter(file), error = function(e) list())
  if (is.list(yaml)) yaml else list()
}

# The document that an app directory's app renders into that directory before
# serving it, or NULL. Quarto always renders there, and rmarkdown::run()
# renders a prerendered Shiny document there; other R Markdown documents are
# rendered into a temporary directory.
document_rendered_in_place <- function(app_path) {
  app <- tryCatch(detect_app_type(app_path), error = function(e) NULL)
  if (is.null(app) || identical(app$type, "shiny")) {
    return(NULL)
  }
  if (identical(app$type, "qmd") || is_prerendered_document(app$file)) app$file else NULL
}

# Why apps cannot share their directory, or NULL when none do. A document
# rendered in place is written by every process that renders it, and two
# renders at once leave a broken page, or none. The broken page is then
# newer than the document, so it is served at every start until deleted.
shared_document_problem <- function(apps) {
  # Paths that do not exist hold no document, however they are spelled
  paths <- vapply(apps, function(app) normalizePath(app$path, mustWork = FALSE), character(1))
  for (path in unique(paths[duplicated(paths)])) {
    document <- document_rendered_in_place(path)
    if (is.null(document)) next
    names <- vapply(apps[paths == path], function(app) app$name, character(1))
    listed <- if (length(names) == 2) {
      paste(names, collapse = " and ")
    } else {
      paste0(paste(names[-length(names)], collapse = ", "), ", and ", names[length(names)])
    }
    return(paste0(
      "Apps ", listed, " serve the same document, ", basename(document),
      ", which is rendered into its directory before it is served, and two renders at once",
      " can leave a broken page. Serve it from one app, or give each app its own copy of the directory"
    ))
  }
  NULL
}

# Why an app's directory cannot be served, or NULL when it can
app_start_problem <- function(app_config) {
  tryCatch(
    {
      detect_app_type(normalizePath(app_config$path, mustWork = FALSE))
      NULL
    },
    error = function(e) conditionMessage(e)
  )
}

# quarto_serve() renders a document before serving it, which takes many
# seconds on every start, including each start of an on-demand app. The
# rendered page stays valid while it is newer than every other source file in
# the app directory, so render only when it is missing or something has changed.
# The server's logs are not sources: when log_dir lies in the app directory,
# they change at every start. start_app() hands this function to the app's
# own process, which need not load this package, so it may call only base R.
quarto_needs_render <- function(qmd_file, log_dir = NULL) {
  # Quarto names the page "doc.html" for "doc.qmd" and "doc.QMD" alike
  output <- sub("\\.qmd$", ".html", qmd_file, ignore.case = TRUE)
  if (!file.exists(output)) {
    return(TRUE)
  }

  app_dir <- dirname(qmd_file)
  # Files that rendering itself writes
  generated <- c(basename(output), sub("\\.qmd$", "_files", basename(qmd_file), ignore.case = TRUE), ".quarto")
  sources <- list.files(app_dir, recursive = TRUE, all.files = TRUE)
  sources <- sources[!sub("/.*$", "", sources) %in% generated]
  # Files that change without the document changing: hidden files and
  # directories, such as .git, .Rproj.user, and .Rhistory, and the packages
  # that renv installs. A top-level .Rprofile or .Renviron does count, as it
  # configures the R session that renders the document.
  hidden <- grepl("(^|/)[.]", sources) & !sources %in% c(".Rprofile", ".Renviron")
  renv_owned <- grepl("^renv/(library|local|python|sandbox|staging)/", sources)
  sources <- sources[!hidden & !renv_owned]

  # The .log files directly in log_dir: server.log and every app's logs
  if (!is.null(log_dir)) {
    windows <- .Platform$OS.type == "windows"
    canonical <- function(path) {
      path <- sub("/$", "", normalizePath(path, winslash = "/", mustWork = FALSE))
      if (windows) tolower(path) else path
    }
    root <- canonical(app_dir)
    logs <- canonical(log_dir)
    if (identical(logs, root) || startsWith(logs, paste0(root, "/"))) {
      prefix <- if (identical(logs, root)) "" else paste0(substring(logs, nchar(root) + 2), "/")
      listed <- if (windows) tolower(sources) else sources
      in_logs <- startsWith(listed, prefix) & !grepl("/", substring(listed, nchar(prefix) + 1), fixed = TRUE)
      sources <- sources[!(in_logs & endsWith(listed, ".log"))]
    }
  }

  # A file as new as the page may have changed after it was rendered
  changed <- file.mtime(file.path(app_dir, sources)) >= file.mtime(output)
  any(is.na(changed) | changed)
}

# The user environment file that R reads when it starts in an app's directory:
# the file that R_ENVIRON_USER names, the directory's .Renviron, or
# ~/.Renviron. callr would otherwise copy the .Renviron of this session's
# working directory into every app.
app_environ_file <- function(app_path) {
  named <- Sys.getenv("R_ENVIRON_USER", "")
  if (nzchar(named)) {
    return(named)
  }
  local <- file.path(app_path, ".Renviron")
  if (file.exists(local)) local else path.expand("~/.Renviron")
}

# The user profile that R reads when it starts in the working directory: the
# file that R_PROFILE_USER names, the directory's .Rprofile, or ~/.Rprofile.
# start_app() hands this function to the app's own process, which need not
# load this package, so it may call only base R.
startup_profile_file <- function() {
  named <- Sys.getenv("R_PROFILE_USER", "")
  if (nzchar(named)) {
    return(path.expand(named))
  }
  if (file.exists(".Rprofile")) ".Rprofile" else path.expand("~/.Rprofile")
}

# Process Manager Class
ProcessManager <- setRefClass("ProcessManager",
  fields = list(
    config = "ANY",
    pending_restarts = "environment"
  ),
  methods = list(
    initialize = function(server_config) {
      config <<- server_config
      pending_restarts <<- new.env(parent = emptyenv())
    },
    start_app = function(app_config) {
      "Start a Shiny application process"

      app_name <- app_config$name
      assign(app_name, NULL, envir = pending_restarts)
      app_port <- app_config$port
      app_path <- normalizePath(app_config$path, mustWork = FALSE)

      # Fails here, with the reason, for a directory that cannot be served
      app <- detect_app_type(app_path)

      logger::log_info("Starting app {app_name} on port {app_port}", app_name = app_name, app_port = app_port)

      if (app$type == "qmd") {
        logger::log_info("Detected Quarto document for {app_name}, using quarto::quarto_serve", app_name = app_name)
      } else if (app$type == "rmd") {
        logger::log_info("Detected R Markdown app for {app_name}, using rmarkdown::run", app_name = app_name)
      } else {
        logger::log_info("Detected traditional Shiny app for {app_name}", app_name = app_name)
      }

      # Recreate log_dir if it was removed while the server runs; app
      # processes cannot start without their log files
      dir.create(config$config$log_dir, recursive = TRUE, showWarnings = FALSE)
      # The app process starts in its own directory, so it gets absolute paths
      log_dir <- normalizePath(config$config$log_dir, mustWork = FALSE)

      # Prepare log files, keeping the previous run's logs (e.g. a crash
      # traceback) for one restart
      output_log <- file.path(log_dir, paste0(app_name, "_output.log"))
      error_log <- file.path(log_dir, paste0(app_name, "_error.log"))
      for (log_file in c(output_log, error_log)) {
        if (file.exists(log_file)) {
          previous_log <- sub("\\.log$", ".prev.log", log_file)
          tryCatch(
            suppressWarnings({
              unlink(previous_log)
              file.rename(log_file, previous_log)
            }),
            error = function(e) FALSE
          )
        }
      }

      # Whether a Quarto document must be rendered is decided in the app's own
      # process: the check reads every file of the app directory, which would
      # hold up every app's traffic if it ran here. The function travels
      # without this package's namespace.
      needs_render <- quarto_needs_render
      environment(needs_render) <- baseenv()
      profile_file <- startup_profile_file
      environment(profile_file) <- baseenv()

      # Start the app process. Its stdout and stderr go straight to the log
      # files so that output from child processes and native code is captured
      # and can never fill an unread pipe.
      #
      # The process starts in the app directory and reads the startup files
      # that R reads there, so that an app's own .Renviron and .Rprofile, such
      # as a profile that activates renv, apply as they would if the app were
      # run on its own; without them, the user's ~/.Renviron and ~/.Rprofile
      # apply. callr would read the files of this session's working directory
      # instead: its profile option is turned off, and it is told which
      # environment file to use. The profile runs after callr has set the
      # library paths, which renv then replaces. As at R's own startup, an
      # error in the profile is reported and the app still starts.
      process <- callr::r_bg(
        function(app_path, port, type, file, needs_render, log_dir, profile_file) {
          profile <- profile_file()
          if (file.exists(profile)) {
            tryCatch(source(profile), error = function(e) {
              message("Error in ", normalizePath(profile), ": ", conditionMessage(e))
            })
          }
          if (type == "qmd") {
            render <- needs_render(file, log_dir)
            message(if (render) "Rendering " else "Serving the rendered page of ", file)
            quarto::quarto_serve(
              input = file,
              render = render,
              port = port,
              host = "127.0.0.1",
              browse = FALSE
            )
          } else if (type == "rmd") {
            rmarkdown::run(
              file = file,
              shiny_args = list(
                port = port,
                host = "127.0.0.1",
                launch.browser = FALSE
              )
            )
          } else {
            shiny::runApp(
              appDir = app_path,
              port = port,
              host = "127.0.0.1",
              launch.browser = FALSE
            )
          }
        },
        args = list(
          app_path = app_path,
          port = app_port,
          type = app$type,
          file = app$file,
          needs_render = needs_render,
          log_dir = log_dir,
          profile_file = profile_file
        ),
        stdout = output_log,
        stderr = error_log,
        user_profile = FALSE,
        env = c(callr::rcmd_safe_env(), R_ENVIRON_USER = app_environ_file(app_path)),
        # Passed on to processx: the directory the app's R session starts in
        wd = app_path,
        # Stop the app even if this R session dies without cleaning up
        supervise = TRUE
      )

      config$add_app_process(app_name, process)

      # Mark app as starting (for non-blocking startup handling)
      started_at <- Sys.time()
      config$set_app_starting(app_name)

      # Schedule async readiness check instead of blocking
      later::later(function() {
        check_app_ready(app_name, app_port, process, started_at = started_at)
      }, delay = 0.5)

      # Return immediately - don't block
      logger::log_info("App {app_name} process started, checking readiness asynchronously", app_name = app_name)
      return(TRUE)
    },
    check_app_ready = function(app_name, app_port, process, attempt = 1, started_at = Sys.time(),
                               timed_out = FALSE) {
      "Check if app is ready to accept connections (async, non-blocking)"

      force(started_at)
      # Poll often for the window in which the app counts as starting up,
      # measured in time from its start: a probe of a port that nothing
      # listens on can take a second, as on Windows
      app_config <- config$get_app_config(app_name)
      startup_timeout <- max(config$APP_STARTUP_TIMEOUT_SECONDS, app_config$appstart_timeout %||% 2)

      # Ignore callbacks from stopped or replaced process generations.
      if (!identical(config$get_app_process(app_name), process)) return(FALSE)

      # Check if process is still alive
      if (!is_process_alive(process)) {
        logger::log_error("App {app_name} process died during startup", app_name = app_name)
        if (!kill_process_safely(process)) return(FALSE)
        config$set_app_ready(app_name) # Remove startup state
        config$remove_app_process(app_name)
        config$note_start_failure(app_name)
        return(FALSE)
      }

      # Use the same nonblocking TCP probe as HTTP startup requests.
      tracked_process <- config$get_app_process(app_name)
      return(promises::then(wait_for_backend(paste0("http://127.0.0.1:", app_port)), function(ready) {
        # A replacement may have been registered while the probe was pending.
        if (!identical(config$get_app_process(app_name), tracked_process)) return(FALSE)
        if (ready && !config$app_backend_verified(app_name)) {
          logger::log_warn("Port {port} for app {app_name} is held by another program",
            app_name = app_name, port = app_port
          )
          ready <- FALSE
        }
        if (ready) {
          logger::log_info("App {app_name} is ready on port {port} (attempt {attempt})",
            app_name = app_name, port = app_port, attempt = attempt
          )
          config$set_app_ready(app_name) # Mark as ready
          return(TRUE)
        }

        # If not ready and still within the startup window, schedule another check
        elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
        if (elapsed < startup_timeout) {
          later::later(function() {
            check_app_ready(app_name, app_port, process, attempt + 1, started_at)
          }, delay = 0.5)
          logger::log_debug("App {app_name} not ready yet, will retry (attempt {attempt}, {elapsed} of {timeout} seconds)",
            app_name = app_name, attempt = attempt, elapsed = round(elapsed, 1), timeout = startup_timeout
          )
          return(FALSE)
        }

        if (!timed_out) {
          # The next request for the app reports the timeout and ends this state
          config$set_app_start_timed_out(app_name)
        }
        # A slow start, such as a long Quarto render, may still finish. Keep
        # looking, less often, while the process runs, for as long as an app
        # may keep starting.
        uptime <- process_uptime_seconds(process)
        if (!is.null(uptime) && uptime < config$ON_DEMAND_START_LIMIT_SECONDS) {
          if (!timed_out) {
            logger::log_warn("App {app_name} is not ready after {seconds} seconds; still waiting for it to start",
              app_name = app_name, seconds = round(elapsed)
            )
          }
          later::later(function() {
            check_app_ready(app_name, app_port, process, attempt + 1, started_at, timed_out = TRUE)
          }, delay = 2)
        } else {
          logger::log_error("App {app_name} failed to become ready after {seconds} seconds",
            app_name = app_name, seconds = round(elapsed)
          )
        }

        return(FALSE)
      }, onRejected = function(e) {
        logger::log_error("Readiness check failed for {app_name}: {error}",
          app_name = app_name, error = conditionMessage(e)
        )
        FALSE
      }))
    },
    start_app_on_demand = function(app_name) {
      "Start a non-resident app on demand if not already running"

      app_config <- config$get_app_config(app_name)
      if (is.null(app_config)) {
        logger::log_error("Cannot start app on demand: {app_name} not found in configuration", app_name = app_name)
        return(FALSE)
      }

      # Check if app is already running or starting
      process <- config$get_app_process(app_name)
      if (!is.null(process) && is_process_alive(process)) {
        logger::log_debug("App {app_name} already running on demand", app_name = app_name)
        return(TRUE)
      }

      # Retain ownership of a crashed process until its workers are terminated.
      if (!is.null(process)) {
        if (!kill_process_safely(process)) return(FALSE)
        cleanup_app_connections(app_name)
        config$remove_app_process(app_name)
        config$note_start_failure(app_name)
      }

      # Check if app is currently starting
      if (config$is_app_starting(app_name)) {
        logger::log_debug("App {app_name} is already starting, not starting again", app_name = app_name)
        return(TRUE)
      }

      # An app whose process failed starts again only after restart_delay, as
      # a resident app does, rather than at every request: a visitor's starting
      # page asks every few seconds, and each start loads a new R process
      backoff <- config$start_backoff_seconds(app_name)
      if (backoff > 0) {
        logger::log_info("Not starting app {app_name} yet: its process failed; it can start again in {seconds} seconds",
          app_name = app_name, seconds = ceiling(backoff)
        )
        return(FALSE)
      }

      logger::log_info("Starting app {app_name} on demand", app_name = app_name)

      # Start the app (now non-blocking). A start error, which may name
      # internal paths, is logged here instead of reaching the client.
      success <- tryCatch(start_app(app_config), error = function(e) {
        logger::log_error("Error starting app {app_name}: {error}", app_name = app_name, error = conditionMessage(e))
        FALSE
      })
      if (success) {
        logger::log_info("Successfully initiated startup for app {app_name} on demand", app_name = app_name)
      } else {
        logger::log_error("Failed to start app {app_name} on demand", app_name = app_name)
      }

      return(success)
    },
    schedule_restart = function(app_config) {
      "Restart a failed resident app after restart_delay, without blocking other applications"
      app_name <- app_config$name
      token <- new.env(parent = emptyenv())
      assign(app_name, token, envir = pending_restarts)
      restart <- function() {
        if (!identical(pending_restarts[[app_name]], token)) return(FALSE)
        assign(app_name, NULL, envir = pending_restarts)
        process <- config$get_app_process(app_name)
        if (!is.null(process) && is_process_alive(process)) return(FALSE)
        tryCatch(start_app(app_config), error = function(e) {
          logger::log_error("Failed to restart app {app_name}: {error}", app_name = app_name, error = e$message)
          FALSE
        })
      }
      delay <- config$config$restart_delay %||% 5
      if (delay <= 0) {
        return(isTRUE(restart()))
      }
      later::later(restart, delay)
      TRUE
    },
    restart_app = function(app_name) {
      "Restart a specific application on request"

      app_config <- config$get_app_config(app_name)
      if (is.null(app_config)) {
        return(list(success = FALSE, message = "App not found"))
      }

      logger::log_info("Restarting app: {app_name}", app_name = app_name)

      tryCatch(
        {
          # Stop existing process. Its users are disconnected only once it has
          # stopped, so a restart that cannot stop it leaves them connected.
          process <- config$get_app_process(app_name)
          if (!is.null(process)) {
            if (!kill_process_safely(process)) {
              return(list(success = FALSE, message = paste("Failed to stop app", app_name)))
            }
          }

          # Close existing connections for this app
          cleanup_app_connections(app_name)
          config$remove_app_process(app_name)
          # This restart replaces one that a failure had scheduled, and an
          # on-demand app may start again as soon as it is next opened
          assign(app_name, NULL, envir = pending_restarts)
          config$clear_start_failure(app_name)

          # An on-demand app has no users left to serve, so it starts again
          # when it is next opened
          if (!app_config$resident) {
            return(list(success = TRUE, message = paste(
              "App", app_name, "stopped; it starts again when it is next opened"
            )))
          }

          # restart_delay spaces out the retries of an app that keeps failing.
          # A restart asked for here has nothing to wait for.
          start_app(app_config)
          return(list(success = TRUE, message = paste("App", app_name, "restarted")))
        },
        error = function(e) {
          logger::log_error("Failed to restart app {app_name}: {error}", app_name = app_name, error = e$message)
          # The dashboard shows this message, so that a restart that cannot
          # start the app says why, such as a missing app directory, even
          # though the reason can name files on the server. Only the
          # management server answers with it, and only on the loopback
          # address, to the same people who can read server.log. The proxy
          # never sends start errors to visitors.
          return(list(success = FALSE, message = paste0("Failed to restart app ", app_name, ": ", e$message)))
        }
      )
    },
    stop_app = function(app_name) {
      "Stop a specific application"

      logger::log_info("Stopping app: {app_name}", app_name = app_name)
      assign(app_name, NULL, envir = pending_restarts)

      process <- config$get_app_process(app_name)
      if (!is.null(process)) {
        success <- kill_process_safely(process)
        if (success) {
          config$remove_app_process(app_name)
          logger::log_info("Successfully stopped app {app_name}", app_name = app_name)
          return(list(success = TRUE, message = paste("App", app_name, "stopped successfully")))
        } else {
          logger::log_error("Failed to stop app {app_name}: its process is still running", app_name = app_name)
          return(list(success = FALSE, message = paste("Failed to stop app", app_name)))
        }
      }

      return(list(success = FALSE, message = paste("App", app_name, "not found or already stopped")))
    },
    stop_app_immediately = function(app_name) {
      "Stop a non-resident app immediately when connections reach zero"

      app_config <- config$get_app_config(app_name)
      if (is.null(app_config)) {
        logger::log_error("Cannot stop unknown app: {app_name}", app_name = app_name)
        return(FALSE)
      }

      # Only allow immediate stops for non-resident apps
      if (app_config$resident) {
        logger::log_debug("Not stopping resident app: {app_name}", app_name = app_name)
        return(FALSE)
      }

      logger::log_info("Immediately stopping non-resident app {app_name} (no active connections)", app_name = app_name)
      result <- stop_app(app_name)
      return(result$success)
    },
    health_check = function() {
      "Check health of all applications and restart if necessary"

      for (app_config in config$config$apps) {
        app_name <- app_config$name
        if (!is.null(pending_restarts[[app_name]])) next
        tryCatch({
          process <- config$get_app_process(app_name)

          if (!is.null(process)) {
            if (!is_process_alive(process)) {
              logger::log_error("App {app_name} died, restarting", app_name = app_name)
              if (!kill_process_safely(process)) next

              # Clean up connections for this app
              cleanup_app_connections(app_name)

              # Remove dead process
              config$remove_app_process(app_name)
              config$note_start_failure(app_name)

              # Only restart if it's a resident app
              if (app_config$resident) {
                schedule_restart(app_config)
              } else {
                logger::log_info("Non-resident app {app_name} died, will start on next request", app_name = app_name)
              }
            }
          } else {
            # App not running - only restart if it's resident. Apps that died
            # during startup or were cleaned up also wait for restart_delay.
            if (app_config$resident) {
              if (is.null(pending_restarts[[app_name]])) {
                logger::log_info("Resident app {app_name} not running, restarting", app_name = app_name)
                schedule_restart(app_config)
              }
            } else {
              logger::log_debug("Non-resident app {app_name} is stopped (normal state)", app_name = app_name)
            }
          }
        }, error = function(e) {
          logger::log_error("Health check failed for app {app_name}: {error}",
            app_name = app_name, error = conditionMessage(e))
        })
      }
    },
    cleanup_app_connections = function(app_name) {
      "Clean up WebSocket connections for a specific app"

      # Find and close backend connections for this app
      for (session_id in names(config$get_all_backend_connections())) {
        conn_info <- config$get_backend_connection(session_id)
        if (!is.null(conn_info) && conn_info$app_name == app_name) {
          if (!is.null(conn_info$ws)) {
            tryCatch(conn_info$ws$close(), error = function(e) {})
          }
          config$remove_backend_connection(session_id)
        }
      }

      # Find and close client connections for this app
      sessions_to_remove <- c()
      for (session_id in names(config$get_all_ws_connections())) {
        conn_info <- config$get_ws_connection(session_id)
        if (!is.null(conn_info) && !is.null(conn_info$app_name) && conn_info$app_name == app_name) {
          if (!is.null(conn_info$ws)) {
            tryCatch(conn_info$ws$close(), error = function(e) {})
          }
          sessions_to_remove <- c(sessions_to_remove, session_id)
        }
      }

      # Remove closed connections
      for (session_id in sessions_to_remove) {
        config$remove_ws_connection(session_id)
      }
    },
    cleanup_stale_connections = function() {
      "Close sessions that have carried no message for idle_timeout_minutes"

      # An idle session's sockets are still open, so this is a limit on idle
      # time, not a search for dead peers: a socket that closes removes its own
      # session. A timeout of zero keeps idle sessions open.
      timeout_minutes <- config$config$idle_timeout_minutes %||% config$CONNECTION_TIMEOUT_MINUTES
      if (timeout_minutes <= 0) {
        return(0)
      }

      current_time <- Sys.time()
      timeout_threshold <- current_time - (timeout_minutes * 60)
      connections_cleaned <- 0

      # Snapshot connection names to avoid issues if connections are removed during iteration
      backend_session_ids <- names(config$get_all_backend_connections())
      client_session_ids <- names(config$get_all_ws_connections())

      # Clean up stale backend connections
      for (session_id in backend_session_ids) {
        tryCatch(
          {
            conn_info <- config$get_backend_connection(session_id)

            # Defensive: connection might have been removed by another callback
            if (is.null(conn_info)) {
              next
            }

            # Check if connection has timestamp and is stale
            last_activity <- config$get_session_activity(session_id, conn_info$last_activity)
            if (!is.null(last_activity) && last_activity < timeout_threshold) {
              logger::log_info("Cleaning stale backend connection: {session_id}, last activity: {time}",
                session_id = session_id, time = format(last_activity)
              )

              if (!is.null(config$get_ws_connection(session_id))) {
                create_connection_manager(config, .self)$close_client_connection(session_id)
              } else {
                config$remove_backend_connection(session_id)
                if (!is.null(conn_info$ws)) {
                  tryCatch(conn_info$ws$close(), error = function(e) {
                    logger::log_debug("Error closing backend ws: {error}", error = e$message)
                  })
                }
              }
              connections_cleaned <- connections_cleaned + 1
            }
          },
          error = function(e) {
            logger::log_warn("Error during backend connection cleanup for {session_id}: {error}",
              session_id = session_id, error = e$message
            )
          }
        )
      }

      # Clean up stale client connections
      for (session_id in client_session_ids) {
        tryCatch(
          {
            conn_info <- config$get_ws_connection(session_id)

            # Defensive: connection might have been removed by another callback
            if (is.null(conn_info)) {
              next
            }

            # Check if connection has timestamp and is stale
            last_activity <- config$get_session_activity(session_id, conn_info$last_activity)
            if (!is.null(last_activity) && last_activity < timeout_threshold) {
              logger::log_info("Cleaning stale client connection: {session_id}, app: {app}, last activity: {time}",
                session_id = session_id,
                app = conn_info$app_name %||% "unknown",
                time = format(last_activity)
              )

              # Close both sockets and run the normal idle-app cleanup path.
              create_connection_manager(config, .self)$close_client_connection(session_id)
              connections_cleaned <- connections_cleaned + 1
            }
          },
          error = function(e) {
            logger::log_warn("Error during client connection cleanup for {session_id}: {error}",
              session_id = session_id, error = e$message
            )
          }
        )
      }

      if (connections_cleaned > 0) {
        logger::log_info("Cleaned up {connections_cleaned} stale connections", connections_cleaned = connections_cleaned)
      }

      # Log current connection counts for monitoring
      backend_count <- length(config$get_all_backend_connections())
      ws_count <- length(config$get_all_ws_connections())
      logger::log_debug("Active connections - Backend: {backend_count}, WebSocket: {ws_count}",
        backend_count = backend_count, ws_count = ws_count
      )

      return(connections_cleaned)
    },
    get_app_status = function(app_name) {
      "Get status of a specific app"

      process <- config$get_app_process(app_name)
      app_config <- config$get_app_config(app_name)

      if (is.null(app_config)) {
        return(NULL)
      }

      # An on-demand app whose process has exited but not yet been cleaned up
      # is not shown as crashed: opening it starts it again, while a restart
      # from the dashboard would only stop it
      alive <- !is.null(process) && is_process_alive(process)
      status <- if (alive) {
        "running"
      } else if (app_config$resident) {
        if (is.null(process)) "stopped" else "crashed" # Resident app should be running
      } else if (is.null(config$cached_start_problem(app_config))) {
        "dormant" # Non-resident app is normally stopped
      } else {
        "unavailable" # Opening the app could not start it
      }

      app_status <- list(
        name = app_name,
        status = status,
        resident = app_config$resident,
        port = app_config$port,
        path = app_config$path,
        connections = config$get_app_connection_count(app_name)
      )
      # Only a running app has a process ID. It is left out otherwise,
      # because jsonlite writes NULL as {}.
      if (status == "running") {
        app_status$pid <- process$get_pid()
      }
      return(app_status)
    },
    get_all_app_status = function() {
      "Get status of all apps"

      apps_status <- list()

      for (app_config in config$get_sorted_apps()) {
        app_name <- app_config$name
        status <- get_app_status(app_name)
        if (!is.null(status)) {
          apps_status[[app_name]] <- status
        }
      }

      return(apps_status)
    },
    stop_all_apps = function() {
      "Stop all running applications and clean up all connections. Returns the names of apps that did not stop."
      pending_restarts <<- new.env(parent = emptyenv())

      logger::log_info("Stopping all applications...")

      # Stop all app processes. One that outlives the first attempt, for
      # example because it is slow to exit, is tried once more.
      for (attempt in 1:2) {
        for (app_name in names(config$get_all_app_processes())) {
          stop_app(app_name)
        }
      }

      # Clean up backend connections (close WebSocket handles)
      backend_session_ids <- names(config$get_all_backend_connections())
      for (session_id in backend_session_ids) {
        conn_info <- config$get_backend_connection(session_id)
        if (!is.null(conn_info) && !is.null(conn_info$ws)) {
          tryCatch(conn_info$ws$close(), error = function(e) {})
        }
        config$remove_backend_connection(session_id)
      }

      # Forget the client connections; remove_ws_connection() also updates
      # each app's connection count
      client_session_ids <- names(config$get_all_ws_connections())
      for (session_id in client_session_ids) {
        config$remove_ws_connection(session_id)
      }

      # A process that is still tracked is still running
      surviving <- names(config$get_all_app_processes())
      if (length(surviving) > 0) {
        logger::log_error("Could not stop apps: {apps}", apps = paste(surviving, collapse = ", "))
      } else {
        logger::log_info("All applications stopped")
      }
      invisible(surviving)
    }
  )
)

# Create process manager factory function
create_process_manager <- function(server_config) {
  return(ProcessManager$new(server_config))
}
