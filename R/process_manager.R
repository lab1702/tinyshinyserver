# Process Management Module
# Handles Shiny app process lifecycle, health monitoring, and cleanup

# What an app directory holds, in the order start_app() prefers: a standard
# Shiny app, then an R Markdown document, then a Quarto document. A directory
# that cannot be served is an error here, so that no process is started only
# to exit again.
detect_app_type <- function(app_path) {
  if (!dir.exists(app_path)) {
    # As when path names the app's app.R instead of its directory
    if (file.exists(app_path)) {
      stop("App path names a file, not a directory: ", app_path,
        "; set path to the directory that holds it", call. = FALSE)
    }
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

# A Quarto document is rendered before it is served, which takes many seconds,
# so it is rendered only when its page is missing or may be out of date. The
# server records each render of its own in state_file: a render that did not
# finish, such as one stopped part-way, leaves an incomplete page; and once a
# render has finished, the page is current while the source files are the
# ones it was rendered from, with the same names, sizes, and modification
# times, so that a file copied in with an older time, or deleted, counts. A
# page that the server did not render, such as one deployed with the app or
# rendered since, is current while it is newer than every source file. Files
# that rendering writes, and the server's own files in log_dir, are not
# sources: when log_dir lies in the app directory, they change at every start.
# Either way, a page is out of date once a package that it was rendered with
# has another version, as after an update of R or of the app's library, and
# once the {stem}_files directory that it loads its resources from is missing.
#
# The result carries the sources as attribute "sources", and a function that
# lists them again as attribute "snapshot", for render_quarto_document() to
# record the sources as the render left them. start_app() hands this function
# to the app's own process, which need not load this package, so it may call
# only base R.
quarto_needs_render <- function(qmd_file, log_dir = NULL, state_file = NULL) {
  # Quarto names the page "doc.html" for "doc.qmd" and "doc.QMD" alike
  output <- sub("\\.qmd$", ".html", qmd_file, ignore.case = TRUE)
  app_dir <- dirname(qmd_file)
  # Files that rendering itself writes: the page, its resources, the data of
  # "context: data" chunks, the knitr cache, and the intermediate files of a
  # render, which a render that was stopped part-way leaves behind
  stem <- sub("\\.qmd$", "", basename(qmd_file), ignore.case = TRUE)
  generated <- c(
    basename(output), ".quarto",
    paste0(stem, c("_files", "_data", "_cache", ".rmarkdown", ".knit.md", ".quarto_ipynb"))
  )

  # The names, modification times, and sizes of the source files as they are
  # now. render_quarto_document() takes them again once a render finishes.
  take_snapshot <- function() {
    sources <- list.files(app_dir, recursive = TRUE, all.files = TRUE)
    sources <- sources[!sub("/.*$", "", sources) %in% generated]
    # Files that change without the document changing: hidden files and
    # directories, such as .git, .Rproj.user, and .Rhistory, and the packages
    # that renv installs. A top-level .Rprofile or .Renviron does count, as it
    # configures the R session that renders the document.
    hidden <- grepl("(^|/)[.]", sources) & !sources %in% c(".Rprofile", ".Renviron")
    renv_owned <- grepl("^renv/(library|local|python|sandbox|staging)/", sources)
    sources <- sources[!hidden & !renv_owned]

    # The files directly in log_dir that the server writes: server.log, every
    # app's logs, and every app's record of its renders
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
        sources <- sources[!(in_logs & (endsWith(listed, ".log") | endsWith(listed, "_render.rds")))]
      }
    }

    sources <- sort(sources, method = "radix")
    paths <- file.path(app_dir, sources)
    list(files = sources, mtime = as.numeric(file.mtime(paths)), size = as.numeric(file.size(paths)))
  }
  snapshot <- take_snapshot()
  answer <- function(render) structure(render, sources = snapshot, snapshot = take_snapshot)

  if (!file.exists(output)) {
    return(answer(TRUE))
  }

  page_lines <- NULL
  read_page <- function() {
    if (is.null(page_lines)) {
      page_lines <<- tryCatch(readLines(output, encoding = "UTF-8", warn = FALSE), error = function(e) character())
    }
    page_lines
  }

  # A page whose resources are missing, as when the page was deployed without
  # its {stem}_files directory, or that directory was removed, loads without
  # its scripts and styles, and its Shiny session never starts
  resources <- paste0(stem, "_files")
  if (!dir.exists(file.path(app_dir, resources))) {
    references <- paste0(unique(c(resources, utils::URLencode(resources))), "/")
    if (any(vapply(references, function(reference) any(grepl(reference, read_page(), fixed = TRUE)), logical(1)))) {
      return(answer(TRUE))
    }
  }

  # The page keeps the scripts and styles of the packages that it was
  # rendered with, and records their versions. rmarkdown would render it
  # again when one of them has changed, but start_app() tells it not to
  # render, so the versions are compared here, as rmarkdown compares them. A
  # page whose record cannot be read, as where jsonlite is missing, is judged
  # by its files alone, so that it is not rendered at every start.
  packages_changed <- function() {
    lines <- read_page()
    ends <- which(lines == "</script>")
    context <- function(name) {
      tag <- paste0("<script type=\"application/shiny-prerendered\" data-context=\"", name, "\">")
      start <- which(grepl(tag, lines, fixed = TRUE))
      if (length(start) == 0) {
        return(NULL)
      }
      start <- start[length(start)]
      end <- ends[ends > start][1]
      if (is.na(end) || end == start + 1) {
        return(NULL)
      }
      json <- gsub("<\\u002f", "</", lines[(start + 1):(end - 1)], fixed = TRUE)
      tryCatch(jsonlite::unserializeJSON(paste(json, collapse = "\n")), error = function(e) NULL)
    }
    recorded <- character()
    for (dependency in context("dependencies")) {
      if (is.list(dependency) && is.character(dependency$package) && is.character(dependency$pkgVersion)) {
        recorded[dependency$package[1]] <- dependency$pkgVersion[1]
      }
    }
    execution <- context("execution_dependencies")$packages
    if (is.data.frame(execution) && all(c("packages", "version") %in% names(execution))) {
      recorded[as.character(execution[["packages"]])] <- as.character(execution[["version"]])
    }
    for (package in names(recorded)) {
      installed <- tryCatch(
        suppressWarnings(as.character(utils::packageDescription(package, fields = "Version"))),
        error = function(e) NA_character_
      )
      if (!identical(installed, unname(recorded[package]))) {
        return(TRUE)
      }
    }
    FALSE
  }

  state <- if (is.null(state_file)) NULL else tryCatch(suppressWarnings(readRDS(state_file)), error = function(e) NULL)
  if (is.list(state) && identical(state$document, qmd_file)) {
    # A render that has not finished may have left an incomplete page
    if (!identical(state$status, "rendered")) {
      return(answer(TRUE))
    }
    page <- file.info(output)
    same_page <- identical(state$page, list(mtime = as.numeric(page$mtime), size = as.numeric(page$size)))
    # A page that has changed since was rendered elsewhere, and is judged as one
    if (same_page) {
      return(answer(!identical(state$sources, snapshot) || packages_changed()))
    }
  }

  # A file as new as the page may have changed after it was rendered
  changed <- snapshot$mtime >= as.numeric(file.mtime(output))
  answer(any(is.na(changed) | changed) || packages_changed())
}

# Render a Quarto document with the Quarto CLI, recording in state_file that a
# render has begun and, once it has finished, the page and the sources it was
# rendered from (see quarto_needs_render()): those that snapshot() lists then,
# with the document as it was when the render began. The CLI is run directly
# rather than through the quarto package: an app's profile, such as one that
# activates renv, may have replaced the library paths with ones that lack it.
# As the quarto package does, the CLI is the one that QUARTO_PATH names or
# that PATH finds, here in the app's environment, which its .Renviron and
# .Rprofile may have set; quarto_bin, the one this server found, comes next.
# Called in the app's own process, so it may call only base R.
render_quarto_document <- function(qmd_file, sources, state_file, quarto_bin, snapshot = NULL) {
  own <- Sys.getenv("QUARTO_PATH", unset = "")
  if (!nzchar(own)) own <- unname(Sys.which("quarto"))
  if (nzchar(own)) quarto_bin <- own
  if (is.null(quarto_bin) || !nzchar(quarto_bin)) {
    stop("Cannot render ", qmd_file, ": the Quarto CLI was not found. Install it from https://quarto.org/",
      call. = FALSE
    )
  }
  record <- function(state) {
    tryCatch(saveRDS(c(list(document = qmd_file), state), state_file), error = function(e) {
      message("Could not record the render in ", state_file, ": ", conditionMessage(e))
    })
  }
  record(list(status = "rendering"))
  # Quarto then renders with this R, as the quarto package arranges
  if (is.na(Sys.getenv("QUARTO_R", unset = NA))) {
    Sys.setenv(QUARTO_R = R.home("bin"))
  }
  # The R session that Quarto starts would make its library paths from the
  # startup files again, and could find other versions of the packages that
  # the app is served with, or none of them. It gets this session's library
  # paths, which the app's .Renviron or profile may have set, and reads no
  # user environment file: this session's variables, which it inherits,
  # already hold what that file set. A profile that sets the library paths,
  # such as one that activates renv, runs there too and sets them again.
  saved <- Sys.getenv(c("R_LIBS", "R_ENVIRON_USER"), unset = NA, names = TRUE)
  on.exit(for (name in names(saved)) {
    if (is.na(saved[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, structure(list(saved[[name]]), names = name))
  }, add = TRUE)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep), R_ENVIRON_USER = nullfile())
  status <- system2(quarto_bin, c("render", shQuote(qmd_file)))
  if (!identical(as.integer(status), 0L)) {
    stop("quarto render ", qmd_file, " failed with exit status ", status, call. = FALSE)
  }
  # The sources as the render left them, from snapshot(), so that files that
  # the document's own code writes into its directory, such as a plot or data
  # that it saves, are not taken for changes at the next start. The document
  # keeps the state that it was rendered from, so that an edit made to it
  # while it rendered is rendered at the next start.
  if (is.function(snapshot)) {
    after <- snapshot()
    document <- basename(qmd_file)
    before_at <- match(document, sources$files)
    after_at <- match(document, after$files)
    if (!is.na(before_at) && !is.na(after_at)) {
      after$mtime[after_at] <- sources$mtime[before_at]
      after$size[after_at] <- sources$size[before_at]
    }
    sources <- after
  }
  page <- file.info(sub("\\.qmd$", ".html", qmd_file, ignore.case = TRUE))
  record(list(
    status = "rendered", sources = sources,
    page = list(mtime = as.numeric(page$mtime), size = as.numeric(page$size))
  ))
  invisible(TRUE)
}

# Library paths for an app whose environment file sets R_LIBS or R_LIBS_USER,
# as R gives them at startup. callr gives every app this session's library
# paths, and sets these variables back to this session's values once the
# app's process has started, after R has read the app's environment file. The
# file is read again for them alone; other variables keep the values that R
# gave them. Called in the app's own process, so it may call only base R.
apply_environ_libraries <- function(environ_file) {
  if (!is.character(environ_file) || length(environ_file) != 1 || is.na(environ_file) ||
    !nzchar(environ_file) || !file.exists(environ_file) || dir.exists(environ_file)) {
    return(invisible(FALSE))
  }
  lines <- tryCatch(readLines(environ_file, warn = FALSE), error = function(e) character())
  assigned <- unique(sub(
    "^[[:space:]]*([^=[:space:]]+)[[:space:]]*=.*$", "\\1",
    grep("^[[:space:]]*[^#=[:space:]]+[[:space:]]*=", lines, value = TRUE)
  ))
  library_vars <- intersect(c("R_LIBS", "R_LIBS_USER"), assigned)
  if (length(library_vars) == 0) {
    return(invisible(FALSE))
  }
  others <- setdiff(assigned, library_vars)
  saved <- Sys.getenv(others, unset = NA, names = TRUE)
  # R skips a line that it cannot read, with a warning that it gave once
  # already, at the app's startup; the file's other lines still apply
  read <- tryCatch(
    withCallingHandlers(readRenviron(environ_file), warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) FALSE
  )
  for (name in others) {
    if (is.na(saved[[name]])) {
      Sys.unsetenv(name)
    } else {
      do.call(Sys.setenv, structure(list(saved[[name]]), names = name))
    }
  }
  if (!isTRUE(read)) {
    return(invisible(FALSE))
  }
  paths <- function(name) {
    value <- Sys.getenv(name)
    if (!nzchar(value)) {
      return(character())
    }
    # R expands "%v" and the like in R_LIBS_USER
    if (name == "R_LIBS_USER" && exists(".expand_R_libs_env_var", envir = baseenv())) {
      value <- get(".expand_R_libs_env_var", envir = baseenv())(value)
    }
    strsplit(value, .Platform$path.sep, fixed = TRUE)[[1]]
  }
  .libPaths(c(paths("R_LIBS"), paths("R_LIBS_USER")))
  invisible(TRUE)
}

# The user environment file that R reads when it starts in an app's directory:
# the file that R_ENVIRON_USER names, the directory's .Renviron, or
# ~/.Renviron. callr would otherwise copy the .Renviron of this session's
# working directory into every app.
app_environ_file <- function(app_path, named = Sys.getenv("R_ENVIRON_USER", unset = NA)) {
  if (!is.na(named)) {
    # Set but empty, the variable tells R to read no user environment file.
    # An empty value does not reach the app's process, so it is given the
    # null device, which holds no variables.
    return(if (nzchar(named)) named else nullfile())
  }
  local <- file.path(app_path, ".Renviron")
  if (file.exists(local)) local else path.expand("~/.Renviron")
}

# The names of the variables that an environment file assigns, in lines of the
# form name=value, as R reads it
environ_assigned_names <- function(environ_file) {
  if (!is.character(environ_file) || length(environ_file) != 1 || is.na(environ_file) ||
    !file.exists(environ_file) || dir.exists(environ_file)) {
    return(character())
  }
  lines <- tryCatch(readLines(environ_file, warn = FALSE), error = function(e) character())
  unique(sub(
    "^[[:space:]]*([^=[:space:]]+)[[:space:]]*=.*$", "\\1",
    grep("^[[:space:]]*[^#=[:space:]]+[[:space:]]*=", lines, value = TRUE)
  ))
}

# An environment file for R to read in place of an app's own, environ_file,
# when that one sets R_PROFILE_USER. callr finishes setting up an app's process
# in a profile of its own, which R_PROFILE_USER names: it restores the
# environment variables that callr changed, and attaches the tools that let
# an error that ends the app reach the app's error log. The app's file would
# replace that profile, as a variable in an environment file replaces the one
# that is set. This file reads the app's file, keeps the profile that it chose
# in TSS_APP_PROFILE_USER, for the app's process to run after callr's, and
# names callr's profile again. Written to path, which is returned.
profile_keeping_environ_file <- function(environ_file, path) {
  writeLines('TSS_CALLR_PROFILE_USER="${R_PROFILE_USER}"', path)
  file.append(path, environ_file)
  cat('\nTSS_APP_PROFILE_USER="${R_PROFILE_USER}"\nR_PROFILE_USER="${TSS_CALLR_PROFILE_USER}"\n',
    file = path, append = TRUE
  )
  path
}

# The user profile that R reads when it starts in the working directory: the
# file that R_PROFILE_USER names, the directory's .Rprofile, or ~/.Rprofile.
# Set but empty, R_PROFILE_USER names none, and "" is returned. start_app()
# hands this function to the app's own process, which need not load this
# package, so it may call only base R. callr does not pass an empty variable
# on, so the server's own value is given to it.
startup_profile_file <- function(named = Sys.getenv("R_PROFILE_USER", unset = NA)) {
  if (!is.na(named)) {
    return(if (nzchar(named)) path.expand(named) else "")
  }
  if (file.exists(".Rprofile")) ".Rprofile" else path.expand("~/.Rprofile")
}

# Process Manager Class
ProcessManager <- setRefClass("ProcessManager",
  fields = list(
    config = "ANY",
    pending_restarts = "environment",
    # The process of each app for which a readiness check has reported that
    # another program holds its port
    port_conflicts = "environment"
  ),
  methods = list(
    initialize = function(server_config) {
      config <<- server_config
      pending_restarts <<- new.env(parent = emptyenv())
      port_conflicts <<- new.env(parent = emptyenv())
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
        logger::log_info("Detected Quarto document for {app_name}, rendering it with the Quarto CLI when needed and serving it with rmarkdown::run",
          app_name = app_name
        )
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
      # file.rename() replaces the older .prev.log. A log that cannot be
      # renamed, such as one that another program holds open on Windows, is
      # copied instead, because the app's new process empties it.
      for (log_file in c(output_log, error_log)) {
        if (file.exists(log_file)) {
          previous_log <- sub("\\.log$", ".prev.log", log_file)
          tryCatch(
            suppressWarnings({
              if (!file.rename(log_file, previous_log)) {
                file.copy(log_file, previous_log, overwrite = TRUE)
              }
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
      render_document <- render_quarto_document
      environment(render_document) <- baseenv()
      environ_libraries <- apply_environ_libraries
      environment(environ_libraries) <- baseenv()
      profile_file <- startup_profile_file
      environment(profile_file) <- baseenv()
      environ_file <- app_environ_file(app_path)
      # Only the app's own environment file sets its library paths. Without
      # one, the app keeps this session's, such as a renv library, even where
      # ~/.Renviron, which R would read instead, sets R_LIBS_USER.
      library_file <- if (identical(environ_file, file.path(app_path, ".Renviron"))) environ_file else ""
      # R reads the environment file before it chooses a profile, so a file
      # that sets R_PROFILE_USER has chosen the app's profile, as it would for
      # the app run on its own: the profile that it names, or none for an
      # empty value (on Windows, which cannot hold an empty variable, the one
      # that R finds without it), and no other. R reads such a file through
      # one that keeps callr's own profile; see profile_keeping_environ_file().
      profile_chosen <- "R_PROFILE_USER" %in% environ_assigned_names(environ_file)
      process_environ_file <- if (profile_chosen) {
        # tempdir(check = TRUE) makes R's temporary directory again where a
        # cleaner of old temporary files has removed it from a long-running
        # server, as callr::r_bg() does for its own files
        profile_keeping_environ_file(environ_file,
          tempfile(paste0("tss-", app_name, "-"), tmpdir = tempdir(check = TRUE), fileext = ".Renviron"))
      } else {
        environ_file
      }
      # The record of the server's renders of the app's Quarto document
      state_file <- file.path(log_dir, paste0(app_name, "_render.rds"))
      # Found here, where the quarto package is loaded; needed only to render
      quarto_bin <- if (identical(app$type, "qmd")) {
        tryCatch(quarto::quarto_path() %||% "", error = function(e) "")
      } else {
        ""
      }

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
      # library paths, and after those that the environment file sets, which
      # renv then replaces. As at R's own startup, an error in the profile is
      # reported and the app still starts. The site profile runs too.
      process <- callr::r_bg(
        function(app_path, port, type, file, needs_render, render_document, log_dir, state_file,
                 quarto_bin, environ_libraries, library_file, profile_file, profile_setting,
                 profile_chosen, environ_file, process_environ_file) {
          if (profile_chosen) {
            # The profile that the app's environment file chose. Processes
            # that the app starts, such as the R session that renders a Quarto
            # document, see the app's own file and the profile that it chose.
            profile_setting <- Sys.getenv("TSS_APP_PROFILE_USER", unset = NA)
            Sys.unsetenv(c("TSS_APP_PROFILE_USER", "TSS_CALLR_PROFILE_USER"))
            if (is.na(profile_setting)) {
              Sys.unsetenv("R_PROFILE_USER")
            } else {
              Sys.setenv(R_PROFILE_USER = profile_setting)
            }
            Sys.setenv(R_ENVIRON_USER = environ_file)
            unlink(process_environ_file)
          }
          environ_libraries(library_file)
          profile <- profile_file(profile_setting)
          if (nzchar(profile) && file.exists(profile)) {
            first <- get0(".First", envir = globalenv(), inherits = FALSE)
            tryCatch(source(profile), error = function(e) {
              message("Error in ", normalizePath(profile), ": ", conditionMessage(e))
            })
            # R calls the .First() that the profile defines once it has run
            # the profile, as it would for the app run on its own
            app_first <- get0(".First", envir = globalenv(), inherits = FALSE)
            if (is.function(app_first) && !identical(app_first, first)) {
              tryCatch(app_first(), error = function(e) {
                message("Error in .First(): ", conditionMessage(e))
              })
            }
          }
          if (type == "qmd") {
            render <- needs_render(file, log_dir, state_file)
            message(if (render) "Rendering " else "Serving the rendered page of ", file)
            if (render) render_document(file, attr(render, "sources"), state_file, quarto_bin, attr(render, "snapshot"))
            # As quarto::quarto_serve() does: the rendered page is served
            # without rmarkdown rendering it again
            Sys.setenv(RMARKDOWN_RUN_PRERENDER = "0")
            rmarkdown::run(file, shiny_args = list(
              port = port,
              host = "127.0.0.1",
              launch.browser = FALSE
            ))
          } else if (type == "rmd") {
            # The document chosen here is the one served at the app's root.
            # rmarkdown::run() would otherwise choose its own, among the .qmd
            # files of the directory as well.
            rmarkdown::run(
              file = file,
              default_file = basename(file),
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
          render_document = render_document,
          log_dir = log_dir,
          state_file = state_file,
          quarto_bin = quarto_bin,
          environ_libraries = environ_libraries,
          library_file = library_file,
          profile_file = profile_file,
          profile_setting = Sys.getenv("R_PROFILE_USER", unset = NA),
          profile_chosen = profile_chosen,
          environ_file = environ_file,
          process_environ_file = process_environ_file
        ),
        stdout = output_log,
        stderr = error_log,
        system_profile = TRUE,
        user_profile = FALSE,
        env = c(callr::rcmd_safe_env(), R_ENVIRON_USER = process_environ_file),
        # Passed on to processx: the directory the app's R session starts in
        wd = app_path,
        # Stop the app even if this R session dies without cleaning up
        supervise = TRUE
      )

      config$add_app_process(app_name, process)
      config$set_app_document(app_name, app)

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
          # Probes repeat every half second, and then every 2 seconds for as
          # long as the app may keep starting, so this is said once a start
          if (identical(port_conflicts[[app_name]], tracked_process)) {
            logger::log_debug("Port {port} for app {app_name} is still held by another program",
              app_name = app_name, port = app_port
            )
          } else {
            assign(app_name, tracked_process, envir = port_conflicts)
            logger::log_warn("Port {port} for app {app_name} is held by another program",
              app_name = app_name, port = app_port
            )
          }
          ready <- FALSE
        }
        if (ready) {
          logger::log_info("App {app_name} is ready on port {port} (attempt {attempt})",
            app_name = app_name, port = app_port, attempt = attempt
          )
          config$set_app_ready(app_name) # Mark as ready
          return(TRUE)
        }

        # If not ready and still within the startup window, schedule another
        # check. R has no clock that cannot be set back: a clock set back
        # during the startup would keep the window open for as long as it was
        # set back, so the window starts again instead.
        elapsed <- as.numeric(difftime(Sys.time(), started_at, units = "secs"))
        if (elapsed < 0) {
          started_at <- Sys.time()
          elapsed <- 0
        }
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
        failed <- process_failed(process)
        if (!kill_process_safely(process)) return(FALSE)
        cleanup_app_connections(app_name)
        config$remove_app_process(app_name)
        if (failed) config$note_start_failure(app_name)
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

      # An app whose directory cannot be served, which the landing page shows
      # as unavailable, is not tried: each try would read the directory again
      # and log the same errors, for every request that asks for the app
      if (is.character(app_config$path)) {
        problem <- config$cached_start_problem(app_config)
        if (!is.null(problem)) {
          config$note_start_problem(app_name, problem)
          return(FALSE)
        }
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
          # when it is next opened. One without a process was waiting out
          # restart_delay after a failure, which ends here.
          if (!app_config$resident) {
            return(list(success = TRUE, message = if (is.null(process)) {
              paste("App", app_name, "starts again when it is next opened")
            } else {
              paste("App", app_name, "stopped; it starts again when it is next opened")
            }))
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

      # An app whose process has already exited, such as one that crashed and
      # so closed its sessions, may have failed. The health check would record
      # that, but this idle check can come first, and the app would then start
      # again at the next request instead of after restart_delay.
      process <- config$get_app_process(app_name)
      if (!is.null(process) && !is_process_alive(process)) {
        failed <- process_failed(process)
        if (failed) {
          logger::log_error("Non-resident app {app_name} died", app_name = app_name)
        } else {
          logger::log_info("Non-resident app {app_name} exited", app_name = app_name)
        }
        if (!kill_process_safely(process)) return(FALSE)
        cleanup_app_connections(app_name)
        config$remove_app_process(app_name)
        if (failed) config$note_start_failure(app_name)
        return(TRUE)
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
              # An app that stopped itself, as with shiny::stopApp(), did not
              # fail, and an on-demand one may start again at once
              failed <- process_failed(process)
              if (failed) {
                logger::log_error("App {app_name} died", app_name = app_name)
              } else {
                logger::log_info("App {app_name} exited", app_name = app_name)
              }
              if (!kill_process_safely(process)) next

              # Clean up connections for this app
              cleanup_app_connections(app_name)

              # Remove dead process
              config$remove_app_process(app_name)
              if (failed) config$note_start_failure(app_name)

              # Only restart if it's a resident app
              if (app_config$resident) {
                logger::log_info("Restarting resident app {app_name}", app_name = app_name)
                schedule_restart(app_config)
              } else {
                logger::log_info("Non-resident app {app_name} will start on next request", app_name = app_name)
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

      # Remove closed connections. Their sockets' close handlers then find
      # them gone, so the close is logged here, once.
      for (session_id in sessions_to_remove) {
        if (isTRUE(config$remove_ws_connection(session_id))) {
          logger::log_info("WebSocket connection closed for session {session_id}", session_id = session_id)
        }
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

      timeout_seconds <- timeout_minutes * 60
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
            idle_seconds <- config$session_idle_seconds(session_id, conn_info)
            if (!is.null(idle_seconds) && idle_seconds > timeout_seconds) {
              last_activity <- config$get_session_activity(session_id, conn_info$last_activity)
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
            idle_seconds <- config$session_idle_seconds(session_id, conn_info)
            if (!is.null(idle_seconds) && idle_seconds > timeout_seconds) {
              last_activity <- config$get_session_activity(session_id, conn_info$last_activity)
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
      # A dormant app whose process failed starts again only after
      # restart_delay; a restart from the dashboard ends that wait
      backoff <- if (status == "dormant") config$start_backoff_seconds(app_name) else 0
      if (backoff > 0) {
        app_status$restart_in_seconds <- ceiling(backoff)
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
    cancel_pending_restarts = function() {
      "Keep the restarts that are waiting for restart_delay from starting any app"
      pending_restarts <<- new.env(parent = emptyenv())
      invisible(NULL)
    },
    stop_all_apps = function() {
      "Stop all running applications and close their sessions. Returns the names of apps that did not stop."
      cancel_pending_restarts()

      logger::log_info("Stopping all applications...")

      # Stop all app processes. One that outlives the first attempt, for
      # example because it is slow to exit, is tried once more.
      for (attempt in 1:2) {
        for (app_name in names(config$get_all_app_processes())) {
          stop_app(app_name)
        }
      }

      # A process that is still tracked is still running. Its users stay
      # connected, as they do when a restart cannot stop the app; the
      # sessions of every other app are closed.
      surviving <- names(config$get_all_app_processes())
      closing <- function(conn_info) {
        !is.null(conn_info) && !(length(conn_info$app_name) == 1 && conn_info$app_name %in% surviving)
      }
      for (session_id in names(config$get_all_backend_connections())) {
        conn_info <- config$get_backend_connection(session_id)
        if (!closing(conn_info)) next
        config$remove_backend_connection(session_id)
        if (!is.null(conn_info$ws)) {
          tryCatch(conn_info$ws$close(), error = function(e) {})
        }
      }
      # remove_ws_connection() also updates each app's connection count. The
      # tracking goes first, as a socket's close handler may run at once.
      for (session_id in names(config$get_all_ws_connections())) {
        conn_info <- config$get_ws_connection(session_id)
        if (!closing(conn_info)) next
        if (isTRUE(config$remove_ws_connection(session_id))) {
          if (!is.null(conn_info$ws)) {
            tryCatch(conn_info$ws$close(), error = function(e) {})
          }
          logger::log_info("WebSocket connection closed for session {session_id}", session_id = session_id)
        }
      }

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
