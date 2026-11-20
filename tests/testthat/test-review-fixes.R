# Tests for the fixes that followed the full-package review

restore_test_logging <- function(log_dir) {
  logger::log_threshold("INFO", namespace = "tinyshinyserver")
  logger::log_appender(logger::appender_console, namespace = "tinyshinyserver")
  unlink(log_dir, recursive = TRUE)
}

# ============================================================================
# App directories
# ============================================================================

test_that("detect_app_type names what a directory holds, in order of preference", {
  app_dir <- tempfile("tss-detect")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  add <- function(...) for (name in c(...)) writeLines("", file.path(app_dir, name))
  clear <- function() unlink(list.files(app_dir, full.names = TRUE))

  add("b.qmd", "a.qmd")
  expect_equal(detect_app_type(app_dir)$type, "qmd")
  expect_equal(basename(detect_app_type(app_dir)$file), "a.qmd")

  add("z.Rmd", "report.Rmd")
  expect_equal(detect_app_type(app_dir)$type, "rmd")
  expect_equal(basename(detect_app_type(app_dir)$file), "report.Rmd")

  add("ui.R")
  expect_equal(detect_app_type(app_dir)$type, "rmd")
  add("server.R")
  expect_equal(detect_app_type(app_dir), list(type = "shiny"))

  clear()
  add("app.R", "notes.Rmd")
  expect_equal(detect_app_type(app_dir), list(type = "shiny"))

  # shiny::runApp() serves a server.R whose page is www/index.html
  clear()
  add("server.R")
  expect_equal(detect_app_type(app_dir), list(type = "shiny"))
})

test_that("detect_app_type explains why a directory cannot be served", {
  missing <- file.path(tempdir(), "tss-no-such-app")
  expect_error(detect_app_type(missing), "App directory not found: .*tss-no-such-app")

  empty <- tempfile("tss-empty")
  dir.create(empty)
  on.exit(unlink(empty, recursive = TRUE), add = TRUE)
  writeLines("", file.path(empty, "README.md"))
  expect_error(detect_app_type(empty), "No app.R, ui.R and server.R, .Rmd, or .qmd file found in")
})

test_that("an app whose directory is missing starts no process", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "ghost", path = file.path(tempdir(), "tss-no-such-app"),
    port = 1, resident = FALSE)), log_dir = tempdir())
  local_mocked_bindings(r_bg = function(...) stop("No process may be started"), .package = "callr")
  pm <- ProcessManager$new(config)

  expect_error(pm$start_app(config$config$apps[[1]]), "App directory not found")
  expect_null(config$get_app_process("ghost"))
  expect_false(config$is_app_starting("ghost"))
  # A visitor's request reports the failure instead of raising it
  expect_false(pm$start_app_on_demand("ghost"))
})

test_that("on-demand apps that cannot start are reported when the configuration is loaded", {
  log_dir <- tempfile("tss-logging")
  app_dir <- make_test_app_dir()
  on.exit(restore_test_logging(log_dir), add = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  setup_logging(log_dir, "INFO")
  config <- ShinyServerConfig$new()
  missing <- file.path(tempdir(), "tss-no-such-app")
  config$config <- list(apps = list(
    list(name = "good", path = app_dir, resident = FALSE),
    list(name = "ghost", path = missing, resident = FALSE),
    # A resident app reports the problem itself when it is started
    list(name = "resident-ghost", path = missing, resident = TRUE)
  ))
  config$log_unusable_apps()

  warnings <- grep("cannot start", readLines(file.path(log_dir, "server.log")), value = TRUE)
  expect_length(warnings, 1)
  expect_match(warnings, "On-demand app ghost cannot start: App directory not found")
})

# ============================================================================
# Quarto rendering
# ============================================================================

test_that("a Quarto document is rendered only when its page is out of date", {
  app_dir <- tempfile("tss-qmd")
  dir.create(file.path(app_dir, "doc_files", "libs"), recursive = TRUE)
  dir.create(file.path(app_dir, "data"))
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  html <- file.path(app_dir, "doc.html")
  data <- file.path(app_dir, "data", "sales.csv")
  generated <- file.path(app_dir, "doc_files", "libs", "lib.js")
  for (file in c(qmd, data, generated)) writeLines("", file)
  now <- Sys.time()
  age <- function(file, seconds) Sys.setFileTime(file, now - seconds)

  # Never rendered
  expect_true(quarto_needs_render(qmd))

  writeLines("", html)
  age(qmd, 300)
  age(data, 300)
  age(html, 200)
  # What rendering writes is newer than the page without making it stale
  age(generated, 100)
  expect_false(quarto_needs_render(qmd))

  age(qmd, 100)
  expect_true(quarto_needs_render(qmd))
  age(qmd, 300)
  expect_false(quarto_needs_render(qmd))

  # A file the document reads has changed
  age(data, 100)
  expect_true(quarto_needs_render(qmd))

  # A change in the same instant as the page may have come after it
  age(data, 200)
  expect_true(quarto_needs_render(qmd))
})

test_that("start_app tells a Quarto app whether to render first", {
  app_dir <- tempfile("tss-qmd")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  writeLines("", qmd)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = 1, resident = TRUE)),
    log_dir = tempdir())
  launched <- NULL
  local_mocked_bindings(r_bg = function(func, args, ...) {
    launched <<- args
    list(is_alive = function() TRUE)
  }, .package = "callr")
  pm <- ProcessManager$new(config)
  start <- function() {
    pm$start_app(config$config$apps[[1]])
    # Retire the fake process so its scheduled readiness check does nothing
    config$remove_app_process("app")
  }

  start()
  expect_equal(launched$type, "qmd")
  expect_equal(basename(launched$file), "doc.qmd")
  expect_true(launched$render)

  writeLines("", file.path(app_dir, "doc.html"))
  Sys.setFileTime(qmd, Sys.time() - 300)
  start()
  expect_false(launched$render)
})

# ============================================================================
# Configuration options
# ============================================================================

read_test_config <- function(...) {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  settings <- utils::modifyList(
    list(apps = list(list(name = "app", path = "/tmp")), log_dir = tempdir(), starting_port = 3001),
    list(...)
  )
  writeLines(jsonlite::toJSON(settings, auto_unbox = TRUE), path)
  ShinyServerConfig$new()$read_config(path)
}

test_that("idle_timeout_minutes defaults to 30 and accepts zero", {
  expect_equal(read_test_config()$idle_timeout_minutes, 30)
  expect_equal(read_test_config(idle_timeout_minutes = 0)$idle_timeout_minutes, 0)
  expect_equal(read_test_config(idle_timeout_minutes = 0.5)$idle_timeout_minutes, 0.5)
  for (value in list(-1, "30", c(1, 2))) {
    expect_error(read_test_config(idle_timeout_minutes = value),
      "idle_timeout_minutes must be a single non-negative finite number of minutes")
  }
})

test_that("log_level defaults to INFO and is normalized to upper case", {
  expect_equal(read_test_config()$log_level, "INFO")
  expect_equal(read_test_config(log_level = "debug")$log_level, "DEBUG")
  expect_equal(read_test_config(log_level = "WARN")$log_level, "WARN")
  for (value in list("TRACE", "", 3, c("INFO", "DEBUG"))) {
    expect_error(read_test_config(log_level = value), "log_level must be one of: DEBUG, INFO, WARN, ERROR")
  }
})

test_that("the new options are known configuration fields", {
  expect_length(unrecognized_config_fields(list(idle_timeout_minutes = 0, log_level = "DEBUG", apps = list())), 0)
})

test_that("the configured log level reaches the server log", {
  log_dir <- tempfile("tss-logging")
  on.exit(restore_test_logging(log_dir), add = TRUE)
  config <- ShinyServerConfig$new()
  debug_lines <- function() grep("Cache: Added NEW connection", readLines(file.path(log_dir, "server.log")))

  setup_logging(log_dir, "INFO")
  config$add_ws_connection("quiet", list(app_name = "app"))
  expect_length(debug_lines(), 0)

  setup_logging(log_dir, "DEBUG")
  config$add_ws_connection("detailed", list(app_name = "app"))
  expect_length(debug_lines(), 1)
})

test_that("the server starts logging at the configured level", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir(), log_level = "WARN")
  level <- NULL
  local_mocked_bindings(create_server_config = function(...) config,
    setup_logging = function(log_dir, log_level = "INFO") level <<- log_level)
  TinyShinyServer$new()
  expect_equal(level, "WARN")
})

# ============================================================================
# start_tss()
# ============================================================================

test_that("start_tss checks its argument before starting anything", {
  local_mocked_bindings(create_server_config = function(...) stop("Nothing may be started"))
  for (value in list(NULL, NA_character_, "", 1, c("a.json", "b.json"), list("a.json"))) {
    expect_error(start_tss(value), "`config` must be the path to a configuration file, as a single string",
      fixed = TRUE)
  }
  expect_error(start_tss(file.path(tempdir(), "tss-no-such-config.json")), "Configuration file not found")
})

# ============================================================================
# WebSocket sessions
# ============================================================================

test_that("a WebSocket path that names no app is rejected like an HTTP path", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 1, resident = TRUE)))
  for (path in c("/proxy//app/websocket/", "/proxy/", "/websocket/")) {
    closed <- FALSE
    ws <- list(request = list(PATH_INFO = path, HTTP_HOST = "127.0.0.1:3838"),
      close = function() closed <<- TRUE)
    handle_websocket_connection(ws, config, ConnectionManager$new(config))
    expect_true(closed, info = path)
    expect_length(config$get_all_ws_connections(), 0)
  }
})

test_that("a message for a session without an app socket is refused", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 1, resident = TRUE)))
  cm <- ConnectionManager$new(config)
  cm$add_client_connection("s", list(), "app", "127.0.0.1", "test")
  local_mocked_bindings(WebSocket = list(new = function(...) stop("No app socket may be opened")),
    .package = "websocket")
  expect_false(cm$handle_client_message("s", "{}", "app"))
  expect_length(config$get_all_backend_connections(), 0)
})

test_that("the management API reports a session's latest message as its last activity", {
  config <- ShinyServerConfig$new()
  opened <- as.POSIXct("2026-01-02 03:04:05", tz = "UTC")
  config$add_ws_connection("s", list(app_name = "app", client_ip = "127.0.0.1", user_agent = "test",
    created_at = opened, last_activity = opened))
  connection <- function() jsonlite::fromJSON(handle_management_connections_api(config)$body)$s

  expect_equal(connection()$last_activity, format(opened, "%Y-%m-%d %H:%M:%S"))
  assign("s", opened + 90, envir = config$session_activity)
  expect_equal(connection()$last_activity, format(opened + 90, "%Y-%m-%d %H:%M:%S"))
  expect_equal(connection()$connected_at, format(opened, "%Y-%m-%d %H:%M:%S"))
})
