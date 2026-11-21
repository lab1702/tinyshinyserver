# Tests for the TinyShinyServer class and start_tss()

# ============================================================================
# Startup
# ============================================================================

test_that("start_tss checks its argument before starting anything", {
  local_mocked_bindings(create_server_config = function(...) stop("Nothing may be started"))
  for (value in list(NULL, NA_character_, "", 1, c("a.json", "b.json"), list("a.json"))) {
    expect_error(start_tss(value), "`config` must be the path to a configuration file, as a single string",
      fixed = TRUE)
  }
  expect_error(start_tss(file.path(tempdir(), "tss-no-such-config.json")), "Configuration file not found")
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

test_that("a busy port stops startup before any app or monitoring starts", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = "/tmp", resident = TRUE)),
    log_dir = tempdir(), starting_port = 3001), auto_unbox = TRUE), path)
  started_apps <- 0
  stopped_apps <- 0
  health_checks <- 0
  local_mocked_bindings(is_port_in_use = function(...) FALSE, setup_logging = function(...) NULL,
    create_process_manager = function(config) list(
      start_app = function(app) {started_apps <<- started_apps + 1; TRUE},
      stop_all_apps = function() {stopped_apps <<- stopped_apps + 1},
      health_check = function() {health_checks <<- health_checks + 1}
    ))
  server <- TinyShinyServer$new(path)
  calls <- 0
  stopped_servers <- 0
  local_mocked_bindings(
    startServer = function(...) {
      calls <<- calls + 1
      if (calls == 2) stop("management port busy")
      list(proxy = TRUE)
    },
    stopServer = function(...) {stopped_servers <<- stopped_servers + 1}, .package = "httpuv"
  )
  scheduled <- list()
  local_mocked_bindings(later = function(func, ...) {
    scheduled[[length(scheduled) + 1]] <<- func
  }, .package = "later")
  later::with_loop(later::create_loop(), {
    expect_error(server$start(), "management port busy")
    expect_true(server$is_shutting_down)
    expect_null(server$proxy_server)
    expect_null(server$management_server)
    # The ports are bound first, so a second server on the same ports never
    # starts app processes or rotates the logs the first one is writing
    expect_equal(started_apps, 0)
    expect_equal(stopped_apps, 1)
    expect_equal(stopped_servers, 1)
    expect_length(scheduled, 0)
    expect_equal(health_checks, 0)
  })
})

test_that("started servers limit request bodies from their headers", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = "/tmp")),
    log_dir = tempdir(), starting_port = 3001, max_request_size_mb = 2), auto_unbox = TRUE), path)
  local_mocked_bindings(is_port_in_use = function(...) FALSE, setup_logging = function(...) NULL,
    create_process_manager = function(config) list())
  server <- TinyShinyServer$new(path)
  apps <- list()
  local_mocked_bindings(startServer = function(host, port, app, ...) {
    apps[[as.character(port)]] <<- app
    list(port = port)
  }, .package = "httpuv")
  server$start_servers()
  status <- function(app, size) app$onHeaders(list(CONTENT_LENGTH = format(size, scientific = FALSE)))$status
  expect_null(status(apps[["3838"]], 2 * 1024^2))
  expect_equal(status(apps[["3838"]], 2 * 1024^2 + 1), 413)
  expect_null(status(apps[["3839"]], 65536))
  expect_equal(status(apps[["3839"]], 65537), 413)
})

# ============================================================================
# Monitoring, event loop and shutdown
# ============================================================================

test_that("monitoring continues after an unexpected health-check error", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir())
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL)
  server <- TinyShinyServer$new()
  assign("health_check", function() stop("Unexpected failure"), envir = server$process_manager)
  scheduled <- list()
  local_mocked_bindings(later = function(func, ...) scheduled[[length(scheduled) + 1]] <<- func, .package = "later")
  server$start_monitoring_services()
  expect_length(scheduled, 2)
  expect_no_error(scheduled[[1]]())
  expect_length(scheduled, 3)
  expect_false(server$is_shutting_down)
  server$is_shutting_down <- TRUE
  scheduled[[3]]()
  expect_length(scheduled, 3)
})

test_that("the event loop stops when the management API requests shutdown", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir())
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL)
  server <- TinyShinyServer$new()
  handle_server_shutdown(config)
  local_mocked_bindings(service = function(...) stop("Loop must stop before servicing"), .package = "httpuv")
  server$run_event_loop()
  expect_true(server$is_shutting_down)
})

test_that("an event-loop error shuts the server down once", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir())
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL)
  server <- TinyShinyServer$new()
  stops <- 0
  server$field("process_manager", list(stop_all_apps = function() stops <<- stops + 1))
  local_mocked_bindings(service = function(...) stop("loop failure"), .package = "httpuv")
  expect_no_error(server$run_event_loop())
  expect_equal(stops, 1)
  expect_true(server$shutdown_complete)
  # A completed shutdown does nothing when start() calls it again on exit
  server$shutdown()
  expect_equal(stops, 1)
})

test_that("a shutdown that was interrupted runs again", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir())
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL)
  server <- TinyShinyServer$new()
  attempts <- 0
  server$field("process_manager", list(stop_all_apps = function() {
    attempts <<- attempts + 1
    if (attempts == 1) stop("interrupted while stopping apps")
  }))
  expect_error(server$shutdown(), "interrupted while stopping apps")
  expect_false(server$shutdown_complete)
  server$shutdown()
  expect_equal(attempts, 2)
  expect_true(server$shutdown_complete)
})

test_that("shutdown names the apps it could not stop", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir())
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL)
  server <- TinyShinyServer$new()
  # A process that is still tracked after stop_all_apps() is still running
  config$add_app_process("stuck", list())
  server$field("process_manager", list(stop_all_apps = function() NULL))

  logged <- capture_test_log(server$shutdown())
  expect_true(any(grepl("these apps are still running: stuck", logged, fixed = TRUE)))
  expect_false(any(grepl("Server shutdown complete$", logged)))
  expect_true(server$shutdown_complete)

  config$remove_app_process("stuck")
  server$field("shutdown_complete", FALSE)
  logged <- capture_test_log(server$shutdown())
  expect_true(any(grepl("Server shutdown complete$", logged)))
})

test_that("an unwritable server log does not stop logging or shutdown", {
  log_dir <- tempfile("tss-logging")
  on.exit({
    logger::log_appender(logger::appender_console, namespace = "tinyshinyserver")
    unlink(log_dir, recursive = TRUE)
  }, add = TRUE)
  setup_logging(log_dir, "INFO")
  unlink(log_dir, recursive = TRUE)
  expect_no_error(logger::log_info("After the log directory is removed", namespace = "tinyshinyserver"))

  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = log_dir)
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL)
  server <- TinyShinyServer$new()
  stopped <- FALSE
  server$field("process_manager", list(stop_all_apps = function() stopped <<- TRUE))
  expect_no_error(server$shutdown())
  expect_true(stopped)
})
