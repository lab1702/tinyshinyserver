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
  local_mocked_bindings(create_server_config = function(...) config, is_port_in_use = function(...) FALSE,
    setup_logging = function(log_dir, log_level = "INFO") level <<- log_level)
  TinyShinyServer$new()
  expect_equal(level, "WARN")
})

test_that("a busy port stops startup before any app or monitoring starts", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = unused_app_path(), resident = TRUE)),
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
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = unused_app_path())),
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
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL,
    is_port_in_use = function(...) FALSE)
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
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL,
    is_port_in_use = function(...) FALSE)
  server <- TinyShinyServer$new()
  handle_server_shutdown(config)
  waits <- numeric()
  local_mocked_bindings(service = function(timeoutMs) {
    waits <<- c(waits, timeoutMs)
    Sys.sleep(timeoutMs / 1000)
  }, .package = "httpuv")
  # The loop serves on briefly, so that httpuv can finish sending the answer
  # to the shutdown request, and then stops
  started <- Sys.time()
  expect_no_error(server$run_event_loop())
  expect_gte(as.numeric(difftime(Sys.time(), started, units = "secs")), SHUTDOWN_RESPONSE_SECONDS - 0.05)
  expect_gt(length(waits), 0)
  expect_true(all(waits == 50))
  expect_true(server$is_shutting_down)
})

test_that("the event loop stops for an interrupt that a WebSocket callback caught", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir())
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL,
    is_port_in_use = function(...) FALSE)
  server <- TinyShinyServer$new()
  stops <- 0
  server$field("process_manager", list(stop_all_apps = function() stops <<- stops + 1))

  # The websocket package catches the interrupt; the callback notes it
  interrupt <- structure(class = c("interrupt", "condition"), list(message = "", call = NULL))
  callback <- note_interrupts(config, function(event) signalCondition(interrupt))
  caught <- tryCatch(callback(list()), interrupt = function(e) "caught by the websocket package")
  expect_equal(caught, "caught by the websocket package")
  expect_true(config$interrupt_requested)

  local_mocked_bindings(service = function(...) stop("Loop must stop before servicing"), .package = "httpuv")
  expect_no_error(server$run_event_loop())
  expect_equal(stops, 1)
  expect_true(server$shutdown_complete)
})

test_that("requests that reach a shutting-down server start nothing", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = unused_app_path())),
    log_dir = tempdir(), starting_port = 3001), auto_unbox = TRUE), path)
  local_mocked_bindings(is_port_in_use = function(...) FALSE, setup_logging = function(...) NULL,
    create_process_manager = function(config) list())
  server <- TinyShinyServer$new(path)
  apps <- list()
  local_mocked_bindings(startServer = function(host, port, app, ...) {
    apps[[as.character(port)]] <<- app
    list(port = port)
  }, .package = "httpuv")
  server$start_servers()
  local_mocked_bindings(
    handle_http_request = function(...) stop("A proxy request was handled"),
    handle_websocket_connection = function(...) stop("A WebSocket was handled"),
    handle_management_request = function(...) stop("A management request was handled")
  )
  server$is_shutting_down <- TRUE

  expect_equal(apps[["3838"]]$call(list())$status, 503)
  expect_equal(apps[["3839"]]$call(list())$status, 503)
  closed <- 0
  apps[["3838"]]$onWSOpen(list(close = function() closed <<- closed + 1))
  expect_equal(closed, 1)
})

test_that("an event-loop error shuts the server down once", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir())
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL,
    is_port_in_use = function(...) FALSE)
  server <- TinyShinyServer$new()
  stops <- 0
  server$field("process_manager", list(stop_all_apps = function() stops <<- stops + 1))
  local_mocked_bindings(service = function(...) stop("loop failure"), .package = "httpuv")
  # The error is raised again once the server has stopped, so that Rscript
  # exits with a failure status
  expect_error(server$run_event_loop(), "stopped after an error: loop failure")
  expect_equal(stops, 1)
  expect_true(server$shutdown_complete)
  # A completed shutdown does nothing when start() calls it again on exit
  server$shutdown()
  expect_equal(stops, 1)
  # Nor can the server start again, since it could not be shut down again
  expect_error(server$start(), "has been shut down")
  expect_equal(stops, 1)
})

test_that("a shutdown that was interrupted runs again", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir())
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL,
    is_port_in_use = function(...) FALSE)
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
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL,
    is_port_in_use = function(...) FALSE)
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
  local_mocked_bindings(create_server_config = function(...) config, setup_logging = function(...) NULL,
    is_port_in_use = function(...) FALSE)
  server <- TinyShinyServer$new()
  stopped <- FALSE
  server$field("process_manager", list(stop_all_apps = function() stopped <<- TRUE))
  expect_no_error(server$shutdown())
  expect_true(stopped)
})

test_that("a server whose ports are in use stops before it writes to the server log", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = unused_app_path())),
    log_dir = tempdir(), starting_port = 3001), auto_unbox = TRUE), path)
  logging <- 0
  local_mocked_bindings(setup_logging = function(...) logging <<- logging + 1,
    is_port_in_use = function(host, port, ...) port == 3839,
    loopback_port_accepts = function(port, ...) port == 3839)
  expect_error(TinyShinyServer$new(path), "management port 3839 is already in use")
  expect_equal(logging, 0)
})

test_that("the server's ports are checked for programs on either loopback address", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = unused_app_path())),
    log_dir = tempdir(), starting_port = 3001, proxy_host = "0.0.0.0"), auto_unbox = TRUE), path)
  asked <- list()
  local_mocked_bindings(setup_logging = function(...) stop("must not log"),
    is_port_in_use = function(host, port, loopback = FALSE) {
      asked[[length(asked) + 1]] <<- list(host = host, port = port, loopback = loopback)
      port == 3838
    },
    loopback_port_accepts = function(port, timeout_ms = 100, address = "127.0.0.1") address == "::1")
  expect_error(TinyShinyServer$new(path), "proxy port 3838 is already in use by another program")
  # App ports were checked first, as the configuration was read
  proxy_checks <- Filter(function(check) check$port == 3838, asked)
  expect_equal(proxy_checks, list(list(host = "0.0.0.0", port = 3838, loopback = TRUE)))
})

test_that("a server port that nothing answers on is not reported as in use", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = unused_app_path())),
    log_dir = tempdir(), starting_port = 3001, proxy_port = 80), auto_unbox = TRUE), path)
  # As when a port below 1024 needs rights the user lacks
  local_mocked_bindings(setup_logging = function(...) stop("must not log"),
    is_port_in_use = function(host, port, ...) port == 80,
    loopback_port_accepts = function(...) FALSE)
  error <- expect_error(TinyShinyServer$new(path), "proxy port 80 could not be opened at 127.0.0.1")
  expect_no_match(conditionMessage(error), "already in use")
  expect_match(conditionMessage(error), "not available to this user or on this machine", fixed = TRUE)
})

test_that("a port that the server cannot open is named in the server log", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = unused_app_path())),
    log_dir = tempdir(), starting_port = 3001), auto_unbox = TRUE), path)
  local_mocked_bindings(is_port_in_use = function(...) FALSE, setup_logging = function(...) NULL,
    create_process_manager = function(config) list(stop_all_apps = function() NULL))
  server <- TinyShinyServer$new(path)
  local_mocked_bindings(startServer = function(...) stop("Failed to create server"), .package = "httpuv")
  logged <- capture_test_log(expect_error(server$start(), "Failed to create server"))
  expect_true(any(grepl("Could not start the server: Failed to create server", logged, fixed = TRUE)))
})
