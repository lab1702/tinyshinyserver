# Tests for reloading the configuration and restarting all apps

write_reload_config <- function(path, apps, ...) {
  settings <- utils::modifyList(list(apps = apps, log_dir = tempdir(), starting_port = 3001), list(...))
  writeLines(jsonlite::toJSON(settings, auto_unbox = TRUE), path)
}

reload_test_server <- function(path, env = parent.frame()) {
  calls <- new.env()
  calls$stopped <- 0
  calls$started <- character()
  local_mocked_bindings(is_port_in_use = function(...) FALSE, setup_logging = function(...) NULL,
    create_process_manager = function(config) list(
      start_app = function(app) {calls$started <- c(calls$started, app$name); TRUE},
      stop_all_apps = function() {calls$stopped <- calls$stopped + 1}
    ), .env = env)
  server <- TinyShinyServer$new(path)
  list(server = server, calls = calls)
}

test_that("read_reload_config reads the loaded file again with defaults", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "one", path = "/tmp")))
  local_mocked_bindings(is_port_in_use = function(...) FALSE)
  config <- create_server_config(path)
  # An absolute path is kept as given, links and all
  expect_equal(config$config_file, path)

  write_reload_config(path, list(list(name = "two", path = "/tmp", resident = TRUE)), title = "  New  ")
  check <- config$read_reload_config()
  expect_true(check$valid)
  expect_equal(check$config$apps[[1]]$name, "two")
  expect_equal(check$config$apps[[1]]$appstart_timeout, 2)
  expect_equal(check$config$title, "New")
  # Reading is not applying
  expect_equal(config$config$apps[[1]]$name, "one")
})

test_that("read_reload_config reports invalid files", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "one", path = "/tmp")))
  local_mocked_bindings(is_port_in_use = function(...) FALSE)
  config <- create_server_config(path)

  writeLines("{ invalid json }", path)
  expect_false(config$read_reload_config()$valid)

  write_reload_config(path, list(list(name = "bad name", path = "/tmp")))
  check <- config$read_reload_config()
  expect_false(check$valid)
  expect_match(check$error, "validation failed")

  unlink(path)
  check <- config$read_reload_config()
  expect_false(check$valid)
  expect_match(check$error, "not found")

  expect_false(ShinyServerConfig$new()$read_reload_config()$valid)
})

test_that("read_reload_config rejects listener changes", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  apps <- list(list(name = "one", path = "/tmp"))
  write_reload_config(path, apps, proxy_host = "localhost")
  local_mocked_bindings(is_port_in_use = function(...) FALSE)
  config <- create_server_config(path)

  # Explicit defaults and localhost for 127.0.0.1 are not changes
  write_reload_config(path, apps, proxy_host = "127.0.0.1", proxy_port = 3838, management_port = 3839)
  expect_true(config$read_reload_config()$valid)

  write_reload_config(path, apps, proxy_port = 4000)
  check <- config$read_reload_config()
  expect_false(check$valid)
  expect_equal(check$error, "proxy_port changed; restart the server to apply")

  write_reload_config(path, apps, proxy_host = "0.0.0.0", management_port = 4001)
  expect_equal(config$read_reload_config()$error,
    "proxy_host, management_port changed; restart the server to apply")
})

test_that("read_reload_config creates a new log_dir and rejects one it cannot write", {
  path <- tempfile(fileext = ".json")
  blocker <- tempfile()
  new_logs <- tempfile()
  on.exit(unlink(c(path, blocker, new_logs), recursive = TRUE), add = TRUE)
  apps <- list(list(name = "one", path = "/tmp"))
  write_reload_config(path, apps)
  local_mocked_bindings(is_port_in_use = function(...) FALSE)
  config <- create_server_config(path)

  write_reload_config(path, apps, log_dir = new_logs)
  expect_true(config$read_reload_config()$valid)
  expect_true(dir.exists(new_logs))

  # A directory cannot be created below a regular file
  writeLines("", blocker)
  write_reload_config(path, apps, log_dir = file.path(blocker, "logs"))
  check <- config$read_reload_config()
  expect_false(check$valid)
  expect_match(check$error, "Cannot write the server log")

  # The log_dir that the server already uses is checked as well: it may
  # have become unwritable since the server started
  write_reload_config(path, apps, log_dir = new_logs)
  config$config$log_dir <- new_logs
  local_mocked_bindings(check_log_dir = function(log_dir) paste("Cannot write the server log to", log_dir))
  check <- config$read_reload_config()
  expect_false(check$valid)
  expect_match(check$error, "Cannot write the server log")
})

test_that("reload endpoint rejects a bad file without requesting a reload", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "one", path = "/tmp")))
  local_mocked_bindings(is_port_in_use = function(...) FALSE)
  config <- create_server_config(path)
  req <- list(HTTP_X_TINYSHINYSERVER_REQUEST = "management")

  write_reload_config(path, list(list(name = "one", path = "/tmp")), proxy_port = 4000)
  result <- route_management_request("/api/reload", "POST", req, config, list(), list())
  expect_equal(result$status, 400)
  body <- jsonlite::fromJSON(result$body)
  expect_false(body$success)
  expect_match(body$message, "proxy_port changed")
  expect_false(config$reload_requested)

  write_reload_config(path, list(list(name = "two", path = "/tmp")))
  result <- route_management_request("/api/reload", "POST", req, config, list(), list())
  expect_equal(result$status, 200)
  expect_true(jsonlite::fromJSON(result$body)$success)
  expect_true(config$reload_requested)
  # The event loop applies it; the request only asks
  expect_equal(config$config$apps[[1]]$name, "one")

  expect_equal(route_management_request("/api/reload", "POST", list(), config, list(), list())$status, 403)
})

test_that("a reload names the configuration fields that had no effect", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "app", path = "/tmp", resident = TRUE)))
  test <- reload_test_server(path)
  config <- test$server$config

  write_reload_config(path, list(list(name = "app", path = "/tmp", resdient = TRUE)), restart_dealy = 1)
  test$server$reload()
  expect_true(config$last_reload$success)
  expect_equal(config$last_reload$message,
    'Configuration reloaded; ignored unrecognized fields: "restart_dealy", "resdient" in app "app"')
})

test_that("the status API reports the latest reload outcome", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "app", path = "/tmp", resident = TRUE)))
  test <- reload_test_server(path)
  config <- test$server$config
  req <- list(HTTP_X_TINYSHINYSERVER_REQUEST = "management")
  status <- function() {
    jsonlite::fromJSON(route_management_request("/api/status", "GET", list(), config, list(), list())$body)
  }
  expect_null(status()$last_reload)

  test$server$reload()
  expect_equal(status()$last_reload, list(success = TRUE, message = "Configuration reloaded"))

  # A requested reload clears the previous outcome until it runs
  route_management_request("/api/reload", "POST", req, config, list(), list())
  expect_null(status()$last_reload)

  config$add_app_process("app", list(is_alive = function() TRUE))
  test$server$reload()
  expect_equal(status()$last_reload, list(success = FALSE, message = "Could not stop apps: app"))

  write_reload_config(path, list(list(name = "app", path = "/tmp")), proxy_port = 4000)
  test$server$reload()
  expect_false(status()$last_reload$success)
  expect_match(status()$last_reload$message, "proxy_port changed")
})

test_that("reload stops everything, applies the file, and starts resident apps", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "old", path = "/tmp", resident = TRUE)))
  test <- reload_test_server(path)
  server <- test$server
  config <- server$config

  closed <- 0
  ws <- list(close = function() closed <<- closed + 1)
  server$connection_manager$add_client_connection("s1", ws, "old", "127.0.0.1", "agent")
  expect_equal(config$get_app_connection_count("old"), 1)
  config$set_app_starting("old")

  write_reload_config(path, list(
    list(name = "resident", path = "/tmp", resident = TRUE),
    list(name = "ondemand", path = "/tmp")
  ), title = "Reloaded", starting_port = 4001, base_path = "/apps/")
  result <- server$reload()

  expect_true(result$valid)
  expect_equal(closed, 1)
  expect_length(config$get_all_ws_connections(), 0)
  expect_equal(config$get_app_connection_count("old"), 0)
  expect_false(config$is_app_starting("old"))
  expect_equal(test$calls$stopped, 1)
  expect_equal(test$calls$started, "resident")
  expect_equal(vapply(config$config$apps, function(app) app$name, ""), c("resident", "ondemand"))
  expect_equal(vapply(config$config$apps, function(app) app$port, 0), c(4001, 4002))
  expect_equal(server$template_manager$server_title, "Reloaded")
  expect_equal(config$config$base_path, "/apps")
  expect_equal(server$template_manager$base_url, "/apps")
})

test_that("reload finishes when the new log_dir stops being writable after its check", {
  path <- tempfile(fileext = ".json")
  new_logs <- tempfile("tss-new-logs")
  on.exit(unlink(c(path, new_logs), recursive = TRUE), add = TRUE)
  write_reload_config(path, list(list(name = "old", path = "/tmp", resident = TRUE)))
  test <- reload_test_server(path)
  local_mocked_bindings(setup_logging = function(...) stop("Cannot write the server log"))

  write_reload_config(path, list(list(name = "resident", path = "/tmp", resident = TRUE)),
    title = "Reloaded", log_dir = new_logs)
  result <- test$server$reload()

  expect_true(result$valid)
  expect_equal(test$calls$started, "resident")
  expect_equal(test$server$template_manager$server_title, "Reloaded")
  expect_true(test$server$config$last_reload$success)
  expect_match(test$server$config$last_reload$message, "kept the previous server log: Cannot write the server log")
})

test_that("a reload with the real process manager stops an app that the file no longer names", {
  app_dir <- make_test_app_dir()
  path <- tempfile(fileext = ".json")
  on.exit(unlink(c(path, app_dir), recursive = TRUE), add = TRUE)
  write_reload_config(path, list(
    list(name = "removed", path = app_dir, resident = TRUE),
    list(name = "kept", path = app_dir, resident = TRUE)
  ), restart_delay = 0)
  local_mocked_bindings(is_port_in_use = function(...) FALSE, setup_logging = function(...) NULL)
  started <- character()
  local_mocked_bindings(r_bg = function(func, args, ...) {
    started <<- c(started, args$port)
    alive <- TRUE
    list(is_alive = function() alive, kill = function() alive <<- FALSE, get_pid = function() 1L)
  }, .package = "callr")
  server <- TinyShinyServer$new(path)
  config <- server$config
  on.exit(config$app_processes <- list(), add = TRUE, after = FALSE)
  server$start_all_apps()
  removed <- config$get_app_process("removed")
  expect_true(removed$is_alive())

  write_reload_config(path, list(list(name = "kept", path = app_dir, resident = TRUE)), restart_delay = 0)
  expect_true(server$reload()$valid)
  expect_false(removed$is_alive())
  expect_named(config$get_all_app_processes(), "kept")
  # Nothing that was scheduled for the removed app brings it back
  server$process_manager$health_check()
  later::run_now(0.1)
  expect_named(config$get_all_app_processes(), "kept")
})

test_that("a reload applies a new log level even when it keeps the previous server log", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  on.exit(logger::log_threshold("INFO", namespace = "tinyshinyserver"), add = TRUE)
  write_reload_config(path, list(list(name = "app", path = "/tmp")))
  test <- reload_test_server(path)
  logger::log_threshold("INFO", namespace = "tinyshinyserver")
  local_mocked_bindings(setup_logging = function(...) stop("Cannot write the server log"))

  write_reload_config(path, list(list(name = "app", path = "/tmp")), log_level = "ERROR")
  expect_true(test$server$reload()$valid)
  expect_equal(attr(logger::log_threshold(namespace = "tinyshinyserver"), "level"), "ERROR")
})

test_that("an app that fails to start does not stop the other resident apps or the reload", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "old", path = "/tmp", resident = TRUE)))
  test <- reload_test_server(path)
  start_app <- test$server$process_manager$start_app
  test$server$field("process_manager", utils::modifyList(test$server$process_manager, list(
    start_app = function(app) if (app$name == "broken") stop("cannot start processx process") else start_app(app)
  )))

  write_reload_config(path, list(
    list(name = "broken", path = "/tmp", resident = TRUE),
    list(name = "working", path = "/tmp", resident = TRUE)
  ), title = "Reloaded")
  result <- test$server$reload()

  expect_true(result$valid)
  expect_equal(test$calls$started, "working")
  expect_equal(test$server$template_manager$server_title, "Reloaded")
})

test_that("reload leaves apps running when the file is rejected", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "app", path = "/tmp", resident = TRUE)))
  test <- reload_test_server(path)

  write_reload_config(path, list(list(name = "app", path = "/tmp")), management_port = 4000)
  result <- test$server$reload()

  expect_false(result$valid)
  expect_equal(test$calls$stopped, 0)
  expect_length(test$calls$started, 0)
  expect_true(test$server$config$config$apps[[1]]$resident)
})

test_that("reload restarts the previous configuration when ports cannot be assigned", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "app", path = "/tmp", resident = TRUE)))
  test <- reload_test_server(path)

  write_reload_config(path, list(list(name = "new", path = "/tmp", resident = TRUE)))
  local_mocked_bindings(is_port_in_use = function(...) TRUE)
  result <- test$server$reload()

  expect_false(result$valid)
  expect_match(result$error, "Port assignment failed")
  expect_equal(test$server$config$config$apps[[1]]$name, "app")
  expect_equal(test$server$config$config$apps[[1]]$port, 3001)
  expect_equal(test$calls$started, "app")
})

test_that("reload keeps the previous configuration when an app does not stop", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(
    list(name = "stuck", path = "/tmp", resident = TRUE),
    list(name = "other", path = "/tmp", resident = TRUE)
  ))
  test <- reload_test_server(path)
  config <- test$server$config
  # The mocked stop_all_apps leaves this process registered, as a failed stop does
  config$add_app_process("stuck", list(is_alive = function() TRUE))

  write_reload_config(path, list(list(name = "new", path = "/tmp", resident = TRUE)), starting_port = 4001)
  result <- test$server$reload()

  expect_false(result$valid)
  expect_equal(result$error, "Could not stop apps: stuck")
  expect_equal(test$calls$stopped, 1)
  expect_equal(vapply(config$config$apps, function(app) app$name, ""), c("stuck", "other"))
  expect_equal(vapply(config$config$apps, function(app) app$port, 0), c(3001, 3002))
  # The surviving app is not started a second time
  expect_equal(test$calls$started, "other")
})

test_that("reload applies a changed log_level", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "app", path = "/tmp")))
  levels <- character()
  local_mocked_bindings(is_port_in_use = function(...) FALSE,
    setup_logging = function(log_dir, log_level = "INFO") levels <<- c(levels, log_level),
    create_process_manager = function(config) list(
      start_app = function(app) TRUE, stop_all_apps = function() NULL
    ))
  server <- TinyShinyServer$new(path)
  expect_equal(levels, "INFO")

  write_reload_config(path, list(list(name = "app", path = "/tmp")), log_level = "debug")
  expect_true(server$reload()$valid)
  expect_equal(levels, c("INFO", "DEBUG"))

  # Unchanged logging settings leave the logger alone
  expect_true(server$reload()$valid)
  expect_equal(levels, c("INFO", "DEBUG"))
})

test_that("the event loop runs a requested reload", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "app", path = "/tmp")))
  server <- reload_test_server(path)$server
  reloads <- 0
  # Install the loop method before replacing the method it calls
  run_event_loop <- server$run_event_loop
  assign("reload", function() {
    reloads <<- reloads + 1
    server$config$shutdown_requested <- TRUE
    stop("reload failure is logged, not fatal")
  }, envir = server)
  local_mocked_bindings(service = function(...) NULL, .package = "httpuv")

  server$config$reload_requested <- TRUE
  run_event_loop()

  expect_equal(reloads, 1)
  expect_false(server$config$reload_requested)
  expect_equal(server$config$last_reload, list(success = FALSE, message = "reload failure is logged, not fatal"))
  expect_true(server$is_shutting_down)
})

test_that("read_reload_config reports apps that share a rendered document", {
  path <- tempfile(fileext = ".json")
  app_dir <- tempfile("tss-shared")
  dir.create(app_dir)
  on.exit(unlink(c(path, app_dir), recursive = TRUE), add = TRUE)
  writeLines(c("---", "server: shiny", "---"), file.path(app_dir, "dash.qmd"))
  write_reload_config(path, list(list(name = "one", path = app_dir)))
  local_mocked_bindings(is_port_in_use = function(...) FALSE)
  config <- create_server_config(path)

  write_reload_config(path, list(list(name = "one", path = app_dir), list(name = "two", path = app_dir)))
  check <- config$read_reload_config()
  expect_false(check$valid)
  expect_match(check$error, "Apps one and two serve the same document, dash.qmd", fixed = TRUE)
})

test_that("an app that a reload cannot stop keeps the state of its process", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(
    list(name = "stuck", path = "/tmp", resident = TRUE),
    list(name = "other", path = "/tmp", resident = TRUE)
  ))
  test <- reload_test_server(path)
  config <- test$server$config
  config$add_app_process("stuck", list(is_alive = function() TRUE))
  document <- list(type = "rmd", file = "/tmp/report.Rmd")
  config$set_app_document("stuck", document)
  config$set_app_document("other", list(type = "shiny"))
  config$note_start_failure("other")

  write_reload_config(path, list(list(name = "new", path = "/tmp", resident = TRUE)))
  expect_false(test$server$reload()$valid)
  # Without its document, the app would serve its other documents
  expect_identical(config$get_app_document("stuck"), document)
  expect_true(is_other_document_path(config$get_app_document("stuck"), "/_draft.Rmd"))
  # The apps that stopped are forgotten as before
  expect_null(config$get_app_document("other"))
  expect_equal(config$start_backoff_seconds("other"), 0)
})

test_that("a reload that changes health_check_interval applies it at once", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  write_reload_config(path, list(list(name = "app", path = "/tmp")), health_check_interval = 86400)
  test <- reload_test_server(path)
  timers <- list()
  local_mocked_bindings(later = function(func, delay = 0, ...) {
    timer <- new.env()
    timer$delay <- delay
    timer$cancelled <- FALSE
    timers[[length(timers) + 1]] <<- timer
    function() timer$cancelled <- TRUE
  }, .package = "later")
  test$server$start_monitoring_services()
  first <- Filter(function(timer) timer$delay == 5, timers)
  expect_length(first, 1)

  write_reload_config(path, list(list(name = "app", path = "/tmp")), health_check_interval = 10)
  expect_true(test$server$reload()$valid)
  expect_true(first[[1]]$cancelled)
  expect_equal(timers[[length(timers)]]$delay, 10)

  # An unchanged interval leaves the pending check alone
  count <- length(timers)
  expect_true(test$server$reload()$valid)
  expect_length(timers, count)
})
