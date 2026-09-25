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
  expect_equal(config$config_file, normalizePath(path))

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
  ), title = "Reloaded", starting_port = 4001)
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
  expect_true(server$is_shutting_down)
})
