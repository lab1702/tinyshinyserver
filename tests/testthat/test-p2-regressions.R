test_that("duplicate names fail configuration validation before any app starts", {
  config <- ShinyServerConfig$new()
  data <- list(apps = list(list(name = "app", path = "/tmp/one"), list(name = "app", path = "/tmp/two")),
    log_dir = "/tmp", starting_port = 3001)
  result <- config$validate_config(data)
  expect_false(result$valid)
  expect_match(result$error, "Duplicate app name: app", fixed = TRUE)
  data$apps[[2]]$name <- "other"
  expect_true(config$validate_config(data)$valid)
})

test_that("old readiness callbacks cannot remove or mark replacements ready", {
  config <- ShinyServerConfig$new()
  old <- list(is_alive = function() FALSE)
  replacement <- list(is_alive = function() TRUE)
  config$add_app_process("app", replacement)
  config$set_app_starting("app")
  pm <- ProcessManager$new(config)
  expect_false(pm$check_app_ready("app", 3001, old))
  expect_identical(config$get_app_process("app"), replacement)
  expect_true(config$is_app_starting("app"))

  old <- list(is_alive = function() TRUE, generation = 1)
  config$add_app_process("app", old)
  resolve_probe <- NULL
  local_mocked_bindings(wait_for_backend = function(...) promises::promise(function(resolve, reject) {
    resolve_probe <<- resolve
  }))
  result <- pm$check_app_ready("app", 3001, old)
  config$add_app_process("app", replacement)
  config$set_app_starting("app")
  resolve_probe(TRUE)
  expect_false(await_response(result))
  expect_identical(config$get_app_process("app"), replacement)
  expect_true(config$is_app_starting("app"))
  config$remove_app_process("app")
  expect_false(pm$check_app_ready("app", 3001, replacement))
})

test_that("stale cleanup closes sockets and stops idle on-demand apps once", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  pm <- ProcessManager$new(config)
  killed <- 0
  config$add_app_process("app", list(is_alive = function() TRUE))
  local_mocked_bindings(kill_process_safely = function(process) { killed <<- killed + 1; TRUE })
  callbacks <- list()
  local_mocked_bindings(later = function(func, delay, ...) {
    callbacks[[length(callbacks) + 1L]] <<- func
  }, .package = "later")
  cm <- ConnectionManager$new(config, pm)
  client_closed <- 0
  backend_closed <- 0
  config$add_ws_connection("s", list(app_name = "app", last_activity = Sys.time() - 3600,
    ws = list(close = function() {
      client_closed <<- client_closed + 1
      cm$remove_client_connection("s")
    })))
  config$add_backend_connection("s", list(app_name = "app", last_activity = Sys.time(),
    ws = list(close = function() { backend_closed <<- backend_closed + 1 })))
  expect_equal(pm$cleanup_stale_connections(), 1)
  expect_equal(client_closed, 1)
  expect_equal(backend_closed, 1)
  # The idle app stops once the reconnect grace period passes
  expect_equal(killed, 0)
  expect_length(callbacks, 1)
  callbacks[[1]]()
  expect_equal(killed, 1)
  expect_null(config$get_ws_connection("s"))
  expect_null(config$get_backend_connection("s"))
  expect_equal(config$get_app_connection_count("app"), 0)
  cm$close_client_connection("s")
  expect_equal(killed, 1)
  expect_equal(client_closed, 1)
})

test_that("backend closure disconnects its browser and ignores stale callbacks", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  callbacks <- new.env()
  backend <- list(
    onMessage = function(f) NULL, onOpen = function(f) NULL, onError = function(f) NULL,
    onClose = function(f) { callbacks$close <- f }, close = function() callbacks$close(list())
  )
  local_mocked_bindings(WebSocket = list(new = function(url, headers = list()) backend), .package = "websocket")
  local_mocked_bindings(process_owns_port = function(process, port) TRUE)
  config$add_app_process("app", test_backend_process())
  cm <- ConnectionManager$new(config)
  closed <- 0
  client <- list(close = function() {closed <<- closed + 1; cm$remove_client_connection("s")})
  cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
  cm$create_backend_connection("app", "s", client)
  callbacks$close(list())
  callbacks$close(list())
  expect_equal(closed, 1)
  expect_null(config$get_ws_connection("s"))
  expect_null(config$get_backend_connection("s"))
  expect_equal(config$get_app_connection_count("app"), 0)

  cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
  replacement <- list(ws = list(generation = 2))
  config$add_backend_connection("s", replacement)
  callbacks$close(list())
  expect_identical(config$get_backend_connection("s"), replacement)
  expect_equal(config$get_app_connection_count("app"), 1)
  expect_equal(closed, 1)
})

test_that("partial startup failure stops servers, apps and future monitoring", {
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
    expect_equal(started_apps, 1)
    expect_equal(stopped_apps, 1)
    expect_equal(stopped_servers, 1)
    for (callback in scheduled) callback()
    expect_equal(health_checks, 0)
    expect_length(scheduled, 2)
  })
})

test_that("response headers preserve duplicates and remove transfer framing", {
  headers <- charToRaw(paste0("HTTP/1.1 200 OK\r\n",
    "Content-Disposition: attachment; filename=report.csv\r\n",
    "Cache-Control: private, no-store\r\nETag: abc\r\n",
    "Set-Cookie: a=1; Path=/; HttpOnly\r\nSet-Cookie: b=2; Path=/folder; Domain=127.0.0.1\r\n",
    "Connection: keep-alive, X-Internal\r\nX-Internal: hidden\r\n",
    "Transfer-Encoding: chunked\r\nContent-Length: 99\r\n\r\n"))
  result <- proxy_response_headers(headers, "http://127.0.0.1:3001/download", "app", "GET")
  expect_equal(result$`content-disposition`, "attachment; filename=report.csv")
  expect_equal(result$`cache-control`, "private, no-store")
  expect_equal(result$etag, "abc")
  expect_equal(unname(unlist(result[names(result) == "set-cookie"])),
    c("a=1; Path=/proxy/app/; HttpOnly", "b=2; Path=/proxy/app/folder"))
  expect_false(any(c("connection", "x-internal", "transfer-encoding", "content-length") %in% names(result)))
  expect_equal(proxy_response_headers(headers, "http://127.0.0.1:3001/", "app", "HEAD")$`content-length`, "99")
})

test_that("cookies with no path get an app-scoped default path", {
  headers <- charToRaw("HTTP/1.1 200 OK\r\nSet-Cookie: session=abc; HttpOnly\r\n\r\n")
  result <- proxy_response_headers(headers, "http://127.0.0.1:3001/", "app", "GET")
  expect_equal(result$`set-cookie`, "session=abc; HttpOnly; Path=/proxy/app/")
  nested <- proxy_response_headers(headers, "http://127.0.0.1:3001/folder/page?x=1", "app", "GET")
  expect_equal(nested$`set-cookie`, "session=abc; HttpOnly; Path=/proxy/app/folder")
})

test_that("stale backends also close fresh client sockets", {
  config <- ShinyServerConfig$new()
  closed <- 0
  config$add_ws_connection("s", list(app_name = "app", last_activity = Sys.time(),
    ws = list(close = function() {closed <<- closed + 1})))
  config$add_backend_connection("s", list(app_name = "app", last_activity = Sys.time() - 3600,
    ws = list(close = function() NULL)))
  expect_equal(ProcessManager$new(config)$cleanup_stale_connections(), 1)
  expect_equal(closed, 1)
  expect_null(config$get_ws_connection("s"))
  expect_null(config$get_backend_connection("s"))
})
