test_that("backend traffic keeps both connections alive and ignores old sockets", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  message_callback <- NULL
  backend <- list(onMessage = function(f) message_callback <<- f,
    onOpen = function(f) NULL, onClose = function(f) NULL, onError = function(f) NULL, close = function() NULL)
  local_mocked_bindings(WebSocket = list(new = function(...) backend), .package = "websocket")
  closed <- FALSE
  messages <- character()
  client <- list(send = function(data) messages <<- c(messages, data), close = function() closed <<- TRUE)
  cm <- ConnectionManager$new(config)
  cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
  cm$create_backend_connection("app", "s", client)
  for (side in c("client", "backend")) {
    conn <- if (side == "client") config$get_ws_connection("s") else config$get_backend_connection("s")
    conn$last_activity <- Sys.time() - 3600
    if (side == "client") config$add_ws_connection("s", conn) else config$add_backend_connection("s", conn)
  }
  message_callback(list(data = "fresh update"))
  expect_equal(ProcessManager$new(config)$cleanup_stale_connections(), 0)
  expect_false(closed)
  expect_equal(messages, "fresh update")
  config$add_backend_connection("s", list(ws = list(generation = 2)))
  message_callback(list(data = "stale update"))
  expect_equal(messages, "fresh update")
})

test_that("failed termination retains the process and prevents replacement", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = 0)
  process <- list(is_alive = function() TRUE)
  config$add_app_process("app", process)
  local_mocked_bindings(kill_process_safely = function(process) FALSE)
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  expect_false(pm$stop_app("app")$success)
  expect_identical(config$get_app_process("app"), process)
  expect_false(pm$restart_app("app")$success)
  expect_identical(config$get_app_process("app"), process)
  expect_equal(starts, 0)
})

test_that("termination reports a process that survives kill attempts", {
  kills <- 0
  process <- list(is_alive = function() TRUE,
    kill = function() { kills <<- kills + 1 }, kill_tree = function() { kills <<- kills + 1 })
  expect_false(kill_process_safely(process))
  expect_equal(kills, 2)
})

test_that("restart delays yield and are cancelled by stop and shutdown", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = 0.05)
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  tick <- FALSE
  later::later(function() tick <<- TRUE, 0)
  result <- pm$restart_app("app")
  expect_true(result$success)
  expect_match(result$message, "scheduled")
  expect_equal(starts, 0)
  later::run_now(0)
  expect_true(tick)
  pm$health_check()
  expect_equal(starts, 0)
  await_response(promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), .1)))
  expect_equal(starts, 1)

  pm$restart_app("app")
  pm$stop_app("app")
  await_response(promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), .1)))
  expect_equal(starts, 1)
  pm$restart_app("app")
  pm$stop_all_apps()
  await_response(promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), .1)))
  expect_equal(starts, 1)
})

test_that("health checks delay crashed resident app restarts without duplicates", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = .05)
  config$add_app_process("app", list(is_alive = function() FALSE))
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  pm$health_check()
  pm$health_check()
  expect_equal(starts, 0)
  await_response(promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), .1)))
  expect_equal(starts, 1)
})

test_that("termination stops a real child worker as well as its parent", {
  marker <- tempfile()
  child_pid_file <- tempfile()
  parent <- callr::r_bg(function(marker, child_pid_file) {
    child <- callr::r_bg(function(marker) {
      repeat {
        cat("tick\n", file = marker, append = TRUE)
        Sys.sleep(.02)
      }
    }, args = list(marker = marker), supervise = FALSE)
    writeLines(as.character(child$get_pid()), child_pid_file)
    Sys.sleep(30)
  }, args = list(marker = marker, child_pid_file = child_pid_file))
  on.exit({
    parent$kill_tree()
    if (file.exists(child_pid_file)) try(tools::pskill(as.integer(readLines(child_pid_file))), silent = TRUE)
    unlink(c(marker, child_pid_file))
  }, add = TRUE)
  deadline <- Sys.time() + 5
  while ((!file.exists(marker) || !file.exists(child_pid_file)) && Sys.time() < deadline) Sys.sleep(.02)
  expect_true(file.exists(marker))
  expect_true(kill_process_safely(parent))
  expect_false(parent$is_alive())
  Sys.sleep(.1)
  size <- file.info(marker)$size
  Sys.sleep(.15)
  expect_equal(file.info(marker)$size, size)
})

test_that("backend errors close sessions once and ignore replaced connections", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  callbacks <- new.env()
  backend <- list(onMessage = function(f) NULL, onOpen = function(f) NULL,
    onError = function(f) callbacks$error <- f, onClose = function(f) callbacks$close <- f,
    close = function() callbacks$close(list()))
  local_mocked_bindings(WebSocket = list(new = function(...) backend), .package = "websocket")
  cm <- ConnectionManager$new(config)
  closed <- 0
  client <- list(close = function() { closed <<- closed + 1; cm$remove_client_connection("s") })
  cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
  cm$create_backend_connection("app", "s", client)
  callbacks$error(list(message = "Connection refused"))
  callbacks$error(list(message = "Connection refused"))
  expect_equal(closed, 1)
  expect_null(config$get_ws_connection("s"))
  expect_null(config$get_backend_connection("s"))
  expect_equal(config$get_app_connection_count("app"), 0)
  cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
  replacement <- list(ws = list(generation = 2))
  config$add_backend_connection("s", replacement)
  callbacks$error(list(message = "Old error"))
  expect_equal(closed, 1)
  expect_identical(config$get_backend_connection("s"), replacement)
})

test_that("health checks isolate startup errors and retry failed apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = lapply(c("bad", "next", "healthy"), function(name) list(name = name, resident = TRUE)))
  healthy <- list(is_alive = function() TRUE)
  config$add_app_process("healthy", healthy)
  pm <- ProcessManager$new(config)
  attempted <- character()
  fail <- TRUE
  assign("start_app", function(app_config) {
    attempted <<- c(attempted, app_config$name)
    if (app_config$name == "bad" && fail) stop("Resource temporarily unavailable")
    config$add_app_process(app_config$name, list(is_alive = function() TRUE))
    TRUE
  }, envir = pm)
  expect_no_error(pm$health_check())
  expect_equal(attempted, c("bad", "next"))
  expect_identical(config$get_app_process("healthy"), healthy)
  fail <- FALSE
  pm$health_check()
  expect_equal(attempted, c("bad", "next", "bad"))
  expect_true(config$get_app_process("bad")$is_alive())
})

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

test_that("backend-local redirects preserve paths queries and fragments", {
  rewrite <- function(location) {
    raw <- charToRaw(paste0("HTTP/1.1 302 Found\r\nLocation: ", location, "\r\n\r\n"))
    proxy_response_headers(raw, "http://127.0.0.1:3001/", "app", "GET")$location
  }
  for (authority in c("http://127.0.0.1:3001", "//127.0.0.1:3001")) {
    for (suffix in c("", "?foo=1", "#section", "/next?x=a%2Fb#section", "//nested/path")) {
      path <- if (startsWith(suffix, "/")) suffix else paste0("/", suffix)
      expect_equal(rewrite(paste0(authority, suffix)), paste0("/proxy/app", path))
    }
  }
  for (location in c("//example.com/next", "https://example.com/?foo=1", "http://127.0.0.1:3002/next",
    "http://127.0.0.1:3001.example.com/next", "../next?x=1", "?foo=1", "#section")) {
    expect_equal(rewrite(location), location)
  }
  expect_equal(rewrite("/next?foo=1#section"), "/proxy/app/next?foo=1#section")
})

test_that("HTTP accounting releases rejected and synchronous responses", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  cm <- ConnectionManager$new(config)
  failure <- FALSE
  local_mocked_bindings(forward_request = function(...) {
    if (failure) return(promises::promise(function(resolve, reject) reject(simpleError("failure"))))
    create_503_response("starting")
  })
  expect_equal(handle_proxy_request("/proxy/app/", "GET", "", list(), config, connection_manager = cm)$status, 503)
  expect_equal(config$active_http_requests$app, 0)
  failure <- TRUE
  expect_error(await_response(handle_proxy_request("/proxy/app/", "GET", "", list(), config, connection_manager = cm)), "failure")
  expect_equal(config$active_http_requests$app, 0)
})

test_that("deferred shutdown cannot stop reconnected or replacement apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  old <- list(generation = 1)
  replacement <- list(generation = 2)
  config$add_app_process("app", old)
  stops <- 0
  cm <- ConnectionManager$new(config, list(stop_app_immediately = function(app_name) stops <<- stops + 1))
  cm$begin_http_request("app")
  cm$maybe_stop_idle_app("app")
  cm$add_client_connection("s", list(), "app", "127.0.0.1", "test")
  cm$end_http_request("app")
  expect_equal(stops, 0)
  cm$begin_http_request("app")
  cm$remove_client_connection("s")
  config$add_app_process("app", replacement)
  cm$end_http_request("app")
  expect_equal(stops, 0)
  expect_null(config$deferred_idle_stops$app)
})

test_that("root redirect does not launch a dormant app", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  manager <- list(start_app_on_demand = function(...) stop("must not start"))
  response <- handle_proxy_request("/proxy/app", "GET", "x=1", list(), config, manager)
  expect_equal(response$status, 308)
  expect_equal(response$headers$Location, "/proxy/app/?x=1")
})
