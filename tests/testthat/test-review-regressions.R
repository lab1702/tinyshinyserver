test_that("malformed proxy prefixes never start apps or form backend URLs", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  pm <- list(start_app_on_demand = function(...) stop("must not start"))
  local_mocked_bindings(forward_request = function(...) stop("must not forward"))
  for (path in c("/proxy//app/path", "/proxy///app/path", "//proxy/app/path")) {
    expect_equal(handle_proxy_request(path, "GET", "", list(), config, pm)$status, 400)
  }
})

test_that("port-range capacity excludes only reserved ports in that range", {
  config <- ShinyServerConfig$new()
  conf <- list(apps = list(list(name = "app", path = tempdir())), log_dir = tempdir(),
    starting_port = 65535, proxy_port = 3838, management_port = 3839)
  expect_true(config$validate_config(conf)$valid)
  config$config <- conf
  local_mocked_bindings(is_port_in_use = function(...) FALSE)
  config$assign_app_ports()
  expect_equal(config$config$apps[[1]]$port, 65535)
  conf$management_port <- 65535
  expect_false(config$validate_config(conf)$valid)
  conf$starting_port <- 65534
  conf$proxy_port <- 65535
  expect_true(config$validate_config(conf)$valid)
})

test_that("WebSocket-only startup is reclaimed if the client never retries", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE,
    appstart_timeout = 60)))
  alive <- TRUE
  process <- list(is_alive = function() alive, kill_tree = function() alive <<- FALSE)
  pm <- ProcessManager$new(config)
  assign("start_app", function(app_config) {
    config$add_app_process("app", process)
    config$set_app_starting("app")
    TRUE
  }, envir = pm)
  cm <- ConnectionManager$new(config, pm)
  callback <- NULL
  delays <- numeric()
  local_mocked_bindings(later = function(func, delay, ...) {
    callback <<- func
    delays <<- c(delays, delay)
  }, .package = "later")
  closed <- FALSE
  ws <- list(request = list(PATH_INFO = "/proxy/app/websocket/"),
    send = function(...) NULL, close = function() closed <<- TRUE)
  handle_websocket_connection(ws, config, cm, pm)
  expect_true(closed)
  expect_true(alive)
  expect_equal(delays, 90)
  expect_equal(config$get_app_connection_count("app"), 0)
  config$set_app_ready("app")
  callback()
  expect_false(alive)
  expect_null(config$get_app_process("app"))
})

test_that("restarted on-demand apps get an idle grace that reconnects cancel", {
  for (reconnect in c(FALSE, TRUE)) {
    config <- ShinyServerConfig$new()
    config$config <- list(apps = list(list(name = "app", resident = FALSE,
      appstart_timeout = 60)), restart_delay = 0)
    pm <- ProcessManager$new(config)
    alive <- TRUE
    process <- list(is_alive = function() alive, kill_tree = function() alive <<- FALSE)
    assign("start_app", function(app_config) {
      config$add_app_process("app", process)
      TRUE
    }, envir = pm)
    callback <- NULL
    delay <- NULL
    local_mocked_bindings(later = function(func, delay, ...) {
      callback <<- func
      # Store outside the formal argument's scope.
      delays <<- c(delays, delay)
    }, .package = "later")
    delays <- numeric()
    expect_true(pm$restart_app("app")$success)
    expect_equal(delays, 90)
    expect_true(alive)
    if (reconnect) ConnectionManager$new(config, pm)$add_client_connection(
      "s", list(), "app", "127.0.0.1", "test")
    callback()
    expect_identical(alive, reconnect)
  }
})

test_that("unknown WebSocket apps are rejected without retaining state", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)))
  cm <- ConnectionManager$new(config)
  closed <- 0
  for (name in paste0("unknown-", 1:10)) {
    ws <- list(request = list(PATH_INFO = paste0("/proxy/", name, "/websocket/")),
      close = function() closed <<- closed + 1,
      onMessage = function(...) stop("Unknown app must not register callbacks"))
    handle_websocket_connection(ws, config, cm)
  }
  expect_equal(closed, 10)
  expect_length(config$get_all_ws_connections(), 0)
  expect_length(ls(config$app_connection_counts), 0)
  expect_length(ls(config$deferred_idle_stops), 0)
  expect_length(ls(config$pending_session_checks), 0)
})

test_that("HTTP-only launches stop after a cancellable session grace period", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = FALSE)))
  process <- list(generation = 1)
  stops <- 0
  pm <- list(start_app_on_demand = function(...) {
    config$add_app_process("app", process)
    TRUE
  }, stop_app_immediately = function(...) { stops <<- stops + 1; TRUE })
  cm <- ConnectionManager$new(config, pm)
  callbacks <- list()
  delays <- numeric()
  local_mocked_bindings(later = function(func, delay, ...) {
    callbacks[[length(callbacks) + 1L]] <<- func
    delays <<- c(delays, delay)
  }, .package = "later")
  local_mocked_bindings(forward_request = function(...) create_html_response("page"))
  expect_equal(handle_proxy_request("/proxy/app/", "GET", "", list(), config, pm, cm)$status, 200)
  expect_equal(stops, 0)
  expect_equal(delays, 30)
  callbacks[[1]]()
  expect_equal(stops, 1)

  cm$begin_http_request("app")
  cm$end_http_request("app")
  stale <- tail(callbacks, 1)[[1]]
  cm$begin_http_request("app")
  stale()
  expect_equal(stops, 1)
  cm$end_http_request("app")
  stale <- tail(callbacks, 1)[[1]]
  cm$add_client_connection("s", list(), "app", "127.0.0.1", "test")
  stale()
  expect_equal(stops, 1)
  cm$remove_client_connection("s")
  expect_equal(stops, 2)

  cm$begin_http_request("app")
  cm$end_http_request("app")
  stale <- tail(callbacks, 1)[[1]]
  config$add_app_process("app", list(generation = 2))
  stale()
  expect_equal(stops, 2)
  config$config$apps[[1]]$resident <- TRUE
  n <- length(callbacks)
  cm$begin_http_request("app")
  cm$end_http_request("app")
  expect_length(callbacks, n)
})

test_that("removing a starting process allows the next on-demand launch", {
  for (removal in c("health", "cleanup", "stop")) {
    config <- ShinyServerConfig$new()
    config$config <- list(apps = list(list(name = "app", port = 3001,
      resident = FALSE, appstart_timeout = 60)))
    old <- list(is_alive = function() FALSE)
    config$add_app_process("app", old)
    config$set_app_starting("app")
    pm <- ProcessManager$new(config)
    starts <- 0
    replacement <- list(is_alive = function() TRUE)
    assign("start_app", function(app_config) {
      starts <<- starts + 1
      config$add_app_process("app", replacement)
      config$set_app_starting("app")
      TRUE
    }, envir = pm)

    switch(removal, health = pm$health_check(), cleanup = pm$cleanup_dead_processes(),
      stop = pm$stop_app("app"))
    expect_null(config$get_app_process("app"), info = removal)
    expect_false(config$is_app_starting("app"), info = removal)
    expect_false(pm$check_app_ready("app", 3001, old))
    expect_true(pm$start_app_on_demand("app"))
    expect_equal(starts, 1, info = removal)
    expect_identical(config$get_app_process("app"), replacement)
    # A delayed callback from the old process must not clear the new launch.
    expect_false(pm$check_app_ready("app", 3001, old))
    expect_true(config$is_app_starting("app"), info = removal)
  }
})

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
  config$set_app_starting("app")
  local_mocked_bindings(kill_process_safely = function(process) FALSE)
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  expect_false(pm$stop_app("app")$success)
  expect_identical(config$get_app_process("app"), process)
  expect_true(config$is_app_starting("app"))
  expect_false(pm$restart_app("app")$success)
  expect_identical(config$get_app_process("app"), process)
  expect_true(config$is_app_starting("app"))
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
