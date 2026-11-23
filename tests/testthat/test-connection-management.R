# Test for connection management functions
# Tests for WebSocket and backend connection management in ShinyServerConfig,
# and for the ConnectionManager's idle checks of on-demand apps

# ============================================================================
# WebSocket Connection Management Tests
# ============================================================================

test_that("add_ws_connection adds new connection and increments count", {
  config <- ShinyServerConfig$new()

  conn_info <- list(
    ws = NULL,
    app_name = "app1",
    client_ip = "127.0.0.1",
    user_agent = "test",
    last_activity = Sys.time(),
    created_at = Sys.time()
  )

  result <- config$add_ws_connection("session1", conn_info)

  expect_true(result)
  expect_equal(config$get_app_connection_count("app1"), 1)

  # Verify connection is stored
  stored <- config$get_ws_connection("session1")
  expect_equal(stored$app_name, "app1")
  expect_equal(stored$client_ip, "127.0.0.1")
})

test_that("add_ws_connection is idempotent for same session", {
  config <- ShinyServerConfig$new()

  conn_info <- list(
    ws = NULL,
    app_name = "app1",
    client_ip = "127.0.0.1",
    user_agent = "test",
    last_activity = Sys.time(),
    created_at = Sys.time()
  )

  # Add first time
  config$add_ws_connection("session1", conn_info)
  expect_equal(config$get_app_connection_count("app1"), 1)

  # Add again with same session_id (should update, not increment)
  config$add_ws_connection("session1", conn_info)
  expect_equal(config$get_app_connection_count("app1"), 1)
})

test_that("add_ws_connection handles NULL inputs", {
  config <- ShinyServerConfig$new()

  conn_info <- list(app_name = "app1")

  # NULL session_id
  result <- config$add_ws_connection(NULL, conn_info)
  expect_false(result)

  # NULL connection_info
  result <- config$add_ws_connection("session1", NULL)
  expect_false(result)
})

test_that("add_ws_connection handles multiple apps", {
  config <- ShinyServerConfig$new()

  conn1 <- list(app_name = "app1", ws = NULL)
  conn2 <- list(app_name = "app2", ws = NULL)

  config$add_ws_connection("s1", conn1)
  config$add_ws_connection("s2", conn2)
  config$add_ws_connection("s3", conn1)

  expect_equal(config$get_app_connection_count("app1"), 2)
  expect_equal(config$get_app_connection_count("app2"), 1)
})

test_that("remove_ws_connection removes connection and decrements count", {
  config <- ShinyServerConfig$new()

  conn_info <- list(app_name = "app1", ws = NULL)

  config$add_ws_connection("session1", conn_info)
  expect_equal(config$get_app_connection_count("app1"), 1)

  result <- config$remove_ws_connection("session1")

  expect_true(result)
  expect_equal(config$get_app_connection_count("app1"), 0)
  expect_null(config$get_ws_connection("session1"))
})

test_that("remove_ws_connection is idempotent", {
  config <- ShinyServerConfig$new()

  conn_info <- list(app_name = "app1", ws = NULL)

  config$add_ws_connection("session1", conn_info)

  # First remove should succeed
  result1 <- config$remove_ws_connection("session1")
  expect_true(result1)
  expect_equal(config$get_app_connection_count("app1"), 0)

  # Second remove should return FALSE (idempotent)
  result2 <- config$remove_ws_connection("session1")
  expect_false(result2)
  expect_equal(config$get_app_connection_count("app1"), 0)
})

test_that("remove_ws_connection handles NULL session_id", {
  config <- ShinyServerConfig$new()

  result <- config$remove_ws_connection(NULL)
  expect_false(result)
})

test_that("get_ws_connection returns NULL for non-existent session", {
  config <- ShinyServerConfig$new()

  result <- config$get_ws_connection("nonexistent")
  expect_null(result)
})

test_that("get_all_ws_connections returns all connections", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app2", ws = NULL))

  all_conns <- config$get_all_ws_connections()

  expect_equal(length(all_conns), 2)
  expect_true("s1" %in% names(all_conns))
  expect_true("s2" %in% names(all_conns))
})

# ============================================================================
# Backend Connection Management Tests
# ============================================================================

test_that("add_backend_connection stores backend connection", {
  config <- ShinyServerConfig$new()

  conn_info <- list(
    app_name = "app1",
    port = 3001,
    connected_at = Sys.time()
  )

  config$add_backend_connection("backend1", conn_info)

  stored <- config$get_backend_connection("backend1")
  expect_equal(stored$app_name, "app1")
  expect_equal(stored$port, 3001)
})

test_that("remove_backend_connection removes backend connection", {
  config <- ShinyServerConfig$new()

  conn_info <- list(app_name = "app1", port = 3001)

  config$add_backend_connection("backend1", conn_info)
  expect_false(is.null(config$get_backend_connection("backend1")))

  config$remove_backend_connection("backend1")
  expect_null(config$get_backend_connection("backend1"))
})

test_that("get_all_backend_connections returns all backend connections", {
  config <- ShinyServerConfig$new()

  config$add_backend_connection("b1", list(app_name = "app1"))
  config$add_backend_connection("b2", list(app_name = "app2"))

  all_backends <- config$get_all_backend_connections()

  expect_equal(length(all_backends), 2)
  expect_true("b1" %in% names(all_backends))
  expect_true("b2" %in% names(all_backends))
})

# ============================================================================
# App Process Management Tests
# ============================================================================

test_that("add_app_process stores process", {
  config <- ShinyServerConfig$new()

  mock_process <- list(
    pid = 12345,
    get_pid = function() 12345,
    is_alive = function() TRUE
  )

  config$add_app_process("app1", mock_process)

  stored <- config$get_app_process("app1")
  expect_equal(stored$pid, 12345)
})

test_that("remove_app_process removes process", {
  config <- ShinyServerConfig$new()

  mock_process <- list(pid = 12345)

  config$add_app_process("app1", mock_process)
  expect_false(is.null(config$get_app_process("app1")))

  config$remove_app_process("app1")
  expect_null(config$get_app_process("app1"))
})

test_that("get_all_app_processes returns all processes", {
  config <- ShinyServerConfig$new()

  config$add_app_process("app1", list(pid = 1))
  config$add_app_process("app2", list(pid = 2))

  all_procs <- config$get_all_app_processes()

  expect_equal(length(all_procs), 2)
  expect_true("app1" %in% names(all_procs))
  expect_true("app2" %in% names(all_procs))
})

# ============================================================================
# Connection Count Tests
# ============================================================================

test_that("get_app_connection_count returns 0 for new app", {
  config <- ShinyServerConfig$new()

  count <- config$get_app_connection_count("newapp")
  expect_equal(count, 0)
})

test_that("connection counts track multiple apps independently", {
  config <- ShinyServerConfig$new()

  # Add connections for app1
  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app1", ws = NULL))

  # Add connections for app2
  config$add_ws_connection("s3", list(app_name = "app2", ws = NULL))

  expect_equal(config$get_app_connection_count("app1"), 2)
  expect_equal(config$get_app_connection_count("app2"), 1)
  expect_equal(config$get_app_connection_count("app3"), 0)
})

test_that("a session re-added for another app is counted for that app only", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s1", list(app_name = "app2", ws = NULL))

  expect_equal(config$get_app_connection_count("app1"), 0)
  expect_equal(config$get_app_connection_count("app2"), 1)
  config$remove_ws_connection("s1")
  expect_equal(config$get_app_connection_count("app2"), 0)
})

# ============================================================================
# App Startup State Tests
# ============================================================================

test_that("Config tracks app startup state correctly", {
  config <- ShinyServerConfig$new()

  # Test initial state - app not starting
  expect_false(config$is_app_starting("test_app"))
  expect_null(config$get_app_startup_state("test_app"))

  # Mark app as starting
  config$set_app_starting("test_app")

  # Verify app is marked as starting
  expect_true(config$is_app_starting("test_app"))

  startup_state <- config$get_app_startup_state("test_app")
  expect_equal(startup_state$state, "starting")
  expect_true("started_at" %in% names(startup_state))
  expect_true("elapsed" %in% names(startup_state))

  # Mark app as ready
  config$set_app_ready("test_app")

  # Verify app is no longer starting
  expect_false(config$is_app_starting("test_app"))
  expect_null(config$get_app_startup_state("test_app"))
})

test_that("Config handles startup timeout correctly", {
  config <- ShinyServerConfig$new()

  # Manually create an expired startup state
  assign("timeout_app", list(
    state = "starting",
    started_at = Sys.time() - (config$APP_STARTUP_TIMEOUT_SECONDS + 5)
  ), envir = config$app_startup_state)

  # Get startup state should detect timeout
  startup_state <- config$get_app_startup_state("timeout_app")

  expect_equal(startup_state$state, "timeout")
  expect_true(startup_state$elapsed > config$APP_STARTUP_TIMEOUT_SECONDS)

  # Reading the state does not use it up: forward_request() reports the
  # timeout and ends it, whichever caller asks first
  expect_false(config$is_app_starting("timeout_app"))
  expect_equal(config$get_app_startup_state("timeout_app")$state, "timeout")

  # The readiness checks mark a startup they gave up on in the same way
  config$set_app_starting("slow_app")
  config$set_app_start_timed_out("slow_app")
  expect_false(config$is_app_starting("slow_app"))
  expect_equal(config$get_app_startup_state("slow_app")$state, "timeout")
  # Nothing is created for an app that is not starting
  config$set_app_start_timed_out("ready_app")
  expect_null(config$get_app_startup_state("ready_app"))
})

test_that("a session's close is logged once, and not for an untracked session", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  cm <- ConnectionManager$new(config)
  cm$add_client_connection("s1", list(close = function() NULL), "app", "127.0.0.1", "test")
  logged <- capture_test_log({
    cm$close_client_connection("s1")
    # The socket's own close handler asks again
    cm$remove_client_connection("s1")
    cm$remove_client_connection("never-opened")
  })
  expect_equal(sum(grepl("WebSocket connection closed for session", logged)), 1)
})

# ============================================================================
# Idle Check Tests
# ============================================================================

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
  # Closing the last session waits for a reconnect before stopping
  cm$remove_client_connection("s")
  expect_equal(stops, 1)
  expect_equal(tail(delays, 1), 30)
  tail(callbacks, 1)[[1]]()
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

test_that("a superseded idle check withdraws its timer", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  config$add_app_process("app", list(generation = 1))
  stops <- 0
  cm <- ConnectionManager$new(config, list(stop_app_immediately = function(...) stops <<- stops + 1))
  timers <- list()
  cancelled <- 0
  local_mocked_bindings(later = function(func, delay, ...) {
    timers[[length(timers) + 1L]] <<- func
    function() cancelled <<- cancelled + 1
  }, .package = "later")

  # Each finished request asks for a check; only the latest timer stays queued
  for (i in 1:5) {
    cm$begin_http_request("app")
    cm$end_http_request("app")
  }
  expect_length(timers, 5)
  expect_equal(cancelled, 4)

  # A timer that fires anyway after being superseded does nothing
  timers[[1]]()
  expect_equal(stops, 0)
  timers[[5]]()
  expect_equal(stops, 1)

  # A new session withdraws the pending check as well
  cm$end_http_request("app")
  cm$add_client_connection("s", list(), "app", "127.0.0.1", "test")
  expect_equal(cancelled, 5)
  expect_null(config$pending_session_checks[["app"]])
})

test_that("idle checks cannot stop busy, reconnected or replacement apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  old <- list(generation = 1)
  replacement <- list(generation = 2)
  config$add_app_process("app", old)
  stops <- 0
  checks <- list()
  local_mocked_bindings(later = function(func, delay, ...) {
    checks[[length(checks) + 1L]] <<- func
  }, .package = "later")
  cm <- ConnectionManager$new(config, list(stop_app_immediately = function(app_name) stops <<- stops + 1))

  # A request in flight keeps the app alive
  cm$begin_http_request("app")
  cm$maybe_stop_idle_app("app")
  expect_equal(stops, 0)

  # So does a browser session that opened before the request finished
  cm$add_client_connection("s", list(), "app", "127.0.0.1", "test")
  cm$end_http_request("app")
  expect_length(checks, 0)

  # A check scheduled for the old process cannot stop its replacement
  cm$begin_http_request("app")
  cm$remove_client_connection("s")
  config$add_app_process("app", replacement)
  cm$end_http_request("app")
  expect_length(checks, 2)
  checks[[1]]()
  expect_equal(stops, 0)

  # The replacement's own check reclaims it once it is idle
  checks[[2]]()
  expect_equal(stops, 1)
})

test_that("a new page can connect after the last old session closes during HTTP", {
  for (reconnect in c(FALSE, TRUE)) {
    config <- ShinyServerConfig$new()
    config$config <- list(apps = list(list(name = "app", resident = FALSE)))
    config$add_app_process("app", list(generation = 1))
    stops <- 0
    cm <- ConnectionManager$new(config, list(stop_app_immediately = function(...) stops <<- stops + 1))
    callback <- NULL
    delays <- numeric()
    local_mocked_bindings(later = function(func, delay, ...) {
      callback <<- func
      delays <<- c(delays, delay)
    }, .package = "later")
    cm$add_client_connection("old", list(), "app", "127.0.0.1", "test")
    cm$begin_http_request("app")
    cm$remove_client_connection("old")
    expect_equal(stops, 0)
    cm$end_http_request("app")
    expect_equal(stops, 0)
    expect_equal(unique(delays), 30)
    if (reconnect) cm$add_client_connection("new", list(), "app", "127.0.0.1", "test")
    callback()
    expect_equal(stops, if (reconnect) 0 else 1)
  }
})

test_that("reloading an on-demand page does not stop its app between pages", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  config$add_app_process("app", list(generation = 1))
  stops <- 0
  cm <- ConnectionManager$new(config, list(stop_app_immediately = function(...) stops <<- stops + 1))
  callbacks <- list()
  local_mocked_bindings(later = function(func, delay, ...) {
    callbacks[[length(callbacks) + 1L]] <<- func
  }, .package = "later")
  cm$add_client_connection("old", list(), "app", "127.0.0.1", "test")
  # The new page's document completes before the old page unloads
  cm$begin_http_request("app")
  cm$end_http_request("app")
  cm$remove_client_connection("old")
  expect_equal(stops, 0)
  cm$add_client_connection("new", list(), "app", "127.0.0.1", "test")
  for (callback in callbacks) callback()
  expect_equal(stops, 0)
  # Once the last page really leaves, the app stops after the grace period
  cm$remove_client_connection("new")
  expect_equal(stops, 0)
  tail(callbacks, 1)[[1]]()
  expect_equal(stops, 1)
})

test_that("an unused on-demand app that is still starting is left to finish", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = FALSE)))
  starting <- list(is_alive = function() TRUE, get_start_time = function() Sys.time() - 150)
  config$add_app_process("app", starting)
  stops <- 0
  checks <- list()
  delays <- numeric()
  local_mocked_bindings(later = function(func, delay, ...) {
    checks[[length(checks) + 1L]] <<- func
    delays <<- c(delays, delay)
  }, .package = "later")
  listening <- FALSE
  local_mocked_bindings(process_owns_port = function(process, port) listening)
  cm <- ConnectionManager$new(config, list(stop_app_immediately = function(app_name) stops <<- stops + 1))

  # The visitor's page has given up, but the app has yet to listen on its
  # port: a slow start is not thrown away, and the app is checked again
  cm$schedule_session_check("app", 45)
  checks[[1]]()
  expect_equal(stops, 0)
  expect_equal(delays, c(45, config$HTTP_SESSION_GRACE_SECONDS))

  # Once it is ready and still unused, it is stopped
  listening <- TRUE
  checks[[2]]()
  expect_equal(stops, 1)

  # An app that never listens is not kept for ever
  listening <- FALSE
  stuck <- list(is_alive = function() TRUE,
    get_start_time = function() Sys.time() - config$ON_DEMAND_START_LIMIT_SECONDS - 1)
  config$add_app_process("app", stuck)
  cm$schedule_session_check("app")
  tail(checks, 1)[[1]]()
  expect_equal(stops, 2)

  # Nor is one whose process has exited, or whose start time cannot be read
  for (process in list(
    list(is_alive = function() FALSE, get_start_time = function() Sys.time()),
    list(is_alive = function() TRUE)
  )) {
    config$add_app_process("app", process)
    expect_false(cm$is_still_starting("app", process))
  }
})

test_that("a queued message that cannot be sent when the app's socket opens ends the session", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 1, resident = TRUE)))
  assign("app_backend_verified", function(app_name) TRUE, envir = config)
  sent <- character()
  handlers <- list()
  fake_socket <- list(
    onMessage = function(f) handlers$message <<- f, onOpen = function(f) handlers$open <<- f,
    onClose = function(f) handlers$close <<- f, onError = function(f) handlers$error <<- f,
    send = function(message) {
      if (identical(message, "refused")) stop("A payload contained invalid data")
      sent <<- c(sent, message)
    },
    close = function() NULL
  )
  local_mocked_bindings(WebSocket = list(new = function(...) fake_socket), .package = "websocket")
  cm <- ConnectionManager$new(config)
  closed <- 0
  client <- list(request = list(PATH_INFO = "/proxy/app/websocket/"), close = function() closed <<- closed + 1)
  cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
  cm$create_backend_connection("app", "s", client)
  for (message in c("first", "refused", "last")) {
    expect_true(cm$handle_client_message("s", message, "app"))
  }

  capture_test_log(handlers$open(list()))
  # As once the socket is open, the session ends instead of going on
  # without the rest of its messages
  expect_equal(sent, "first")
  expect_equal(closed, 1)
  expect_null(config$get_ws_connection("s"))
  expect_null(config$get_backend_connection("s"))
})
