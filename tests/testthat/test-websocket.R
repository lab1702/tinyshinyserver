# Tests for WebSocket handshakes and the sessions proxied to apps
# (handle_websocket_connection() and the ConnectionManager's app connections)

# ============================================================================
# Handshake checks
# ============================================================================

test_that("cross-origin WebSocket handshakes are rejected before routing", {
  config <- ShinyServerConfig$new()
  # Bound to all interfaces, so only the Origin check applies to these hosts
  config$config <- list(apps = list(list(name = "app", port = 1, resident = TRUE)), proxy_host = "0.0.0.0")
  cm <- ConnectionManager$new(config)
  closed <- 0
  for (origin in c("https://evil.example", "http://myapp.example.com.evil.example", "null",
                   "http://myapp.example.com:8080", "file://")) {
    ws <- list(request = list(PATH_INFO = "/proxy/app/websocket/", HTTP_HOST = "myapp.example.com",
      HTTP_ORIGIN = origin, REMOTE_ADDR = "203.0.113.5", HTTP_X_FORWARDED_HOST = "evil.example"),
      close = function() closed <<- closed + 1,
      onMessage = function(...) stop("Cross-origin socket must not register callbacks"))
    handle_websocket_connection(ws, config, cm)
  }
  expect_equal(closed, 5)
  expect_length(config$get_all_ws_connections(), 0)
})

test_that("same-origin WebSocket handshakes are recognised", {
  same <- function(...) is_same_origin_websocket(list(...))
  expect_true(same(HTTP_HOST = "localhost:3838"))
  expect_true(same(HTTP_HOST = "localhost:3838", HTTP_ORIGIN = "http://localhost:3838"))
  expect_true(same(HTTP_HOST = "[::1]:3838", HTTP_ORIGIN = "http://[::1]:3838"))
  expect_true(same(HTTP_HOST = "MyApp.example.com", HTTP_ORIGIN = "https://myapp.example.com"))
  expect_true(same(HTTP_HOST = "myapp.example.com:443", HTTP_ORIGIN = "https://myapp.example.com"))
  # A local reverse proxy that rewrites Host reports the original host
  expect_true(same(HTTP_HOST = "127.0.0.1:3838", HTTP_ORIGIN = "https://myapp.example.com",
    REMOTE_ADDR = "127.0.0.1", HTTP_X_FORWARDED_HOST = "myapp.example.com"))
  expect_false(same(HTTP_HOST = "127.0.0.1:3838", HTTP_ORIGIN = "https://myapp.example.com",
    REMOTE_ADDR = "203.0.113.5", HTTP_X_FORWARDED_HOST = "myapp.example.com"))
  expect_false(same(HTTP_HOST = "localhost:3838", HTTP_ORIGIN = "http://localhost:3839"))
})

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
  expect_length(ls(config$app_sessions), 0)
  expect_length(ls(config$pending_session_checks), 0)
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

# ============================================================================
# Connections to the app
# ============================================================================

test_that("backend WebSocket handshakes carry the request's end-to-end headers", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  sent <- NULL
  backend <- list(onMessage = function(f) NULL, onOpen = function(f) NULL,
    onError = function(f) NULL, onClose = function(f) NULL, close = function() NULL)
  local_mocked_bindings(WebSocket = list(new = function(url, headers, ...) {
    sent <<- headers
    backend
  }), .package = "websocket")
  local_mocked_bindings(process_owns_port = function(process, port) TRUE)
  config$add_app_process("app", test_backend_process())
  cm <- ConnectionManager$new(config)
  request <- list(PATH_INFO = "/proxy/app/websocket/", REMOTE_ADDR = "127.0.0.1",
    HTTP_HOST = "myapp.example.com", HTTP_CONNECTION = "Upgrade", HTTP_UPGRADE = "websocket",
    HTTP_ORIGIN = "https://myapp.example.com", HTTP_SEC_WEBSOCKET_KEY = "client-key",
    HTTP_SEC_WEBSOCKET_VERSION = "13", HTTP_SEC_WEBSOCKET_EXTENSIONS = "permessage-deflate",
    HTTP_COOKIE = "session=alice", HTTP_AUTHORIZATION = "Basic YWxpY2U6cHc=",
    HTTP_X_FORWARDED_USER = "alice", HTTP_USER_AGENT = "Browser/1.0", HTTP_ACCEPT_LANGUAGE = "sv")
  client <- list(request = request, close = function() NULL)
  cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
  cm$create_backend_connection("app", "s", client)
  expect_setequal(names(sent), c("COOKIE", "AUTHORIZATION", "X-FORWARDED-USER", "USER-AGENT", "ACCEPT-LANGUAGE"))
  expect_equal(sent[["X-FORWARDED-USER"]], "alice")

  # A handshake with nothing to forward still connects
  bare <- list(request = list(PATH_INFO = "/proxy/app/websocket/", HTTP_HOST = "127.0.0.1:3838",
    HTTP_SEC_WEBSOCKET_KEY = "client-key"), close = function() NULL)
  cm$add_client_connection("t", bare, "app", "127.0.0.1", "test")
  cm$create_backend_connection("app", "t", bare)
  expect_length(sent, 0)
})

test_that("backend traffic keeps both connections alive and ignores old sockets", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  message_callback <- NULL
  backend <- list(onMessage = function(f) message_callback <<- f,
    onOpen = function(f) NULL, onClose = function(f) NULL, onError = function(f) NULL, close = function() NULL)
  local_mocked_bindings(WebSocket = list(new = function(...) backend), .package = "websocket")
  local_mocked_bindings(process_owns_port = function(process, port) TRUE)
  config$add_app_process("app", test_backend_process())
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

test_that("backend errors close sessions once and ignore replaced connections", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  callbacks <- new.env()
  backend <- list(onMessage = function(f) NULL, onOpen = function(f) NULL,
    onError = function(f) callbacks$error <- f, onClose = function(f) callbacks$close <- f,
    close = function() callbacks$close(list()))
  local_mocked_bindings(WebSocket = list(new = function(...) backend), .package = "websocket")
  local_mocked_bindings(process_owns_port = function(process, port) TRUE)
  config$add_app_process("app", test_backend_process())
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

test_that("backend closure disconnects its browser and ignores stale callbacks", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  callbacks <- new.env()
  backend <- list(
    onMessage = function(f) NULL, onOpen = function(f) NULL, onError = function(f) NULL,
    onClose = function(f) { callbacks$close <- f }, close = function() callbacks$close(list())
  )
  local_mocked_bindings(WebSocket = list(new = function(url, headers = list(), ...) backend), .package = "websocket")
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

# ============================================================================
# Browser messages
# ============================================================================

test_that("messages for untracked sessions do not open backend connections", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 1, resident = TRUE)))
  cm <- ConnectionManager$new(config)
  expect_false(cm$handle_client_message("gone", "{}", "app"))
  expect_null(config$get_backend_connection("gone"))
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
