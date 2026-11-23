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
  # A page on the plain-HTTP site may not open the HTTPS site's sessions
  expect_false(same(HTTP_HOST = "myapp.example.com", HTTP_ORIGIN = "http://myapp.example.com",
    HTTP_X_FORWARDED_PROTO = "https"))
  expect_false(same(HTTP_HOST = "myapp.example.com", HTTP_ORIGIN = "http://myapp.example.com:80",
    HTTP_X_FORWARDED_PROTO = "HTTPS, http"))
  expect_true(same(HTTP_HOST = "myapp.example.com", HTTP_ORIGIN = "https://myapp.example.com",
    HTTP_X_FORWARDED_PROTO = "https"))
  # Behind a second proxy that ends TLS, the inner one reports "http"
  expect_true(same(HTTP_HOST = "myapp.example.com", HTTP_ORIGIN = "https://myapp.example.com",
    HTTP_X_FORWARDED_PROTO = "http"))
  expect_true(same(HTTP_HOST = "myapp.example.com", HTTP_ORIGIN = "http://myapp.example.com"))
})

test_that("rejected handshakes log their Host and Origin without control characters", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 1, resident = TRUE)))
  cm <- ConnectionManager$new(config)
  hostile <- paste0("evil\033[2K\033[1Afake line\r", strrep("x", 500))
  logged <- capture_test_log({
    handle_websocket_connection(list(request = list(PATH_INFO = "/proxy/app/websocket/", HTTP_HOST = hostile),
      close = function() NULL), config, cm)
    handle_websocket_connection(list(request = list(PATH_INFO = "/proxy/app/websocket/", HTTP_HOST = "localhost",
      HTTP_ORIGIN = hostile), close = function() NULL), config, cm)
  })
  rejected <- grep("Rejecting", logged, value = TRUE)
  expect_length(rejected, 2)
  expect_false(any(grepl("[\001-\037]", rejected, useBytes = TRUE)))
  expect_true(all(grepl("evil?[2K?[1Afake line?xxx", rejected, fixed = TRUE)))
  expect_true(all(nchar(rejected, type = "bytes") < 400))
  expect_equal(log_value(NULL), "(none)")
})

test_that("an R Markdown app's socket may not open another document of its directory", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "reports", port = 1, resident = TRUE)))
  config$set_app_document("reports", list(type = "rmd", file = file.path(tempdir(), "report.Rmd")))
  cm <- ConnectionManager$new(config)
  closed <- 0
  for (path in c("/_partial.Rmd/websocket/", "/drafts/notes.Rmd/websocket/", "/%5fpartial.rmd/websocket/",
                 "/other.qmd/websocket/")) {
    handle_websocket_connection(list(request = list(PATH_INFO = paste0("/proxy/reports", path), HTTP_HOST = "localhost"),
      close = function() closed <<- closed + 1,
      onMessage = function(...) stop("The socket must be rejected")), config, cm)
  }
  expect_equal(closed, 4)
  expect_length(config$get_all_ws_connections(), 0)
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

test_that("a backend handshake that times out is tried again, keeping the browser's messages", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  sockets <- list()
  local_mocked_bindings(WebSocket = list(new = function(...) {
    socket <- new.env()
    socket$sent <- list()
    socket$closed <- FALSE
    socket$onMessage <- function(f) socket$message <- f
    socket$onOpen <- function(f) socket$open <- f
    socket$onError <- function(f) socket$error <- f
    socket$onClose <- function(f) socket$closed_by_app <- f
    socket$send <- function(data) socket$sent[[length(socket$sent) + 1]] <- data
    socket$close <- function() socket$closed <- TRUE
    sockets[[length(sockets) + 1]] <<- socket
    socket
  }), .package = "websocket")
  local_mocked_bindings(process_owns_port = function(process, port) TRUE)
  config$add_app_process("app", test_backend_process())
  cm <- ConnectionManager$new(config)
  closed <- character()
  connect <- function(session) {
    client <- list(close = function() closed <<- c(closed, session), send = function(data) NULL)
    cm$add_client_connection(session, client, "app", "127.0.0.1", "test")
    cm$create_backend_connection("app", session, client)
  }
  run_until <- function(condition) {
    deadline <- Sys.time() + 5
    while (!condition() && Sys.time() < deadline) later::run_now(0.05)
  }
  # Longer than the retry's wait of half a second. later::run_now(1) would
  # return as soon as any other callback had run.
  run_past_retry <- function() {
    deadline <- Sys.time() + 1
    while (Sys.time() < deadline) later::run_now(0.05)
  }
  timeout <- list(message = "The opening handshake timed out")

  # A busy app completes the handshake only once it is free again
  connect("busy")
  expect_true(cm$handle_client_message("busy", "init", "app"))
  sockets[[1]]$error(timeout)
  expect_length(closed, 0)
  run_until(function() length(sockets) == 2)
  expect_length(sockets, 2)
  expect_true(cm$handle_client_message("busy", "update", "app"))
  sockets[[2]]$open(list())
  expect_equal(sockets[[2]]$sent, list("init", "update"))
  expect_true(config$get_backend_connection("busy")$ready)
  # The failed socket's late events no longer act on the session
  sockets[[1]]$error(timeout)
  expect_length(closed, 0)

  # A session that ends while it waits is not connected again
  connect("left")
  sockets[[3]]$error(timeout)
  cm$remove_client_connection("left")
  run_past_retry()
  expect_length(sockets, 3)

  # The app is not waited for without end, and a refused connection fails at once
  config$WS_BACKEND_CONNECT_SECONDS <- 0
  connect("slow")
  sockets[[4]]$error(timeout)
  expect_equal(closed, "slow")
  config$WS_BACKEND_CONNECT_SECONDS <- 120
  connect("refused")
  sockets[[5]]$error(list(message = "Connection refused"))
  expect_equal(closed, c("slow", "refused"))
  run_past_retry()
  expect_length(sockets, 5)
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

test_that("an R Markdown app's socket is refused for any other file, however the path is spelled", {
  app_dir <- tempfile("tss-rmd")
  dir.create(file.path(app_dir, "drafts"), recursive = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  for (name in c("report.Rmd", "_child.Rmd", "helper.R", "notes.txt", "drafts/x.Rmd")) {
    writeLines("", file.path(app_dir, name))
  }
  document <- list(type = "rmd", file = file.path(app_dir, "report.Rmd"))
  # rmarkdown decodes the path before it removes "/websocket/"
  for (path in c("/_child.Rmd%2Fwebsocket%2F", "/_child.Rmd/websocket%2F", "/%5Fchild.Rmd/websocket/",
                 "/_child.Rmd/", "/_child.Rmd//websocket/", "/helper.R/websocket/", "/notes.txt/websocket/",
                 "/drafts%2Fx.Rmd/websocket/", "/missing.Rmd/websocket/")) {
    expect_true(is_other_document_path(document, path, websocket = TRUE), info = path)
  }
  # The document's own socket, and paths that name no file or a directory
  for (path in c("/websocket/", "/report.Rmd/websocket/", "/autoreload/", "/drafts/websocket/", "")) {
    expect_false(is_other_document_path(document, path, websocket = TRUE), info = path)
  }
})

test_that("a socket for another document does not start a dormant R Markdown app", {
  app_dir <- tempfile("tss-rmd")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  writeLines(c("---", "runtime: shiny", "---"), file.path(app_dir, "report.Rmd"))
  writeLines("", file.path(app_dir, "_child.Rmd"))
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "reports", path = app_dir, port = 1, resident = FALSE)))
  starts <- 0
  pm <- list(start_app_on_demand = function(app_name) {
    starts <<- starts + 1
    FALSE
  })
  closed <- 0
  handle_websocket_connection(list(
    request = list(PATH_INFO = "/proxy/reports/_child.Rmd%2Fwebsocket%2F", HTTP_HOST = "localhost"),
    close = function() closed <<- closed + 1
  ), config, ConnectionManager$new(config, pm), pm)
  expect_equal(closed, 1)
  expect_equal(starts, 0)
})
