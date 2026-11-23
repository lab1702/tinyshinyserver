# Tests for the limits on WebSocket sessions: what the proxy holds for a
# browser or an app, which handshakes it refuses, and how it logs them

# Mocked app sockets, in the order the proxy opens them. The mocks belong to
# the calling test.
mock_app_sockets <- function(env = parent.frame()) {
  sockets <- new.env()
  sockets$all <- list()
  local_mocked_bindings(WebSocket = list(new = function(url, ...) {
    socket <- new.env()
    socket$url <- url
    socket$sent <- list()
    socket$closed <- FALSE
    socket$onMessage <- function(f) socket$message <- f
    socket$onOpen <- function(f) socket$open <- f
    socket$onError <- function(f) socket$error <- f
    socket$onClose <- function(f) socket$closed_by_app <- f
    socket$send <- function(data) socket$sent[[length(socket$sent) + 1]] <- data
    socket$close <- function() socket$closed <- TRUE
    sockets$all[[length(sockets$all) + 1]] <- socket
    socket
  }), .package = "websocket", .env = env)
  local_mocked_bindings(process_owns_port = function(process, port) TRUE, .env = env)
  sockets
}

# A browser's socket as httpuv presents it to handle_websocket_connection().
# httpuv runs the close handler of a socket that the server closes once the
# browser answers the close, which a client that misbehaves never does:
# answers_close = FALSE.
mock_browser_socket <- function(path = "/proxy/app/websocket/", ..., answers_close = TRUE) {
  browser <- new.env()
  browser$request <- list(PATH_INFO = path, HTTP_HOST = "localhost", ...)
  browser$sent <- list()
  browser$closed <- 0
  browser$onMessage <- function(f) browser$message <- f
  browser$onClose <- function(f) browser$on_close <- f
  browser$send <- function(data) browser$sent[[length(browser$sent) + 1]] <- data
  browser$close <- function() {
    browser$closed <- browser$closed + 1
    if (answers_close && is.function(browser$on_close)) browser$on_close()
  }
  browser
}

test_config <- function(...) {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)), ...)
  config$add_app_process("app", test_backend_process())
  config
}

# ============================================================================
# What the app sends a browser that does not answer
# ============================================================================

test_that("a session on which the app sends more than max_unanswered_mb without a browser message is closed", {
  # A limit of 1000 bytes
  config <- test_config(max_unanswered_mb = 1000 / 1024^2)
  sockets <- mock_app_sockets()
  cm <- ConnectionManager$new(config)
  connect <- function(session) {
    client <- new.env()
    client$received <- list()
    client$closed <- 0
    client$send <- function(data) client$received[[length(client$received) + 1]] <- data
    client$close <- function() client$closed <- client$closed + 1
    cm$add_client_connection(session, client, "app", "127.0.0.1", "test")
    socket <- cm$create_backend_connection("app", session, client)
    socket$open(list())
    list(client = client, socket = socket)
  }
  chunk <- strrep("x", 600)

  a <- connect("a")
  logged <- capture_test_log({
    a$socket$message(list(data = chunk))
    a$socket$message(list(data = chunk))
  })
  expect_length(a$client$received, 1)
  expect_equal(a$client$closed, 1)
  expect_true(a$socket$closed)
  expect_null(config$get_ws_connection("a"))
  expect_null(config$get_backend_connection("a"))
  expect_false(exists("a", envir = config$session_unanswered, inherits = FALSE))
  expect_true(any(grepl("Closing session a of app app: the app sent more than max_unanswered_mb", logged, fixed = TRUE)))
  # A message that the closed socket still delivers is not sent on
  a$socket$message(list(data = "late"))
  expect_length(a$client$received, 1)

  # Each browser message starts the count again, and binary messages count by their bytes
  b <- connect("b")
  b$socket$message(list(data = chunk))
  expect_true(cm$handle_client_message("b", "{}", "app"))
  b$socket$message(list(data = as.raw(rep(1, 600))))
  expect_length(b$client$received, 2)
  expect_equal(b$client$closed, 0)
  b$socket$message(list(data = chunk))
  expect_equal(b$client$closed, 1)

  # Zero sets no limit
  config$config$max_unanswered_mb <- 0
  c <- connect("c")
  for (i in 1:10) c$socket$message(list(data = chunk))
  expect_length(c$client$received, 10)
  expect_equal(c$client$closed, 0)
  expect_false(exists("c", envir = config$session_unanswered, inherits = FALSE))

  # Without the field, the default of 256 MB applies
  config$config$max_unanswered_mb <- NULL
  d <- connect("d")
  for (i in 1:10) d$socket$message(list(data = chunk))
  expect_length(d$client$received, 10)
  expect_equal(config$session_unanswered[["d"]], 6000)
})

test_that("max_unanswered_mb defaults to 256 and must be a non-negative number", {
  expect_equal(read_test_config()$max_unanswered_mb, 256)
  expect_equal(read_test_config(max_unanswered_mb = 0)$max_unanswered_mb, 0)
  expect_equal(read_test_config(max_unanswered_mb = 1.5)$max_unanswered_mb, 1.5)
  for (value in list(-1, "10", TRUE, c(1, 2))) {
    expect_error(read_test_config(max_unanswered_mb = value), "max_unanswered_mb must be", info = format(value))
  }
  settings <- list(apps = list(list(name = "app", path = tempdir())), log_dir = tempdir(), starting_port = 3001)
  for (value in list(Inf, NA_real_)) {
    result <- ShinyServerConfig$new()$validate_config(c(settings, list(max_unanswered_mb = value)))
    expect_false(result$valid, info = format(value))
    expect_match(result$error, "max_unanswered_mb must be")
  }
})

# ============================================================================
# What the browser sends before the app's socket opens
# ============================================================================

test_that("a session whose browser sends more than is kept while the app's socket opens is closed", {
  config <- test_config()
  config$MAX_PENDING_MESSAGES <- 3
  sockets <- mock_app_sockets()
  cm <- ConnectionManager$new(config)
  closed <- character()
  connect <- function(session) {
    client <- list(close = function() closed <<- c(closed, session), send = function(data) NULL)
    cm$add_client_connection(session, client, "app", "127.0.0.1", "test")
    cm$create_backend_connection("app", session, client)
  }

  # No message is dropped to make room: Shiny's first one starts the session
  socket <- connect("many")
  for (message in c("init", "update 1", "update 2")) {
    expect_true(cm$handle_client_message("many", message, "app"))
  }
  expect_equal(config$get_backend_connection("many")$pending_messages, list("init", "update 1", "update 2"))
  logged <- capture_test_log(expect_true(cm$handle_client_message("many", "update 3", "app")))
  expect_equal(closed, "many")
  expect_true(socket$closed)
  expect_null(config$get_ws_connection("many"))
  expect_null(config$get_backend_connection("many"))
  expect_true(any(grepl("Closing session many of app app: more messages arrived", logged, fixed = TRUE)))
  # A socket that opens all the same is closed and sent nothing
  socket$closed <- FALSE
  socket$open(list())
  expect_true(socket$closed)
  expect_length(socket$sent, 0)

  # The queue is also limited by its size
  config$MAX_PENDING_MESSAGES <- 100
  config$MAX_PENDING_BYTES <- 10
  connect("large")
  expect_true(cm$handle_client_message("large", "123456", "app"))
  expect_equal(closed, "many")
  expect_true(cm$handle_client_message("large", "123456", "app"))
  expect_equal(closed, c("many", "large"))

  # Messages that fit all reach the app in order
  config$MAX_PENDING_BYTES <- 16 * 1024^2
  socket <- connect("fits")
  for (message in c("init", "a", "b")) {
    expect_true(cm$handle_client_message("fits", message, "app"))
  }
  socket$open(list())
  expect_equal(socket$sent, list("init", "a", "b"))
  expect_equal(closed, c("many", "large"))
})

test_that("an oversized or invalid browser message is answered with an error and ends the session", {
  config <- test_config()
  sockets <- mock_app_sockets()
  cm <- ConnectionManager$new(config)
  messages <- list(strrep("x", 1048577), rawToChar(as.raw(c(0x61, 0xff))), as.raw(rep(0, 1048577)))
  # Whether or not the browser answers the close, the session ends at once
  for (answers_close in c(TRUE, FALSE)) for (message in messages) {
    browser <- mock_browser_socket(answers_close = answers_close)
    handle_websocket_connection(browser, config, cm)
    expect_length(config$get_all_ws_connections(), 1)
    expect_equal(config$get_app_connection_count("app"), 1)
    socket <- sockets$all[[length(sockets$all)]]
    socket$open(list())
    logged <- capture_test_log(browser$message(is.raw(message), message))
    expect_length(browser$sent, 1)
    expect_equal(as.character(browser$sent[[1]]), '{"error":"Invalid message"}')
    expect_equal(browser$closed, 1)
    expect_length(config$get_all_ws_connections(), 0)
    expect_length(config$get_all_backend_connections(), 0)
    expect_equal(config$get_app_connection_count("app"), 0)
    expect_true(socket$closed)
    expect_length(socket$sent, 0)
    expect_true(any(grepl("Invalid WebSocket message for app app, session", logged, fixed = TRUE)))

    # What a browser that ignored the close sends next reaches no app
    browser$message(FALSE, "after the close")
    expect_length(socket$sent, 0)
    expect_length(browser$sent, 1)
    expect_equal(browser$closed, 1)
    if (is.function(browser$on_close)) browser$on_close()
  }
})

# ============================================================================
# The app's socket
# ============================================================================

test_that("an app socket that opens after its session ended or was replaced closes itself", {
  config <- test_config()
  sockets <- mock_app_sockets()
  cm <- ConnectionManager$new(config)
  client <- list(close = function() NULL, send = function(data) NULL)

  cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
  ended <- cm$create_backend_connection("app", "s", client)
  cm$remove_client_connection("s")
  expect_true(ended$closed)
  # Closing a socket whose handshake is under way does not stop it, so it may open yet
  ended$closed <- FALSE
  ended$open(list())
  expect_true(ended$closed)
  expect_null(config$get_backend_connection("s"))

  cm$add_client_connection("t", client, "app", "127.0.0.1", "test")
  replaced <- cm$create_backend_connection("app", "t", client)
  current <- cm$create_backend_connection("app", "t", client)
  replaced$open(list())
  expect_true(replaced$closed)
  expect_false(current$closed)
  expect_identical(config$get_backend_connection("t")$ws, current)
  expect_false(config$get_backend_connection("t")$ready)
  current$open(list())
  expect_true(config$get_backend_connection("t")$ready)
})

# ============================================================================
# Handshakes
# ============================================================================

test_that("a WebSocket handshake's query string has the length limit of an HTTP request's", {
  config <- test_config()
  sockets <- mock_app_sockets()
  cm <- ConnectionManager$new(config)
  logged <- capture_test_log({
    long <- mock_browser_socket(QUERY_STRING = paste0("?token=", strrep("a", 8192)))
    handle_websocket_connection(long, config, cm)
  })
  expect_equal(long$closed, 1)
  expect_length(config$get_all_ws_connections(), 0)
  expect_length(sockets$all, 0)
  expect_true(any(grepl("Query string too long", logged, fixed = TRUE)))

  fits <- mock_browser_socket(QUERY_STRING = paste0("?", strrep("a", 8192)))
  handle_websocket_connection(fits, config, cm)
  expect_equal(fits$closed, 0)
  expect_length(config$get_all_ws_connections(), 1)
  expect_true(endsWith(sockets$all[[1]]$url, paste0("/websocket/?", strrep("a", 8192))))
})

test_that("a flood of refused handshakes is logged at WARN once per reason in each period", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 1, resident = TRUE)))
  cm <- ConnectionManager$new(config)
  refuse <- function(...) {
    handle_websocket_connection(list(request = list(PATH_INFO = "/proxy/app/websocket/", ...),
      close = function() NULL), config, cm)
  }
  logged <- capture_test_log(for (i in 1:20) {
    refuse(HTTP_HOST = "evil.example")
    refuse(HTTP_HOST = "localhost", HTTP_ORIGIN = "https://evil.example")
    refuse(HTTP_HOST = "localhost", QUERY_STRING = strrep("a", 9000))
  })
  expect_length(grep("Rejecting WebSocket with Host evil.example", logged, fixed = TRUE), 1)
  expect_length(grep("Rejecting cross-origin WebSocket from https://evil.example", logged, fixed = TRUE), 1)
  expect_length(grep("Invalid WebSocket query string", logged, fixed = TRUE), 1)
  # Nothing else is logged at INFO for a handshake that is then refused
  expect_length(logged, 3)

  # Once the period has passed, the next refusal is logged with the number
  # that were logged only at DEBUG
  entry <- cm$rejected_handshakes[["host"]]
  entry$logged_at <- entry$logged_at - REJECTED_HANDSHAKE_LOG_SECONDS - 1
  assign("host", entry, envir = cm$rejected_handshakes)
  logged <- capture_test_log(refuse(HTTP_HOST = "evil.example"))
  expect_length(logged, 1)
  expect_match(logged, "Rejecting WebSocket with Host evil.example (and 19 more for the same reason", fixed = TRUE)
  expect_length(capture_test_log(refuse(HTTP_HOST = "evil.example")), 0)
})

test_that("WebSocket messages are logged short and without control characters", {
  expect_equal(describe_ws_message(as.raw(1:3)), "3 binary bytes")
  described <- describe_ws_message(paste0("{\"a\":1}\033[2J\r", strrep("x", 500)))
  expect_false(grepl("[\001-\037]", described, useBytes = TRUE))
  expect_true(startsWith(described, "{\"a\":1}?[2J?xx"))
  expect_lte(nchar(described, type = "bytes"), 110)
})

# ============================================================================
# Documents
# ============================================================================

test_that("an R Markdown app's socket path that leaves its directory is refused, whether or not the file exists", {
  root <- tempfile("tss-root")
  app_dir <- file.path(root, "app")
  dir.create(app_dir, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (file in c(file.path(app_dir, "report.Rmd"), file.path(app_dir, "notes.txt"), file.path(root, "secret.txt"))) {
    writeLines("", file)
  }
  document <- list(type = "rmd", file = file.path(app_dir, "report.Rmd"))
  # The answer is the same for a file that exists and one that does not, so
  # it tells a client nothing about files outside the app's directory
  for (path in c("/%2e%2e/secret.txt/websocket/", "/%2e%2e/missing.txt/websocket/", "/%2E%2E%2Fsecret.txt/websocket/",
                 "/sub/%2e%2e/%2e%2e/secret.txt/websocket/", "/..%5Csecret.txt/websocket/", "/notes%5c.txt/websocket/",
                 "/%2e%2e/websocket/", "/%2e%2e/app/notes.txt/websocket/")) {
    expect_true(is_other_document_path(document, path, websocket = TRUE), info = path)
  }
  # Inside the directory, a file is refused and a path that names none is not
  expect_true(is_other_document_path(document, "/notes.txt/websocket/", websocket = TRUE))
  expect_false(is_other_document_path(document, "/missing/websocket/", websocket = TRUE))
  expect_false(is_other_document_path(document, "/report.Rmd/websocket/", websocket = TRUE))
  # HTTP paths are left to the app, as before
  expect_false(is_other_document_path(document, "/%2e%2e/secret.txt", websocket = FALSE))
})

# ============================================================================
# Idle checks after large responses
# ============================================================================

test_that("the idle check of an on-demand app allows a slow browser time to download a large response", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = FALSE)))
  config$add_app_process("app", list(generation = 1))
  pm <- list(start_app_on_demand = function(...) TRUE, stop_app_immediately = function(...) TRUE)
  cm <- ConnectionManager$new(config, pm)
  delays <- numeric()
  local_mocked_bindings(later = function(func, delay, ...) {
    delays <<- c(delays, delay)
    function() NULL
  }, .package = "later")
  # proc.time() can read the same time when a download's end is set and when
  # the time left is taken from it, and adding the download time to that time
  # and taking it away again can leave a rounding error above the download time
  last_delay <- function() round(tail(delays, 1), 6)

  # A small response leaves the grace period as it is
  cm$begin_http_request("app")
  cm$end_http_request("app", 100000)
  expect_equal(last_delay(), 30)

  # 10 MB take 80 seconds at 128 KB/s. The lower bounds below leave room for
  # a busy machine to take seconds between the end of a request and the check.
  local_mocked_bindings(forward_request = function(...) create_html_response(strrep("x", 10 * 1024^2)))
  expect_equal(handle_proxy_request("/proxy/app/", "GET", "", list(), config, pm, cm)$status, 200)
  expect_gt(last_delay(), 100)
  expect_lte(last_delay(), 110)

  # A small request that ends while the large response may still be on its
  # way does not shorten the wait
  cm$begin_http_request("app")
  cm$end_http_request("app", 10)
  expect_gt(last_delay(), 100)
  # Nor does a session that closes
  cm$add_client_connection("s", list(), "app", "127.0.0.1", "test")
  cm$remove_client_connection("s")
  expect_gt(last_delay(), 100)

  # The allowance has a ceiling
  cm$begin_http_request("app")
  cm$end_http_request("app", 4 * 1024^3)
  expect_gt(last_delay(), 620)
  expect_lte(last_delay(), 630)
})

test_that("the size of a response that an app sends later, or from a file, is allowed for", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = FALSE)))
  config$add_app_process("app", list(generation = 1))
  pm <- list(start_app_on_demand = function(...) TRUE, stop_app_immediately = function(...) TRUE)
  cm <- ConnectionManager$new(config, pm)
  on.exit(cm$cancel_session_check("app"), add = TRUE)
  local_mocked_bindings(forward_request = function(...) {
    promises::promise_resolve(list(status = 200L, headers = list(), body = raw(3 * 1024^2)))
  })
  response <- await_response(handle_proxy_request("/proxy/app/", "GET", "", list(), config, pm, cm))
  expect_equal(response$status, 200)
  expect_equal(config$active_http_requests[["app"]], 0L)
  # Without the rounding error that the test above describes
  left <- round(cm$transfer_seconds_left("app"), 6)
  expect_gt(left, 20)
  expect_lte(left, 24)

  spooled <- tempfile("tss-response-")
  on.exit(unlink(spooled), add = TRUE)
  writeBin(raw(5000), spooled)
  expect_equal(response_body_bytes(list(file = spooled, owned = TRUE)), 5000)
  expect_equal(response_body_bytes(list(file = tempfile())), 0)
  expect_equal(response_body_bytes("café"), 5)
  expect_equal(response_body_bytes(raw(7)), 7)
  expect_equal(response_body_bytes(NULL), 0)
})
