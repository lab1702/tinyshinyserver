# Test for utility functions
# Tests for various helper and utility functions

# ============================================================================
# get_client_ip() tests
# ============================================================================

test_that("get_client_ip extracts IP from X-Forwarded-For behind a local proxy", {
  req <- list(HTTP_X_FORWARDED_FOR = "192.168.1.100", REMOTE_ADDR = "127.0.0.1")
  result <- get_client_ip(req)
  expect_equal(result, "192.168.1.100")
})

test_that("get_client_ip falls back to REMOTE_ADDR", {
  req <- list(REMOTE_ADDR = "10.0.0.5")
  result <- get_client_ip(req)
  expect_equal(result, "10.0.0.5")
})

test_that("get_client_ip ignores forwarding headers from non-local peers", {
  req <- list(
    HTTP_X_FORWARDED_FOR = "192.168.1.100",
    HTTP_X_REAL_IP = "192.168.1.101",
    REMOTE_ADDR = "10.0.0.5"
  )
  result <- get_client_ip(req)
  expect_equal(result, "10.0.0.5")
  expect_equal(get_client_ip(list(HTTP_X_FORWARDED_FOR = "192.168.1.100")), "unknown")
})

test_that("get_client_ip uses the entry added by the local proxy", {
  req <- list(
    HTTP_X_FORWARDED_FOR = "1.2.3.4, 203.0.113.7",
    REMOTE_ADDR = "127.0.0.1"
  )
  expect_equal(get_client_ip(req), "203.0.113.7")
  req <- list(HTTP_X_REAL_IP = "203.0.113.8", REMOTE_ADDR = "::1")
  expect_equal(get_client_ip(req), "203.0.113.8")
})

test_that("get_client_ip ignores a forwarding header spelled with an underscore", {
  # httpuv reads X_Forwarded_For in place of the header the reverse proxy set
  req <- httpuv_request(c("x-forwarded-for" = "203.0.113.7", "x_forwarded_for" = "198.51.100.1"),
    REMOTE_ADDR = "127.0.0.1")
  expect_equal(req$HTTP_X_FORWARDED_FOR, "198.51.100.1")
  expect_equal(get_client_ip(req), "203.0.113.7")
  spoofed_only <- httpuv_request(c("x_real_ip" = "198.51.100.1"), REMOTE_ADDR = "127.0.0.1")
  expect_equal(get_client_ip(spoofed_only), "127.0.0.1")
})

test_that("request headers are read under the names the client sent", {
  req <- httpuv_request(c("content-type" = "text/plain", "x-one" = "1", "x_one" = "2"))
  expect_equal(request_header(req, "x-one"), "1")
  expect_equal(request_header(req, "x_one"), "2")
  expect_equal(request_header(req, "content-type"), "text/plain")
  expect_null(request_header(req, "x-two"))

  # A request object without HEADERS is read from its HTTP_ entries
  plain <- list(HTTP_X_ONE = "1", CONTENT_LENGTH = "3", CONTENT_TYPE = "text/plain", PATH_INFO = "/")
  expect_equal(request_headers(plain), c("x-one" = "1", "content-length" = "3", "content-type" = "text/plain"))
  expect_length(request_headers(list()), 0)
  expect_null(request_header(list(), "host"))
})

test_that("get_client_ip returns unknown for missing info", {
  req <- list()
  result <- get_client_ip(req)
  expect_equal(result, "unknown")
})

# ============================================================================
# generate_session_id() tests
# ============================================================================

test_that("generate_session_id creates 64-character hex string", {
  session_id <- generate_session_id()

  expect_equal(nchar(session_id), 64)
  expect_match(session_id, "^[a-f0-9]{64}$")
})

test_that("generate_session_id creates unique IDs", {
  ids <- vapply(1:50, function(i) generate_session_id(), "")

  expect_equal(length(unique(ids)), 50)
})

# ============================================================================
# create_json_response() tests
# ============================================================================

test_that("create_json_response creates valid JSON response", {
  data <- list(status = "ok", count = 42)
  result <- create_json_response(data)

  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  parsed <- jsonlite::fromJSON(result$body)
  expect_equal(parsed$status, "ok")
  expect_equal(parsed$count, 42)
})

test_that("create_json_response accepts custom status codes", {
  result <- create_json_response(list(error = "Not found"), status = 404)

  expect_equal(result$status, 404)
  expect_equal(result$headers$`Content-Type`, "application/json")
})

# ============================================================================
# create_html_response() tests
# ============================================================================

test_that("create_html_response creates HTML response", {
  result <- create_html_response("<html><body>Test</body></html>")

  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "text/html")
  expect_equal(result$body, "<html><body>Test</body></html>")
})

test_that("create_html_response accepts custom status codes", {
  result <- create_html_response("<html>Error</html>", status = 500)

  expect_equal(result$status, 500)
})

# ============================================================================
# create_error_response() tests
# ============================================================================

test_that("create_error_response creates error response", {
  result <- create_error_response("Something went wrong", status = 500)

  expect_equal(result$status, 500)
  expect_equal(result$headers$`Content-Type`, "application/json")
  expect_match(result$body, "Something went wrong")
})

test_that("create_error_response defaults to 500", {
  result <- create_error_response("Error")

  expect_equal(result$status, 500)
})

# ============================================================================
# create_503_response() tests
# ============================================================================

test_that("create_503_response creates 503 response with Retry-After", {
  result <- create_503_response("Service unavailable", retry_after_seconds = 5)

  expect_equal(result$status, 503)
  expect_equal(result$headers$`Retry-After`, "5")
  expect_equal(result$headers$`Content-Type`, "application/json")

  body <- jsonlite::fromJSON(result$body)
  expect_match(body$error, "Service unavailable")
  expect_equal(body$retry_after_seconds, 5)
})

test_that("create_503_response defaults to 3 seconds", {
  result <- create_503_response("Unavailable")

  expect_equal(result$headers$`Retry-After`, "3")
})

# ============================================================================
# is_port_in_use() tests
# ============================================================================

test_that("is_port_in_use detects open and closed ports without warnings", {
  backend <- start_test_http_server(function(req) create_html_response("OK"))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  expect_no_warning(expect_true(is_port_in_use("127.0.0.1", backend$port)))
  httpuv::stopServer(backend$server)
  # httpuv closes the listener asynchronously on its background thread
  deadline <- Sys.time() + 5
  repeat {
    in_use <- expect_no_warning(is_port_in_use("127.0.0.1", backend$port))
    if (!in_use || Sys.time() > deadline) break
    Sys.sleep(0.05)
  }
  expect_false(in_use)
})

test_that("is_port_in_use answers for a free port without waiting for a refused connection", {
  # Connecting to a free loopback port takes over a second on Windows
  skip_on_cran()
  backend <- start_test_http_server(function(req) create_html_response("OK"))
  httpuv::stopServer(backend$server)
  deadline <- Sys.time() + 5
  while (is_port_in_use("127.0.0.1", backend$port) && Sys.time() < deadline) Sys.sleep(0.05)
  elapsed <- system.time(in_use <- is_port_in_use("127.0.0.1", backend$port))[["elapsed"]]
  expect_false(in_use)
  expect_lt(elapsed, 0.5)
})

# ============================================================================
# is_process_alive() tests
# ============================================================================

test_that("is_process_alive returns FALSE for NULL process", {
  result <- is_process_alive(NULL)
  expect_false(result)
})

test_that("is_process_alive checks process state", {
  mock_process <- list(
    is_alive = function() TRUE
  )

  result <- is_process_alive(mock_process)
  expect_true(result)

  dead_process <- list(
    is_alive = function() FALSE
  )

  result <- is_process_alive(dead_process)
  expect_false(result)
})

test_that("is_process_alive handles errors gracefully", {
  error_process <- list(
    is_alive = function() stop("Error")
  )

  result <- is_process_alive(error_process)
  expect_false(result)
})

# ============================================================================
# kill_process_safely() tests
# ============================================================================

test_that("kill_process_safely returns TRUE for NULL process", {
  result <- kill_process_safely(NULL)
  expect_true(result)
})

test_that("kill_process_safely returns TRUE for dead process", {
  dead_process <- list(
    is_alive = function() FALSE,
    kill = function() {}
  )

  result <- kill_process_safely(dead_process)
  expect_true(result)
})

# ============================================================================
# Process termination
# ============================================================================

test_that("termination reports a process that survives kill attempts", {
  kills <- 0
  waits <- numeric()
  process <- list(is_alive = function() TRUE,
    kill = function() { kills <<- kills + 1 }, kill_tree = function() { kills <<- kills + 1 },
    wait = function(timeout) { waits <<- c(waits, timeout) })
  expect_false(kill_process_safely(process))
  expect_equal(kills, 2)
  expect_equal(waits, 1000)
})

test_that("termination waits for exit after kill signals are delivered", {
  alive <- TRUE
  events <- character()
  process <- list(
    is_alive = function() alive,
    kill_tree = function() { events <<- c(events, "tree") },
    kill = function() { events <<- c(events, "kill") },
    wait = function(timeout) {
      expect_equal(timeout, 1000)
      events <<- c(events, "wait")
      alive <<- FALSE
    }
  )
  expect_true(kill_process_safely(process))
  expect_false(alive)
  expect_identical(events, c("tree", "kill", "wait"))
})

test_that("termination tolerates tree members exiting before they are signalled", {
  alive <- TRUE
  process <- list(
    is_alive = function() alive,
    kill_tree = function() stop(structure(class = c("no_such_process", "ps_error", "error", "condition"),
      list(message = "Failed to send signal to some processes: 4002", call = NULL))),
    kill = function() NULL,
    wait = function(timeout) alive <<- FALSE
  )
  expect_true(kill_process_safely(process))
  expect_false(alive)
})

test_that("termination stops the app's own process where ps cannot list processes", {
  # processx's kill_tree() only raises an error on such platforms
  local_mocked_bindings(ps_is_supported = function() FALSE, .package = "ps")
  alive <- TRUE
  process <- list(
    is_alive = function() alive,
    kill_tree = function() stop("kill_tree is not supported on this platform"),
    kill = function() alive <<- FALSE,
    wait = function(timeout) NULL
  )
  expect_true(kill_process_safely(process))
  expect_false(alive)
})

test_that("termination stops a real child worker as well as its parent", {
  # Starts two real R processes; see the crash cleanup test above
  skip_on_cran()
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
  deadline <- Sys.time() + 30
  while ((!file.exists(marker) || !file.exists(child_pid_file)) && Sys.time() < deadline) Sys.sleep(.02)
  expect_true(file.exists(marker))
  expect_true(kill_process_safely(parent))
  expect_false(parent$is_alive())
  Sys.sleep(.1)
  size <- file.info(marker)$size
  Sys.sleep(.15)
  expect_equal(file.info(marker)$size, size)
})

# ============================================================================
# Logging
# ============================================================================

test_that("setup_logging leaves the caller's logger configuration alone", {
  global_threshold <- logger::log_threshold()
  global_appender <- logger::log_appender()
  log_dir <- tempfile("tss-logging")
  on.exit({
    logger::log_threshold(logger::INFO, namespace = "tinyshinyserver")
    logger::log_appender(logger::appender_console, namespace = "tinyshinyserver")
    unlink(log_dir, recursive = TRUE)
  }, add = TRUE)
  setup_logging(log_dir, "INFO")
  expect_identical(logger::log_threshold(), global_threshold)
  expect_identical(logger::log_appender(), global_appender)
  # Package log calls still reach the server log
  expect_true(any(grepl("Logging system initialized", readLines(file.path(log_dir, "server.log")))))
})

test_that("setup_logging rejects a log_dir where the server log cannot be written", {
  blocker <- tempfile("tss-blocker")
  on.exit(unlink(blocker), add = TRUE)
  # A directory cannot be created below a regular file
  writeLines("", blocker)
  expect_error(setup_logging(file.path(blocker, "logs"), "INFO"), "Cannot write the server log")
})

test_that("package log messages use glue whatever the global formatter", {
  log_dir <- tempfile("tss-logging")
  on.exit({
    logger::log_formatter(logger::formatter_glue, namespace = "tinyshinyserver")
    logger::log_appender(logger::appender_console, namespace = "tinyshinyserver")
    unlink(log_dir, recursive = TRUE)
  }, add = TRUE)
  logger::log_formatter(logger::formatter_sprintf, namespace = "tinyshinyserver")
  setup_logging(log_dir, "INFO")
  log <- readLines(file.path(log_dir, "server.log"))
  expect_true(any(grepl("file output to .*server\\.log", log)))
  expect_false(any(grepl("{log_file}", log, fixed = TRUE)))
})

test_that("the configured log level reaches the server log", {
  log_dir <- tempfile("tss-logging")
  on.exit(restore_test_logging(log_dir), add = TRUE)
  config <- ShinyServerConfig$new()
  debug_lines <- function() grep("Session (quiet|detailed) tracked for app", readLines(file.path(log_dir, "server.log")))

  setup_logging(log_dir, "INFO")
  config$add_ws_connection("quiet", list(app_name = "app"))
  expect_length(debug_lines(), 0)

  setup_logging(log_dir, "DEBUG")
  config$add_ws_connection("detailed", list(app_name = "app"))
  expect_length(debug_lines(), 1)
})
