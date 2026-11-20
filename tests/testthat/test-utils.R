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

test_that("get_client_ip returns unknown for missing info", {
  req <- list()
  result <- get_client_ip(req)
  expect_equal(result, "unknown")
})

# ============================================================================
# generate_session_id() tests
# ============================================================================

test_that("generate_session_id creates 64-character hex string", {
  req <- list(
    HTTP_USER_AGENT = "Mozilla/5.0",
    REMOTE_ADDR = "127.0.0.1"
  )

  session_id <- generate_session_id(req)

  expect_equal(nchar(session_id), 64)
  expect_match(session_id, "^[a-f0-9]{64}$")
})

test_that("generate_session_id creates unique IDs", {
  req <- list(
    HTTP_USER_AGENT = "Mozilla/5.0",
    REMOTE_ADDR = "127.0.0.1"
  )

  # Generate multiple IDs - they should be different due to random salt and timestamp
  id1 <- generate_session_id(req)
  Sys.sleep(0.01)
  id2 <- generate_session_id(req)

  expect_false(id1 == id2)
})

test_that("generate_session_id handles missing request fields", {
  req <- list()

  session_id <- generate_session_id(req)

  expect_equal(nchar(session_id), 64)
  expect_match(session_id, "^[a-f0-9]{64}$")
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
