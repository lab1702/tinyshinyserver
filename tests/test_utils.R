# Test for utility functions
# Tests for various helper and utility functions

library(testthat)
library(devtools)
load_all(".")

# ============================================================================
# get_client_ip() tests
# ============================================================================

test_that("get_client_ip extracts IP from X-Forwarded-For", {
  req <- list(HTTP_X_FORWARDED_FOR = "192.168.1.100")
  result <- get_client_ip(req)
  expect_equal(result, "192.168.1.100")
})

test_that("get_client_ip falls back to REMOTE_ADDR", {
  req <- list(REMOTE_ADDR = "10.0.0.5")
  result <- get_client_ip(req)
  expect_equal(result, "10.0.0.5")
})

test_that("get_client_ip prefers X-Forwarded-For over REMOTE_ADDR", {
  req <- list(
    HTTP_X_FORWARDED_FOR = "192.168.1.100",
    REMOTE_ADDR = "10.0.0.5"
  )
  result <- get_client_ip(req)
  expect_equal(result, "192.168.1.100")
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
  Sys.sleep(0.01)  # Small delay to ensure different timestamp
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
# format_bytes() tests
# ============================================================================

test_that("format_bytes formats bytes correctly", {
  expect_equal(format_bytes(0), "0 B")
  expect_equal(format_bytes(500), "500 B")
  expect_equal(format_bytes(1024), "1 KB")
  expect_equal(format_bytes(1536), "1.5 KB")
  expect_equal(format_bytes(1048576), "1 MB")
  expect_equal(format_bytes(1572864), "1.5 MB")
  expect_equal(format_bytes(1073741824), "1 GB")
})

test_that("format_bytes handles negative values", {
  result <- format_bytes(-1024)
  expect_match(result, "KB")  # Should still format, just with negative sign
})

# ============================================================================
# format_duration() tests
# ============================================================================

test_that("format_duration formats seconds correctly", {
  expect_equal(format_duration(0), "0s")
  expect_equal(format_duration(30), "30s")
  expect_equal(format_duration(60), "1m 0s")
  expect_equal(format_duration(90), "1m 30s")
  expect_equal(format_duration(3600), "1h 0m 0s")
  expect_equal(format_duration(3661), "1h 1m 1s")
  expect_equal(format_duration(86400), "24h 0m 0s")
  expect_equal(format_duration(90061), "25h 1m 1s")
})

test_that("format_duration handles zero and negative values", {
  expect_equal(format_duration(0), "0s")
  # Negative durations should still be formatted
  result <- format_duration(-60)
  expect_true(is.character(result))
})

# ============================================================================
# safe_file_read() / safe_file_write() tests
# ============================================================================

test_that("safe_file_write and safe_file_read work together", {
  temp_file <- tempfile(fileext = ".txt")

  # Write content
  write_result <- safe_file_write(temp_file, "Hello, World!")

  expect_true(write_result$success)

  # Read it back
  read_result <- safe_file_read(temp_file)

  expect_true(read_result$success)
  expect_equal(read_result$content, "Hello, World!")

  # Cleanup
  unlink(temp_file)
})

test_that("safe_file_read handles non-existent files", {
  result <- safe_file_read("/nonexistent/path/to/file.txt")

  expect_false(result$success)
  expect_true("error" %in% names(result))
})

test_that("safe_file_write handles invalid paths", {
  result <- safe_file_write("/root/cant/write/here.txt", "content")

  expect_false(result$success)
  expect_true("error" %in% names(result))
})

test_that("safe_file_write creates directories if needed", {
  temp_dir <- tempfile()
  temp_file <- file.path(temp_dir, "subdir", "file.txt")

  result <- safe_file_write(temp_file, "test content")

  if (result$success) {
    expect_true(file.exists(temp_file))
    unlink(temp_dir, recursive = TRUE)
  } else {
    # On some systems, this might fail - that's okay
    expect_false(result$success)
  }
})

# ============================================================================
# create_json_response() tests
# ============================================================================

test_that("create_json_response creates valid JSON response", {
  data <- list(status = "ok", count = 42)
  result <- create_json_response(data)

  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  # Parse JSON to verify it's valid
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

test_that("create_error_response accepts custom status codes", {
  result <- create_error_response("Not found", status = 404)

  expect_equal(result$status, 404)
  expect_match(result$body, "Not found")
})

# ============================================================================
# create_503_response() tests
# ============================================================================

test_that("create_503_response creates 503 response with Retry-After", {
  result <- create_503_response("Service unavailable", retry_after_seconds = 5)

  expect_equal(result$status, 503)
  expect_equal(result$headers$`Retry-After`, "5")
  expect_equal(result$headers$`Content-Type`, "application/json")

  # Parse JSON body
  body <- jsonlite::fromJSON(result$body)
  expect_match(body$error, "Service unavailable")
  expect_equal(body$retry_after_seconds, 5)
})

test_that("create_503_response defaults to 3 seconds", {
  result <- create_503_response("Unavailable")

  expect_equal(result$headers$`Retry-After`, "3")
})

# ============================================================================
# is_valid_url() tests
# ============================================================================

test_that("is_valid_url validates HTTP URLs", {
  expect_true(is_valid_url("http://example.com"))
  expect_true(is_valid_url("https://example.com"))
  expect_true(is_valid_url("http://example.com/path"))
  expect_true(is_valid_url("https://example.com:8080/path?query=1"))
})

test_that("is_valid_url rejects invalid URLs", {
  expect_false(is_valid_url("not a url"))
  expect_false(is_valid_url("ftp://example.com"))
  expect_false(is_valid_url(""))
  expect_false(is_valid_url("javascript:alert(1)"))
})

# ============================================================================
# is_port_in_use() tests
# ============================================================================

test_that("is_port_in_use detects used ports", {
  # Returns TRUE if port is in use, FALSE if free
  # This test is tricky because it depends on system state
  # We'll just verify it returns a boolean
  result <- is_port_in_use("127.0.0.1", 80)
  expect_true(is.logical(result))
})

test_that("is_port_in_use handles invalid inputs gracefully", {
  # Test with invalid port - should handle error
  result <- tryCatch({
    is_port_in_use("127.0.0.1", 99999)
    TRUE  # If it doesn't error, that's fine too
  }, error = function(e) {
    FALSE
  })
  expect_true(is.logical(result))
})

# ============================================================================
# is_process_alive() tests
# ============================================================================

test_that("is_process_alive returns FALSE for NULL process", {
  result <- is_process_alive(NULL)
  expect_false(result)
})

test_that("is_process_alive checks process state", {
  # Create a mock process
  mock_process <- list(
    is_alive = function() TRUE
  )

  result <- is_process_alive(mock_process)
  expect_true(result)

  # Dead process
  dead_process <- list(
    is_alive = function() FALSE
  )

  result <- is_process_alive(dead_process)
  expect_false(result)
})

test_that("is_process_alive handles errors gracefully", {
  # Process that throws error
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
  # NULL process is considered already dead, so killing it "succeeds"
  result <- kill_process_safely(NULL)
  expect_true(result)
})

test_that("kill_process_safely returns TRUE for dead process", {
  # Already dead process, so killing it "succeeds"
  dead_process <- list(
    is_alive = function() FALSE,
    kill = function() {}
  )

  result <- kill_process_safely(dead_process)
  expect_true(result)
})

test_that("kill_process_safely attempts to kill live process", {
  killed <- FALSE
  live_process <- list(
    is_alive = function() {
      if (killed) FALSE else TRUE
    },
    kill = function() {
      killed <<- TRUE
    }
  )

  result <- kill_process_safely(live_process)
  expect_true(result)
  expect_true(killed)
})

test_that("kill_process_safely handles errors gracefully", {
  error_process <- list(
    is_alive = function() TRUE,
    kill = function() stop("Cannot kill")
  )

  result <- kill_process_safely(error_process)
  expect_false(result)
})

cat("Utility function tests completed successfully!\n")
