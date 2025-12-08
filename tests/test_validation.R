# Test for input validation functions
# Comprehensive tests for all validation and sanitization functions

library(testthat)
library(devtools)
load_all(".")

# ============================================================================
# validate_path() tests
# ============================================================================

test_that("validate_path accepts valid paths", {
  result <- validate_path("/valid/path/to/file.txt")
  expect_true(result$valid)
  expect_equal(result$sanitized, "/valid/path/to/file.txt")

  result <- validate_path("simple.txt")
  expect_true(result$valid)
  expect_equal(result$sanitized, "simple.txt")
})

test_that("validate_path rejects null and invalid types", {
  result <- validate_path(NULL)
  expect_false(result$valid)
  expect_match(result$error, "Invalid path type")

  result <- validate_path(123)
  expect_false(result$valid)
  expect_match(result$error, "Invalid path type")

  result <- validate_path(c("path1", "path2"))
  expect_false(result$valid)
  expect_match(result$error, "Invalid path type")
})

test_that("validate_path rejects paths that are too long", {
  long_path <- paste0(rep("a", 1001), collapse = "")
  result <- validate_path(long_path, max_length = 1000)
  expect_false(result$valid)
  expect_match(result$error, "Path too long")

  # Just under the limit should be OK
  ok_path <- paste0(rep("a", 1000), collapse = "")
  result <- validate_path(ok_path, max_length = 1000)
  expect_true(result$valid)
})

test_that("validate_path detects null bytes", {
  # Note: R strings cannot actually contain embedded null bytes (null-terminated in C),
  # so this is defensive code that may not trigger in practice. We skip this test
  # as it's not possible to create a valid test case in R.
  skip("R strings cannot contain embedded null bytes")
})

test_that("validate_path detects path traversal attacks", {
  result <- validate_path("../../../etc/passwd")
  expect_false(result$valid)
  expect_match(result$error, "Path traversal")

  result <- validate_path("/safe/path/../../etc/passwd")
  expect_false(result$valid)
  expect_match(result$error, "Path traversal")

  result <- validate_path("..\\..\\windows\\system32")
  expect_false(result$valid)
  expect_match(result$error, "Path traversal")

  # ".." by itself or at the end is OK (handled by patterns)
  result <- validate_path("valid/path..")
  expect_true(result$valid)
})

test_that("validate_path rejects dangerous control characters", {
  # SOH (Start of Heading)
  path_soh <- paste0("path", rawToChar(as.raw(1)), "invalid")
  result <- validate_path(path_soh)
  expect_false(result$valid)
  expect_match(result$error, "invalid characters")

  # US (Unit Separator)
  path_us <- paste0("path", rawToChar(as.raw(31)), "invalid")
  result <- validate_path(path_us)
  expect_false(result$valid)
  expect_match(result$error, "invalid characters")

  # DEL
  path_del <- paste0("path", rawToChar(as.raw(127)), "invalid")
  result <- validate_path(path_del)
  expect_false(result$valid)
  expect_match(result$error, "invalid characters")
})

# ============================================================================
# validate_http_method() tests
# ============================================================================

test_that("validate_http_method accepts valid methods", {
  methods <- c("GET", "POST", "PUT", "DELETE", "HEAD", "OPTIONS")
  for (method in methods) {
    result <- validate_http_method(method)
    expect_true(result$valid)
    expect_equal(result$sanitized, method)
  }
})

test_that("validate_http_method converts to uppercase and trims", {
  result <- validate_http_method("  get  ")
  expect_true(result$valid)
  expect_equal(result$sanitized, "GET")

  result <- validate_http_method("post")
  expect_true(result$valid)
  expect_equal(result$sanitized, "POST")
})

test_that("validate_http_method rejects null and invalid types", {
  result <- validate_http_method(NULL)
  expect_false(result$valid)
  expect_match(result$error, "Invalid method type")

  result <- validate_http_method(123)
  expect_false(result$valid)
  expect_match(result$error, "Invalid method type")

  result <- validate_http_method(c("GET", "POST"))
  expect_false(result$valid)
  expect_match(result$error, "Invalid method type")
})

test_that("validate_http_method rejects invalid methods", {
  result <- validate_http_method("INVALID")
  expect_false(result$valid)
  expect_match(result$error, "not allowed")

  result <- validate_http_method("TRACE")
  expect_false(result$valid)
  expect_match(result$error, "not allowed")
})

test_that("validate_http_method respects custom allowed_methods", {
  result <- validate_http_method("POST", allowed_methods = c("GET"))
  expect_false(result$valid)

  result <- validate_http_method("GET", allowed_methods = c("GET"))
  expect_true(result$valid)
})

# ============================================================================
# validate_query_string() tests
# ============================================================================

test_that("validate_query_string accepts valid query strings", {
  result <- validate_query_string("key=value&foo=bar")
  expect_true(result$valid)
  expect_equal(result$sanitized, "key=value&foo=bar")

  result <- validate_query_string("name=John%20Doe&age=25")
  expect_true(result$valid)
})

test_that("validate_query_string handles null as empty string", {
  result <- validate_query_string(NULL)
  expect_true(result$valid)
  expect_equal(result$sanitized, "")
})

test_that("validate_query_string rejects invalid types", {
  result <- validate_query_string(123)
  expect_false(result$valid)
  expect_match(result$error, "Invalid query string type")

  result <- validate_query_string(c("a=1", "b=2"))
  expect_false(result$valid)
  expect_match(result$error, "Invalid query string type")
})

test_that("validate_query_string rejects strings that are too long", {
  long_query <- paste0(rep("a", 2049), collapse = "")
  result <- validate_query_string(long_query, max_length = 2048)
  expect_false(result$valid)
  expect_match(result$error, "too long")
})

test_that("validate_query_string detects null bytes", {
  # Note: R strings cannot contain embedded null bytes
  # This is defensive code that may not trigger in practice
  skip("R strings cannot contain embedded null bytes")
})

test_that("validate_query_string validates URL encoding", {
  # Invalid: % followed by non-hex
  result <- validate_query_string("key=%ZZ")
  expect_false(result$valid)
  expect_match(result$error, "Invalid URL encoding")

  # Invalid: % followed by only one hex digit
  result <- validate_query_string("key=%2G")
  expect_false(result$valid)
  expect_match(result$error, "Invalid URL encoding")

  # Valid: proper encoding
  result <- validate_query_string("key=%20%2F%3D")
  expect_true(result$valid)
})

# ============================================================================
# validate_ws_message() tests
# ============================================================================

test_that("validate_ws_message accepts valid messages", {
  result <- validate_ws_message("Simple message")
  expect_true(result$valid)
  expect_equal(result$sanitized, "Simple message")

  result <- validate_ws_message('{"type": "update", "data": {"key": "value"}}')
  expect_true(result$valid)
})

test_that("validate_ws_message rejects null", {
  result <- validate_ws_message(NULL)
  expect_false(result$valid)
  expect_match(result$error, "null")
})

test_that("validate_ws_message rejects invalid types", {
  result <- validate_ws_message(123)
  expect_false(result$valid)
  expect_match(result$error, "Invalid message type")

  result <- validate_ws_message(c("msg1", "msg2"))
  expect_false(result$valid)
  expect_match(result$error, "Invalid message type")
})

test_that("validate_ws_message rejects messages that are too large", {
  # Create a message larger than 1MB (1048576 bytes)
  large_message <- paste0(rep("x", 1048577), collapse = "")
  result <- validate_ws_message(large_message, max_size = 1048576)
  expect_false(result$valid)
  expect_match(result$error, "too large")
})

test_that("validate_ws_message detects null bytes", {
  # Note: R strings cannot contain embedded null bytes
  # This is defensive code that may not trigger in practice
  skip("R strings cannot contain embedded null bytes")
})

# ============================================================================
# validate_app_name() tests
# ============================================================================

test_that("validate_app_name accepts valid names", {
  valid_names <- c("myapp", "my-app", "my_app", "MyApp123", "app-1_test")
  for (name in valid_names) {
    result <- validate_app_name(name)
    expect_true(result$valid, info = paste("Failed for:", name))
    expect_equal(result$sanitized, name)
  }
})

test_that("validate_app_name rejects null and invalid types", {
  result <- validate_app_name(NULL)
  expect_false(result$valid)
  expect_match(result$error, "Invalid app name type")

  result <- validate_app_name(123)
  expect_false(result$valid)
  expect_match(result$error, "Invalid app name type")

  result <- validate_app_name(c("app1", "app2"))
  expect_false(result$valid)
  expect_match(result$error, "Invalid app name type")
})

test_that("validate_app_name rejects invalid characters", {
  invalid_names <- c("my app", "my.app", "my@app", "my/app", "my\\app", "my!app")
  for (name in invalid_names) {
    result <- validate_app_name(name)
    expect_false(result$valid, info = paste("Should reject:", name))
    expect_match(result$error, "invalid characters")
  }
})

test_that("validate_app_name rejects names that are too long", {
  long_name <- paste0(rep("a", 51), collapse = "")
  result <- validate_app_name(long_name, max_length = 50)
  expect_false(result$valid)
  expect_match(result$error, "too long")

  # At the limit should be OK
  ok_name <- paste0(rep("a", 50), collapse = "")
  result <- validate_app_name(ok_name, max_length = 50)
  expect_true(result$valid)
})

# ============================================================================
# validate_ip_address() tests
# ============================================================================

test_that("validate_ip_address accepts safe IPs", {
  safe_ips <- c("127.0.0.1", "::1", "localhost", "unknown")
  for (ip in safe_ips) {
    result <- validate_ip_address(ip)
    expect_true(result$valid, info = paste("Failed for:", ip))
    expect_equal(result$sanitized, ip)
  }
})

test_that("validate_ip_address accepts valid IPv4 addresses", {
  valid_ipv4 <- c("192.168.1.1", "10.0.0.1", "172.16.0.1", "0.0.0.0", "255.255.255.255")
  for (ip in valid_ipv4) {
    result <- validate_ip_address(ip)
    expect_true(result$valid, info = paste("Failed for:", ip))
    expect_equal(result$sanitized, ip)
  }
})

test_that("validate_ip_address accepts valid IPv6 addresses", {
  valid_ipv6 <- c("::1", "::", "2001:0db8:85a3:0000:0000:8a2e:0370:7334")
  for (ip in valid_ipv6) {
    result <- validate_ip_address(ip)
    expect_true(result$valid, info = paste("Failed for:", ip))
  }
})

test_that("validate_ip_address rejects null and invalid types", {
  result <- validate_ip_address(NULL)
  expect_false(result$valid)
  expect_match(result$error, "Invalid IP type")

  result <- validate_ip_address(123)
  expect_false(result$valid)
  expect_match(result$error, "Invalid IP type")

  result <- validate_ip_address(c("127.0.0.1", "::1"))
  expect_false(result$valid)
  expect_match(result$error, "Invalid IP type")
})

test_that("validate_ip_address rejects invalid formats", {
  invalid_ips <- c("999.999.999.999", "192.168.1", "192.168.1.1.1", "not.an.ip", "")
  for (ip in invalid_ips) {
    result <- validate_ip_address(ip)
    expect_false(result$valid, info = paste("Should reject:", ip))
    expect_match(result$error, "Invalid IP address format")
  }
})

# ============================================================================
# validate_port() tests
# ============================================================================

test_that("validate_port accepts valid port numbers", {
  valid_ports <- c(1, 80, 443, 8080, 3000, 65535)
  for (port in valid_ports) {
    result <- validate_port(port)
    expect_true(result$valid, info = paste("Failed for:", port))
    expect_equal(result$sanitized, as.integer(port))
  }
})

test_that("validate_port rejects null", {
  result <- validate_port(NULL)
  expect_false(result$valid)
  expect_match(result$error, "null")
})

test_that("validate_port rejects non-numeric values", {
  result <- validate_port("8080")
  expect_false(result$valid)
  expect_match(result$error, "must be a number")

  result <- validate_port(c(80, 443))
  expect_false(result$valid)
  expect_match(result$error, "must be a number")
})

test_that("validate_port rejects out-of-range ports", {
  result <- validate_port(0)
  expect_false(result$valid)
  expect_match(result$error, "between 1 and 65535")

  result <- validate_port(-1)
  expect_false(result$valid)
  expect_match(result$error, "between 1 and 65535")

  result <- validate_port(65536)
  expect_false(result$valid)
  expect_match(result$error, "between 1 and 65535")
})

# ============================================================================
# validate_session_id() tests
# ============================================================================

test_that("validate_session_id accepts valid session IDs", {
  # Valid 64-character hex string (simulating SHA-256)
  valid_id <- paste0(rep("a", 64), collapse = "")
  result <- validate_session_id(valid_id)
  expect_true(result$valid)
  expect_equal(result$sanitized, valid_id)

  # Mixed case hex
  mixed_id <- paste0(c(rep("A", 32), rep("f", 32)), collapse = "")
  result <- validate_session_id(mixed_id)
  expect_true(result$valid)
})

test_that("validate_session_id rejects null and invalid types", {
  result <- validate_session_id(NULL)
  expect_false(result$valid)
  expect_match(result$error, "Invalid session ID type")

  result <- validate_session_id(123)
  expect_false(result$valid)
  expect_match(result$error, "Invalid session ID type")

  result <- validate_session_id(c("abc", "def"))
  expect_false(result$valid)
  expect_match(result$error, "Invalid session ID type")
})

test_that("validate_session_id rejects invalid formats", {
  # Wrong length
  result <- validate_session_id(paste0(rep("a", 63), collapse = ""))
  expect_false(result$valid)
  expect_match(result$error, "Invalid session ID format")

  result <- validate_session_id(paste0(rep("a", 65), collapse = ""))
  expect_false(result$valid)
  expect_match(result$error, "Invalid session ID format")

  # Non-hex characters
  result <- validate_session_id(paste0(c(rep("a", 63), "z"), collapse = ""))
  expect_false(result$valid)
  expect_match(result$error, "Invalid session ID format")
})

# ============================================================================
# validate_json_input() tests
# ============================================================================

test_that("validate_json_input accepts valid JSON", {
  result <- validate_json_input('{"key": "value"}')
  expect_true(result$valid)
  expect_equal(result$sanitized, '{"key": "value"}')
  expect_equal(result$parsed$key, "value")

  result <- validate_json_input('["item1", "item2"]')
  expect_true(result$valid)
  expect_equal(length(result$parsed), 2)
})

test_that("validate_json_input rejects null and invalid types", {
  result <- validate_json_input(NULL)
  expect_false(result$valid)
  expect_match(result$error, "Invalid JSON input type")

  result <- validate_json_input(123)
  expect_false(result$valid)
  expect_match(result$error, "Invalid JSON input type")

  result <- validate_json_input(c('{"a":1}', '{"b":2}'))
  expect_false(result$valid)
  expect_match(result$error, "Invalid JSON input type")
})

test_that("validate_json_input rejects invalid JSON", {
  result <- validate_json_input('{"key": invalid}')
  expect_false(result$valid)
  expect_match(result$error, "Invalid JSON")

  result <- validate_json_input('not json at all')
  expect_false(result$valid)
  expect_match(result$error, "Invalid JSON")

  result <- validate_json_input('{"unclosed": ')
  expect_false(result$valid)
  expect_match(result$error, "Invalid JSON")
})

# ============================================================================
# html_escape() tests
# ============================================================================

test_that("html_escape escapes dangerous characters", {
  result <- html_escape("<script>alert('xss')</script>")
  expect_equal(result, "&lt;script&gt;alert(&#39;xss&#39;)&lt;/script&gt;")

  result <- html_escape('Test & "quotes" and <tags>')
  expect_equal(result, "Test &amp; &quot;quotes&quot; and &lt;tags&gt;")

  result <- html_escape("She said 'hello' & waved")
  expect_equal(result, "She said &#39;hello&#39; &amp; waved")
})

test_that("html_escape handles null and non-character input", {
  result <- html_escape(NULL)
  expect_equal(result, "")

  result <- html_escape(123)
  expect_equal(result, "")

  result <- html_escape(list(a = 1))
  expect_equal(result, "")
})

test_that("html_escape preserves safe characters", {
  result <- html_escape("safe text 123")
  expect_equal(result, "safe text 123")
})

# ============================================================================
# sanitize_for_logging() tests
# ============================================================================

test_that("sanitize_for_logging truncates long text", {
  long_text <- paste0(rep("a", 150), collapse = "")
  result <- sanitize_for_logging(long_text, max_length = 100)
  expect_equal(nchar(result), 103)  # 100 chars + "..."
  expect_match(result, "\\.\\.\\.$")
})

test_that("sanitize_for_logging removes control characters", {
  # Create string with control characters dynamically
  text_with_control <- paste0("text", rawToChar(as.raw(1)), "with", rawToChar(as.raw(31)), "control", rawToChar(as.raw(127)), "chars")
  result <- sanitize_for_logging(text_with_control)
  expect_equal(result, "text?with?control?chars")

  result <- sanitize_for_logging("normal\nnewline\ttab")
  expect_equal(result, "normal?newline?tab")
})

test_that("sanitize_for_logging handles null and non-character input", {
  result <- sanitize_for_logging(NULL)
  expect_equal(result, "(null)")

  result <- sanitize_for_logging(123)
  expect_equal(result, "(null)")

  result <- sanitize_for_logging(list(a = 1))
  expect_equal(result, "(null)")
})

test_that("sanitize_for_logging preserves safe short text", {
  result <- sanitize_for_logging("safe text", max_length = 100)
  expect_equal(result, "safe text")
})

# ============================================================================
# create_validation_error() tests
# ============================================================================

test_that("create_validation_error creates proper error structure", {
  result <- create_validation_error("Test error")

  expect_false(result$valid)
  expect_equal(result$error, "Test error")
  expect_equal(result$status_code, 400)
  expect_equal(result$response$status, 400)
  expect_equal(result$response$headers$`Content-Type`, "application/json")
  expect_match(result$response$body, "Test error")
})

test_that("create_validation_error respects custom status codes", {
  result <- create_validation_error("Forbidden", status_code = 403)

  expect_equal(result$status_code, 403)
  expect_equal(result$response$status, 403)
  expect_match(result$response$body, "403")
})

# ============================================================================
# validate_request_inputs() tests
# ============================================================================

test_that("validate_request_inputs validates all inputs in batch", {
  result <- validate_request_inputs("/valid/path", "GET", "key=value")

  expect_true(result$valid)
  expect_equal(result$path, "/valid/path")
  expect_equal(result$method, "GET")
  expect_equal(result$query_string, "key=value")
})

test_that("validate_request_inputs works without query string", {
  result <- validate_request_inputs("/valid/path", "POST", NULL)

  expect_true(result$valid)
  expect_equal(result$path, "/valid/path")
  expect_equal(result$method, "POST")
  expect_null(result$query_string)
})

test_that("validate_request_inputs returns error for invalid path", {
  result <- validate_request_inputs("../../../etc/passwd", "GET")

  expect_false(result$valid)
  expect_match(result$error, "Invalid path")
  expect_equal(result$status_code, 400)
  expect_true("response" %in% names(result))
})

test_that("validate_request_inputs returns error for invalid method", {
  result <- validate_request_inputs("/valid/path", "INVALID")

  expect_false(result$valid)
  expect_match(result$error, "Invalid method")
  expect_equal(result$status_code, 405)
})

test_that("validate_request_inputs returns error for invalid query string", {
  result <- validate_request_inputs("/valid/path", "GET", "key=%ZZ")

  expect_false(result$valid)
  expect_match(result$error, "Invalid query")
  expect_equal(result$status_code, 400)
})

test_that("validate_request_inputs normalizes method case", {
  result <- validate_request_inputs("/path", "  post  ")

  expect_true(result$valid)
  expect_equal(result$method, "POST")
})

cat("Validation function tests completed successfully!\n")
