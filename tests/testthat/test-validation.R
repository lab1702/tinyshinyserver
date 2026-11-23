# Test for input validation functions
# Comprehensive tests for all validation and sanitization functions

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

  # Two dots at the end of a name are not a parent-directory reference
  result <- validate_path("valid/path..")
  expect_true(result$valid)
})

test_that("validate_path treats only whole '..' segments as traversal", {
  for (path in c("/proxy/app/..hidden/x.css", "/proxy/app/notes../file.txt",
    "/proxy/app/v1...latest/x", "/proxy/app/a..b")) {
    expect_true(validate_path(path)$valid, info = path)
  }
  for (path in c("..", "../x", "/proxy/app/..", "/proxy/app/../x", "a\\..\\b", "a/..\\b")) {
    result <- validate_path(path)
    expect_false(result$valid, info = path)
    expect_match(result$error, "Path traversal", info = path)
  }
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
  methods <- c("GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS")
  for (method in methods) {
    result <- validate_http_method(method)
    expect_true(result$valid)
    expect_equal(result$sanitized, method)
  }
})

test_that("validate_http_method matches method names exactly", {
  # Method names are case-sensitive: "get" is a different method from GET
  for (method in c("get", "Post", "  GET  ", "GET ")) {
    result <- validate_http_method(method)
    expect_false(result$valid, info = method)
    expect_match(result$error, "not supported", info = method)
  }
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
  expect_match(result$error, "not supported")

  result <- validate_http_method("TRACE")
  expect_false(result$valid)
  expect_match(result$error, "not supported")
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

test_that("validate_query_string accepts long Shiny bookmark URLs by default", {
  expect_true(validate_query_string(paste0("_inputs_&x=", strrep("a", 8000)))$valid)
  expect_false(validate_query_string(strrep("a", 8193))$valid)
  # httpuv's QUERY_STRING includes the leading question mark.
  expect_true(validate_query_string(paste0("?", strrep("a", 8192)))$valid)
  expect_false(validate_query_string(paste0("?", strrep("a", 8193)))$valid)
})

test_that("validate_query_string leaves percent-encoding to the app", {
  # The query is forwarded verbatim, so a literal "%" must reach the app
  for (query in c("?discount=50%&x=1", "q=100%", "key=%ZZ", "key=%2G", "key=%20%2F%3D")) {
    result <- validate_query_string(query)
    expect_true(result$valid, info = query)
    expect_identical(result$sanitized, query, info = query)
  }
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
  expect_match(result$error, "App name must be a string")

  result <- validate_app_name(123)
  expect_false(result$valid)
  expect_match(result$error, "App name must be a string")

  result <- validate_app_name(c("app1", "app2"))
  expect_false(result$valid)
  expect_match(result$error, "App name must be a string")

  result <- validate_app_name(NA_character_)
  expect_false(result$valid)
  expect_match(result$error, "App name must be a string")
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
  # A method that no resource serves is not implemented, so the answer is 501,
  # not a 405 whose Allow header would have to list a route's methods
  expect_equal(result$status_code, 501)
  expect_equal(result$response$status, 501)
  expect_null(result$response$headers[["Allow"]])
})

# ============================================================================
# validate_ws_message() tests
# ============================================================================

test_that("validate_ws_message accepts valid messages", {
  result <- validate_ws_message("Hello, World!")
  expect_true(result$valid)
  expect_equal(result$sanitized, "Hello, World!")

  result <- validate_ws_message('{"type":"message","data":"test"}')
  expect_true(result$valid)
})

test_that("validate_ws_message bounds binary messages by byte length", {
  payload <- as.raw(c(0, 1, 128, 255))
  expect_identical(validate_ws_message(payload, 4)$sanitized, payload)
  expect_true(validate_ws_message(raw(), 0)$valid)
  expect_false(validate_ws_message(payload, 3)$valid)
  expect_equal(validate_ws_message(payload, 3)$error, "Message too large")
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

  result <- validate_ws_message(list(a = 1))
  expect_false(result$valid)
  expect_match(result$error, "Invalid message type")
})

test_that("validate_ws_message rejects messages that are too large", {
  # Default max is 1MB (1048576 bytes)
  large_message <- paste0(rep("a", 1048577), collapse = "")
  result <- validate_ws_message(large_message)
  expect_false(result$valid)
  expect_match(result$error, "too large")

  # Just under the limit should be OK
  ok_message <- paste0(rep("a", 1048576), collapse = "")
  result <- validate_ws_message(ok_message)
  expect_true(result$valid)

  # Custom max size
  result <- validate_ws_message("12345", max_size = 4)
  expect_false(result$valid)
  expect_match(result$error, "too large")
})

test_that("percent-encoded bytes are left for the app to interpret", {
  # An R string cannot hold a zero byte, and request paths and query strings
  # are forwarded without decoding, so an encoded one passes through
  expect_true(validate_path("/proxy/app/a%00b")$valid)
  expect_true(validate_query_string("?a=%00")$valid)
  expect_true(validate_ws_message("a%00b")$valid)
})

test_that("validate_ws_message refuses text that is not valid UTF-8", {
  result <- validate_ws_message("caf\xe9")
  expect_false(result$valid)
  expect_match(result$error, "UTF-8")
  expect_true(validate_ws_message(intToUtf8(c(99, 97, 102, 233)))$valid)
  # Binary messages carry any bytes
  expect_true(validate_ws_message(as.raw(c(0xe9, 0xff)))$valid)
})
