# Test for HTTP routing and handler functions
# Tests routing logic and basic handler functions

# ============================================================================
# handle_health_check() tests
# ============================================================================

test_that("handle_health_check returns healthy status", {
  result <- handle_health_check()

  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  # Parse JSON body
  body <- jsonlite::fromJSON(result$body)
  expect_equal(body$status, "healthy")
})

# ============================================================================
# extract_app_name_from_ws_path() tests
# ============================================================================

test_that("extract_app_name_from_ws_path extracts app name from valid paths", {
  config <- ShinyServerConfig$new()

  # Valid proxy path
  result <- extract_app_name_from_ws_path("/proxy/myapp/websocket", config)
  expect_equal(result, "myapp")

  result <- extract_app_name_from_ws_path("/proxy/my-app/some/path", config)
  expect_equal(result, "my-app")

  result <- extract_app_name_from_ws_path("/proxy/my_app_123/ws", config)
  expect_equal(result, "my_app_123")
})

test_that("extract_app_name_from_ws_path returns NULL for invalid paths", {
  config <- ShinyServerConfig$new()

  # Not a proxy path
  result <- extract_app_name_from_ws_path("/health", config)
  expect_null(result)

  # Proxy path but no app name
  result <- extract_app_name_from_ws_path("/proxy/", config)
  expect_null(result)

  # Proxy path with invalid app name (contains spaces)
  result <- extract_app_name_from_ws_path("/proxy/my app/ws", config)
  expect_null(result)

  # Proxy path with invalid app name (special chars)
  result <- extract_app_name_from_ws_path("/proxy/my@app/ws", config)
  expect_null(result)
})

test_that("extract_app_name_from_ws_path handles edge cases", {
  config <- ShinyServerConfig$new()

  # Empty path
  result <- extract_app_name_from_ws_path("", config)
  expect_null(result)

  # Root path
  result <- extract_app_name_from_ws_path("/", config)
  expect_null(result)

  # Path with multiple slashes
  result <- extract_app_name_from_ws_path("/proxy//myapp", config)
  # Double slashes are filtered out, so "myapp" should be extracted
  expect_equal(result, "myapp")
})

# ============================================================================
# route_http_request() tests
# ============================================================================

# Mock template manager for testing
create_mock_template_manager <- function() {
  list(
    generate_landing_page = function(config) {
      return("<html><body>Landing Page</body></html>")
    },
    serve_static_file = function(file_path) {
      if (file_path == "style.css") {
        return(list(
          status = 200,
          headers = list("Content-Type" = "text/css"),
          body = "body { margin: 0; }"
        ))
      }
      return(list(
        status = 404,
        headers = list("Content-Type" = "text/plain"),
        body = "Not Found"
      ))
    }
  )
}

# Mock connection manager for testing
create_mock_connection_manager <- function() {
  list()
}

test_that("route_http_request routes to landing page", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  # Test root path
  result <- route_http_request("/", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "text/html")
  expect_match(result$body, "Landing Page")

  # Test empty path
  result <- route_http_request("", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_match(result$body, "Landing Page")
})

test_that("route_http_request routes to health check", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  result <- route_http_request("/health", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  body <- jsonlite::fromJSON(result$body)
  expect_equal(body$status, "healthy")
})

test_that("route_http_request routes to apps API", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
    )
  )
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  # Mock request with process_manager
  req <- list(process_manager = NULL)

  result <- route_http_request("/api/apps", "GET", NULL, req, config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  body <- jsonlite::fromJSON(result$body)
  expect_true("app1" %in% names(body))
})

test_that("route_http_request routes to static files", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  result <- route_http_request("/templates/style.css", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "text/css")
  expect_match(result$body, "margin")
})

test_that("route_http_request returns 404 for unknown paths", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  result <- route_http_request("/unknown/path", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 404)
  expect_match(result$body, "Not Found")
})

# ============================================================================
# handle_landing_page() tests
# ============================================================================

test_that("handle_landing_page returns HTML response", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()

  result <- handle_landing_page(config, template_manager)

  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "text/html")
  expect_match(result$body, "<html>")
  expect_match(result$body, "Landing Page")
})

test_that("handle_landing_page handles template errors", {
  config <- ShinyServerConfig$new()

  # Mock template manager that throws error
  bad_template_manager <- list(
    generate_landing_page = function(config) {
      stop("Template error")
    }
  )

  result <- handle_landing_page(config, bad_template_manager)

  expect_equal(result$status, 500)
  expect_match(result$body, "Internal Server Error")
})

# ============================================================================
# handle_apps_api() tests
# ============================================================================

test_that("handle_apps_api returns app status without process manager", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE),
      list(name = "app2", path = "/path/to/app2", port = 3002, resident = FALSE)
    )
  )

  result <- handle_apps_api(config, process_manager = NULL)

  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  body <- jsonlite::fromJSON(result$body)

  # Check app1
  expect_true("app1" %in% names(body))
  expect_equal(body$app1$name, "app1")
  expect_equal(body$app1$status, "stopped")
  expect_true(body$app1$resident)
  expect_equal(body$app1$port, 3001)
  expect_equal(body$app1$connections, 0)

  # Check app2
  expect_true("app2" %in% names(body))
  expect_equal(body$app2$name, "app2")
  expect_equal(body$app2$status, "dormant")
  expect_false(body$app2$resident)
  expect_equal(body$app2$port, 3002)
})

test_that("handle_apps_api sorts apps alphabetically", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "zebra", path = "/z", port = 3003, resident = FALSE),
      list(name = "apple", path = "/a", port = 3001, resident = FALSE),
      list(name = "mango", path = "/m", port = 3002, resident = FALSE)
    )
  )

  result <- handle_apps_api(config, process_manager = NULL)
  body <- jsonlite::fromJSON(result$body, simplifyVector = FALSE)

  # Get app names in order they appear in response
  app_names <- names(body)

  # Should be in alphabetical order
  expect_equal(app_names, c("apple", "mango", "zebra"))
})

test_that("handle_apps_api uses process manager when available", {
  config <- ShinyServerConfig$new()

  # Mock process manager
  mock_pm <- list(
    get_all_app_status = function() {
      return(list(
        app1 = list(
          name = "app1",
          status = "running",
          resident = TRUE,
          port = 3001,
          connections = 5,
          pid = 12345
        )
      ))
    }
  )

  result <- handle_apps_api(config, process_manager = mock_pm)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)

  expect_equal(body$app1$name, "app1")
  expect_equal(body$app1$status, "running")
  expect_equal(body$app1$connections, 5)
  expect_equal(body$app1$pid, 12345)
})
