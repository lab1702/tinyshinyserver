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

# ============================================================================
# handle_http_request() tests
# ============================================================================

test_that("handle_http_request validates request inputs", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  # Invalid method
  req <- list(
    PATH_INFO = "/",
    REQUEST_METHOD = "INVALID_METHOD",
    QUERY_STRING = ""
  )

  result <- handle_http_request(req, config, template_manager, connection_manager)

  expect_equal(result$status, 405)
})

test_that("handle_http_request routes valid requests", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  req <- list(
    PATH_INFO = "/health",
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_http_request(req, config, template_manager, connection_manager)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)
  expect_equal(body$status, "healthy")
})

test_that("handle_http_request rejects overly long paths", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  long_path <- paste0("/", paste(rep("a", 1001), collapse = ""))

  req <- list(
    PATH_INFO = long_path,
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_http_request(req, config, template_manager, connection_manager)

  expect_equal(result$status, 400)
})

test_that("handle_http_request attaches managers to request", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path", port = 3001, resident = TRUE)
    )
  )
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  mock_pm <- list(
    get_all_app_status = function() {
      list(app1 = list(name = "app1", status = "running"))
    }
  )

  req <- list(
    PATH_INFO = "/api/apps",
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_http_request(req, config, template_manager, connection_manager, mock_pm)

  expect_equal(result$status, 200)
})

# ============================================================================
# handle_static_file() tests
# ============================================================================

test_that("handle_static_file serves files via template manager", {
  template_manager <- create_mock_template_manager()

  result <- handle_static_file("/templates/style.css", template_manager)

  expect_equal(result$status, 200)
  expect_equal(result$headers[["Content-Type"]], "text/css")
})

test_that("handle_static_file returns 404 for missing files", {
  template_manager <- create_mock_template_manager()

  result <- handle_static_file("/templates/nonexistent.css", template_manager)

  expect_equal(result$status, 404)
})

test_that("handle_static_file strips /templates/ prefix", {
  # Track what file_path is passed to serve_static_file
  received_path <- NULL
  template_manager <- list(
    serve_static_file = function(file_path) {
      received_path <<- file_path
      list(status = 200, headers = list("Content-Type" = "text/css"), body = "test")
    }
  )

  handle_static_file("/templates/styles/main.css", template_manager)

  expect_equal(received_path, "styles/main.css")
})

# ============================================================================
# handle_proxy_request() tests
# ============================================================================

test_that("handle_proxy_request rejects invalid app names", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  req <- list()

  # Path traversal attempt
  result <- handle_proxy_request("/proxy/../../../etc/passwd", "GET", NULL, req, config)

  expect_equal(result$status, 400)
  expect_match(result$body, "Invalid app name")
})

test_that("handle_proxy_request returns 404 for unknown apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  req <- list()

  result <- handle_proxy_request("/proxy/nonexistent/path", "GET", NULL, req, config)

  expect_equal(result$status, 404)
  expect_match(result$body, "App not found")
})

test_that("handle_proxy_request returns 400 for invalid path", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  req <- list()

  result <- handle_proxy_request("/proxy/", "GET", NULL, req, config)

  expect_equal(result$status, 400)
  expect_match(result$body, "Invalid proxy path")
})

test_that("handle_proxy_request starts non-resident app on demand", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "myapp", path = "/path", port = 3001, resident = FALSE)
    )
  )

  start_called <- FALSE
  mock_pm <- list(
    start_app_on_demand = function(app_name) {
      start_called <<- TRUE
      TRUE
    }
  )

  req <- list()

  # This will try to start the app but fail when proxying
  result <- handle_proxy_request("/proxy/myapp/", "GET", NULL, req, config, mock_pm)

  # App start should have been called
  expect_true(start_called)
})

test_that("handle_proxy_request returns 502 when app fails to start", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "myapp", path = "/path", port = 3001, resident = FALSE)
    )
  )

  mock_pm <- list(
    start_app_on_demand = function(app_name) FALSE
  )

  req <- list()

  result <- handle_proxy_request("/proxy/myapp/", "GET", NULL, req, config, mock_pm)

  expect_equal(result$status, 502)
  expect_match(result$body, "Failed to start app")
})

# ============================================================================
# forward_request() tests
# ============================================================================

test_that("forward_request returns 503 for starting apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  # Mark app as starting
  config$set_app_starting("testapp")

  req <- list()

  # Mock is_port_in_use to return FALSE (app not ready)
  local_mocked_bindings(
    is_port_in_use = function(host, port) FALSE
  )

  result <- forward_request("GET", "http://127.0.0.1:3001/", req, "testapp", config)

  expect_equal(result$status, 503)
  expect_match(result$body, "starting up")
  expect_true("Retry-After" %in% names(result$headers))
})

test_that("forward_request returns 502 for timed out startups", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  # Manually create a timed out startup state
  assign("testapp", list(
    state = "starting",
    started_at = Sys.time() - (config$APP_STARTUP_TIMEOUT_SECONDS + 5)
  ), envir = config$app_startup_state)

  req <- list()

  result <- forward_request("GET", "http://127.0.0.1:3001/", req, "testapp", config)

  expect_equal(result$status, 502)
  expect_match(result$body, "timed out")
})

test_that("forward_request returns 503 when port not in use", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  req <- list()

  local_mocked_bindings(
    is_port_in_use = function(host, port) FALSE
  )

  result <- forward_request("GET", "http://127.0.0.1:3001/", req, "testapp", config)

  expect_equal(result$status, 503)
  expect_match(result$body, "not ready")
})

# ============================================================================
# extract_app_name_from_ws_path() additional tests
# ============================================================================

test_that("extract_app_name_from_ws_path validates app name format", {
  config <- ShinyServerConfig$new()

  # Valid app names
  expect_equal(extract_app_name_from_ws_path("/proxy/valid-app/ws", config), "valid-app")
  expect_equal(extract_app_name_from_ws_path("/proxy/valid_app/ws", config), "valid_app")
  expect_equal(extract_app_name_from_ws_path("/proxy/ValidApp123/ws", config), "ValidApp123")

  # Invalid app names - should return NULL
  expect_null(extract_app_name_from_ws_path("/proxy/invalid app/ws", config))
  expect_null(extract_app_name_from_ws_path("/proxy/invalid<script>/ws", config))
})

test_that("extract_app_name_from_ws_path handles various path formats", {
  config <- ShinyServerConfig$new()

  # With trailing components
  expect_equal(extract_app_name_from_ws_path("/proxy/myapp/websocket/", config), "myapp")
  expect_equal(extract_app_name_from_ws_path("/proxy/myapp/__sockjs__/", config), "myapp")

  # Minimal path
  expect_equal(extract_app_name_from_ws_path("/proxy/myapp", config), "myapp")
})

# ============================================================================
# Integration tests for route_http_request with proxy
# ============================================================================

test_that("route_http_request routes proxy requests correctly", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "testapp", path = "/path", port = 3001, resident = TRUE)
    )
  )
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  req <- list(process_manager = NULL, connection_manager = connection_manager)

  # Without a running app, this should return 503
  local_mocked_bindings(
    is_port_in_use = function(host, port) FALSE
  )

  result <- route_http_request("/proxy/testapp/", "GET", NULL, req, config, template_manager, connection_manager)

  expect_equal(result$status, 503)
})
