# Test for management API functions
# Tests for management interface and API endpoints

# ============================================================================
# route_management_request() Tests
# ============================================================================

test_that("route_management_request routes GET / to dashboard", {
  config <- ShinyServerConfig$new()
  pm <- ProcessManager$new(config)

  # Create mock template manager
  mock_tm <- list(
    generate_management_page = function() "<html>Dashboard</html>"
  )

  result <- route_management_request("/", "GET", list(), config, pm, mock_tm)

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "text/html")
  expect_match(result$body, "Dashboard")
})

test_that("route_management_request routes GET /api/apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  result <- route_management_request("/api/apps", "GET", list(), config, pm, list())

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "application/json")
})

test_that("route_management_request routes GET /api/connections", {
  config <- ShinyServerConfig$new()

  result <- route_management_request("/api/connections", "GET", list(), config, list(), list())

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "application/json")
})

test_that("route_management_request routes GET /api/status", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  result <- route_management_request("/api/status", "GET", list(), config, list(), list())

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "application/json")
})

test_that("route_management_request routes POST /api/apps/{name}/restart", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "myapp", path = "/path", port = 3001, resident = TRUE)
    )
  )
  restarted <- NULL
  pm <- list(
    get_app_status = function(name) list(status = "running"),
    restart_app = function(name) {
      restarted <<- name
      list(success = TRUE, message = "Restart scheduled")
    }
  )

  result <- route_management_request("/api/apps/myapp/restart", "POST", list(HTTP_X_TINYSHINYSERVER_REQUEST = "management"), config, pm, list())

  expect_identical(restarted, "myapp")
  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "application/json")
})

test_that("route_management_request routes POST /api/shutdown", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempdir())

  result <- route_management_request("/api/shutdown", "POST", list(HTTP_X_TINYSHINYSERVER_REQUEST = "management"), config, list(), list())

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "application/json")
  expect_true(config$shutdown_requested)
})

test_that("route_management_request routes /templates/ to static files", {
  config <- ShinyServerConfig$new()

  mock_tm <- list(
    serve_static_file = function(path) {
      create_html_response(paste("Static:", path))
    }
  )

  result <- route_management_request("/templates/style.css", "GET", list(), config, list(), mock_tm)

  expect_equal(result$status, 200)
  expect_match(result$body, "Static: style.css")
})

test_that("route_management_request returns 404 for unknown paths", {
  config <- ShinyServerConfig$new()

  result <- route_management_request("/unknown/path", "GET", list(), config, list(), list())

  expect_equal(result$status, 404)
})

# ============================================================================
# handle_management_request() Tests
# ============================================================================

test_that("handle_management_request validates request inputs", {
  config <- ShinyServerConfig$new()
  pm <- ProcessManager$new(config)

  # Test with invalid method - returns 405 Method Not Allowed
  req <- list(
    PATH_INFO = "/",
    REQUEST_METHOD = "INVALID",
    QUERY_STRING = ""
  )

  result <- handle_management_request(req, config, pm, list())

  expect_equal(result$status, 405)
})

test_that("handle_management_request handles valid requests", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  mock_tm <- list(
    generate_management_page = function() "<html>Test</html>"
  )

  req <- list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_management_request(req, config, pm, mock_tm)

  expect_equal(result$status, 200)
})

test_that("handle_management_request handles missing PATH_INFO", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  mock_tm <- list(
    generate_management_page = function() "<html>Test</html>"
  )

  req <- list(
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  # Should default to "/" path
  result <- handle_management_request(req, config, pm, mock_tm)

  expect_equal(result$status, 200)
})

# ============================================================================
# handle_management_dashboard() Tests
# ============================================================================

test_that("handle_management_dashboard returns HTML response", {
  mock_tm <- list(
    generate_management_page = function() "<html><body>Management Dashboard</body></html>"
  )

  result <- handle_management_dashboard(mock_tm)

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "text/html")
  expect_match(result$body, "Management Dashboard")
})

test_that("handle_management_dashboard handles errors", {
  mock_tm <- list(
    generate_management_page = function() {
      stop("Template error")
    }
  )

  result <- handle_management_dashboard(mock_tm)

  expect_equal(result$status, 500)
})

# ============================================================================
# handle_management_apps_api() Tests
# ============================================================================

test_that("handle_management_apps_api returns app status", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1", port = 3001, resident = TRUE),
      list(name = "app2", path = "/path2", port = 3002, resident = FALSE)
    )
  )
  pm <- ProcessManager$new(config)

  result <- handle_management_apps_api(pm)

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "application/json")

  body <- jsonlite::fromJSON(result$body)
  expect_true("app1" %in% names(body))
  expect_true("app2" %in% names(body))
})

test_that("handle_management_apps_api returns empty object for no apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  result <- handle_management_apps_api(pm)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)
  expect_equal(length(body), 0)
})

test_that("handle_management_apps_api handles errors", {
  # Create a mock process manager that throws an error
  mock_pm <- list(
    get_all_app_status = function() {
      stop("Database connection error")
    }
  )

  result <- handle_management_apps_api(mock_pm)

  expect_equal(result$status, 500)
})

# ============================================================================
# handle_management_connections_api() Tests
# ============================================================================

test_that("handle_management_connections_api returns connection info", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("sess1", list(
    app_name = "app1",
    client_ip = "192.168.1.100",
    user_agent = "TestBrowser/1.0",
    created_at = Sys.time() - 3600, # 1 hour ago
    last_activity = Sys.time() - 60 # 1 minute ago
  ))

  result <- handle_management_connections_api(config)

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "application/json")

  body <- jsonlite::fromJSON(result$body)
  expect_true("sess1" %in% names(body))
  expect_equal(body$sess1$app_name, "app1")
  expect_equal(body$sess1$client_ip, "192.168.1.100")
})

test_that("handle_management_connections_api handles missing fields", {
  config <- ShinyServerConfig$new()

  # Connection with minimal info
  config$add_ws_connection("sess1", list(
    app_name = "app1"
  ))

  result <- handle_management_connections_api(config)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)
  expect_equal(body$sess1$client_ip, "unknown")
  expect_equal(body$sess1$user_agent, "unknown")
})

test_that("handle_management_connections_api returns empty for no connections", {
  config <- ShinyServerConfig$new()

  result <- handle_management_connections_api(config)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)
  expect_equal(length(body), 0)
})

# ============================================================================
# handle_management_status_api() Tests
# ============================================================================

test_that("handle_management_status_api returns system status", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1", port = 3001),
      list(name = "app2", path = "/path2", port = 3002),
      list(name = "app3", path = "/path3", port = 3003)
    )
  )

  # Add some connections
  config$add_ws_connection("s1", list(app_name = "app1"))
  config$add_ws_connection("s2", list(app_name = "app1"))

  result <- handle_management_status_api(config)

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "application/json")

  body <- jsonlite::fromJSON(result$body)
  expect_equal(body$total_apps, 3)
  expect_equal(body$running_apps, 0) # No processes running
  expect_equal(body$total_connections, 2)
})

test_that("handle_management_status_api counts running apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1", port = 3001)
    )
  )

  # Add a mock running process
  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  local_mocked_bindings(
    is_process_alive = function(process) TRUE
  )

  result <- handle_management_status_api(config)

  body <- jsonlite::fromJSON(result$body)
  expect_equal(body$running_apps, 1)
})

test_that("handle_management_status_api reports uptime and memory", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  config$started_at <- Sys.time() - 90

  body <- jsonlite::fromJSON(handle_management_status_api(config)$body)

  expect_gte(body$uptime_seconds, 90)
  expect_null(body$server_uptime)
  expect_null(body$memory_usage)
  skip_if_not(ps::ps_is_supported())
  expect_gt(body$server_memory_bytes, 0)
  expect_equal(body$apps_memory_bytes, 0)
})

test_that("handle_management_status_api omits memory ps cannot read", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  local_mocked_bindings(process_memory_bytes = function(process = NULL) NULL)

  body <- jsonlite::fromJSON(handle_management_status_api(config)$body)

  expect_false(any(c("server_memory_bytes", "apps_memory_bytes") %in% names(body)))
})

# ============================================================================
# App resource usage
# ============================================================================

test_that("process_usage is empty for missing or exited processes", {
  expect_equal(process_usage(NULL), list())
  expect_equal(process_usage(list(is_alive = function() FALSE)), list())
})

test_that("app memory and uptime reach the management API but not the public one", {
  skip_on_cran()
  skip_if_not(ps::ps_is_supported())
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(
    list(name = "app1", path = "/path1", port = 3001, resident = TRUE)
  ))
  process <- callr::r_bg(function() Sys.sleep(30))
  on.exit(process$kill(), add = TRUE)
  config$add_app_process("app1", process)
  pm <- ProcessManager$new(config)

  managed <- jsonlite::fromJSON(handle_management_apps_api(pm)$body)$app1
  expect_gt(managed$memory_bytes, 0)
  expect_gte(managed$uptime_seconds, 0)

  status <- jsonlite::fromJSON(handle_management_status_api(config)$body)
  expect_gt(status$apps_memory_bytes, 0)

  public <- jsonlite::fromJSON(handle_apps_api(config, pm)$body)$app1
  expect_false(any(c("memory_bytes", "uptime_seconds") %in% names(public)))
})

# ============================================================================
# handle_app_restart() Tests
# ============================================================================

test_that("handle_app_restart validates app name", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  # Invalid app name with special characters
  result <- handle_app_restart("/api/apps/../etc/passwd/restart", pm)

  expect_equal(result$status, 400)
  expect_match(result$body, "Invalid app name")
})

test_that("handle_app_restart handles short path gracefully", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  # Path with missing app name segment
  result <- handle_app_restart("/api/apps/restart", pm)

  # The function will try to extract app name from path_parts[3] which is "restart"
  # Then it validates it and returns 404 since "restart" isn't a configured app
  expect_equal(result$status, 404)
})

test_that("handle_app_restart returns 404 for an unknown app", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app1", path = "/path", port = 3001, resident = TRUE)))
  pm <- ProcessManager$new(config)

  result <- handle_app_restart("/api/apps/typo/restart", pm)

  expect_equal(result$status, 404)
  body <- jsonlite::fromJSON(result$body)
  expect_false(body$success)
  expect_match(body$message, "not found")
})

test_that("handle_app_restart does not restart dormant apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path", port = 3001, resident = FALSE)
    )
  )
  pm <- ProcessManager$new(config)

  result <- handle_app_restart("/api/apps/app1/restart", pm)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)
  expect_false(body$success)
  expect_match(body$message, "dormant")
})

test_that("handle_app_restart restarts running app", {
  temp_app_dir <- file.path(tempdir(), "restart_api_test")
  dir.create(temp_app_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines("# placeholder", file.path(temp_app_dir, "app.R"))

  config <- ShinyServerConfig$new()
  on.exit(stop_test_app_processes(config), add = TRUE)
  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = TRUE)
    ),
    log_dir = tempdir(),
    restart_delay = 0
  )

  # Add a running process
  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    kill_process_safely = function(process) TRUE
  )

  result <- handle_app_restart("/api/apps/app1/restart", pm)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)
  expect_true(body$success)

  # Cleanup
  unlink(temp_app_dir, recursive = TRUE)
})

test_that("handle_app_restart handles restart failure", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/nonexistent", port = 3001, resident = TRUE)
    ),
    log_dir = tempdir(),
    restart_delay = 0
  )

  # Add a running process
  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    kill_process_safely = function(process) {
      stop("Kill failed")
    }
  )

  result <- handle_app_restart("/api/apps/app1/restart", pm)

  # Should return 500 for restart failure
  expect_equal(result$status, 500)
})

test_that("handle_app_restart returns failure for unknown app", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  result <- handle_app_restart("/api/apps/nonexistent/restart", pm)

  # get_app_status returns NULL for unknown apps, restart_app returns failure
  # The response may be 200 with success=false or 500 depending on implementation
  body <- jsonlite::fromJSON(result$body)
  expect_false(body$success)
  expect_match(body$message, "not found|App not found")
})

# ============================================================================
# handle_server_shutdown() Tests
# ============================================================================

test_that("handle_server_shutdown requests shutdown of this instance only", {
  log_dir <- tempfile("tss-shutdown")
  dir.create(log_dir)
  on.exit(unlink(log_dir, recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = log_dir)
  other <- ShinyServerConfig$new()
  other$config <- list(log_dir = log_dir)

  result <- handle_server_shutdown(config)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)
  expect_true(body$success)
  expect_match(body$message, "Shutdown initiated")
  expect_true(config$shutdown_requested)
  # Another instance sharing the log directory keeps running
  expect_false(other$shutdown_requested)
  expect_length(list.files(log_dir), 0)
})

test_that("handle_server_shutdown does not need a writable log directory", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = "/nonexistent/directory/that/does/not/exist")

  result <- handle_server_shutdown(config)

  expect_equal(result$status, 200)
  expect_true(jsonlite::fromJSON(result$body)$success)
  expect_true(config$shutdown_requested)
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("full request flow works for dashboard", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  mock_tm <- list(
    generate_management_page = function() {
      "<html><head><title>Dashboard</title></head><body>Management</body></html>"
    }
  )

  req <- list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_management_request(req, config, pm, mock_tm)

  expect_equal(result$status, 200)
  expect_match(result$headers[["Content-Type"]], "text/html")
  expect_match(result$body, "Dashboard")
})

test_that("full request flow works for API endpoint", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "testapp", path = "/test/path", port = 3001, resident = TRUE)
    )
  )
  pm <- ProcessManager$new(config)

  req <- list(
    PATH_INFO = "/api/apps",
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_management_request(req, config, pm, list())

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)
  expect_true("testapp" %in% names(body))
})

test_that("API rejects overly long paths", {
  config <- ShinyServerConfig$new()
  pm <- ProcessManager$new(config)

  # Create a path longer than MAX_PATH_LENGTH
  long_path <- paste0("/", paste(rep("a", 1001), collapse = ""))

  req <- list(
    PATH_INFO = long_path,
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_management_request(req, config, pm, list())

  expect_equal(result$status, 400)
})
