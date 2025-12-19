# Test for configuration loading and validation
# Tests for ShinyServerConfig class methods

# ============================================================================
# validate_config() tests
# ============================================================================

test_that("validate_config accepts valid configuration", {
  config <- ShinyServerConfig$new()

  valid_conf <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", resident = TRUE),
      list(name = "app2", path = "/path/to/app2", resident = FALSE)
    ),
    log_dir = "/var/log/shiny",
    starting_port = 3001,
    proxy_port = 3838,
    proxy_host = "127.0.0.1",
    management_port = 3839
  )

  result <- config$validate_config(valid_conf)
  expect_true(result$valid)
  expect_equal(result$sanitized, valid_conf)
})

test_that("validate_config accepts minimal valid configuration", {
  config <- ShinyServerConfig$new()

  minimal_conf <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1")
    ),
    log_dir = "/var/log/shiny",
    starting_port = 3001
  )

  result <- config$validate_config(minimal_conf)
  expect_true(result$valid)
})

test_that("validate_config rejects non-list config", {
  config <- ShinyServerConfig$new()

  result <- config$validate_config("not a list")
  expect_false(result$valid)
  expect_match(result$error, "must be a list")
})

test_that("validate_config rejects missing required fields", {
  config <- ShinyServerConfig$new()

  # Missing apps
  result <- config$validate_config(list(log_dir = "/var/log", starting_port = 3001))
  expect_false(result$valid)
  expect_match(result$error, "Missing required field.*apps")

  # Missing log_dir
  result <- config$validate_config(list(
    apps = list(list(name = "app1", path = "/path")),
    starting_port = 3001
  ))
  expect_false(result$valid)
  expect_match(result$error, "Missing required field.*log_dir")

  # Missing starting_port
  result <- config$validate_config(list(
    apps = list(list(name = "app1", path = "/path")),
    log_dir = "/var/log"
  ))
  expect_false(result$valid)
  expect_match(result$error, "Missing required field.*starting_port")
})

test_that("validate_config validates starting_port", {
  config <- ShinyServerConfig$new()

  base_config <- list(
    apps = list(list(name = "app1", path = "/path")),
    log_dir = "/var/log"
  )

  # Non-numeric
  result <- config$validate_config(c(base_config, list(starting_port = "3001")))
  expect_false(result$valid)
  expect_match(result$error, "Invalid starting_port")

  # Out of range (too low)
  result <- config$validate_config(c(base_config, list(starting_port = 0)))
  expect_false(result$valid)
  expect_match(result$error, "Invalid starting_port")

  # Out of range (too high)
  result <- config$validate_config(c(base_config, list(starting_port = 65536)))
  expect_false(result$valid)
  expect_match(result$error, "Invalid starting_port")

  # Valid
  result <- config$validate_config(c(base_config, list(starting_port = 8080)))
  expect_true(result$valid)
})

test_that("validate_config checks port range sufficiency", {
  config <- ShinyServerConfig$new()

  # Create config with starting_port too high for number of apps
  many_apps <- lapply(1:100, function(i) {
    list(name = paste0("app", i), path = paste0("/path", i))
  })

  conf <- list(
    apps = many_apps,
    log_dir = "/var/log",
    starting_port = 65500,
    proxy_port = 3838,
    management_port = 3839
  )

  result <- config$validate_config(conf)
  expect_false(result$valid)
  expect_match(result$error, "Not enough port range")
})

test_that("validate_config validates apps array", {
  config <- ShinyServerConfig$new()

  base_config <- list(
    log_dir = "/var/log",
    starting_port = 3001
  )

  # Apps not a list
  result <- config$validate_config(c(base_config, list(apps = "not a list")))
  expect_false(result$valid)
  expect_match(result$error, "Apps must be a non-empty list")

  # Apps is empty list
  result <- config$validate_config(c(base_config, list(apps = list())))
  expect_false(result$valid)
  expect_match(result$error, "Apps must be a non-empty list")
})

test_that("validate_config validates individual app configurations", {
  config <- ShinyServerConfig$new()

  base_config <- list(
    log_dir = "/var/log",
    starting_port = 3001
  )

  # App not a list
  result <- config$validate_config(c(base_config, list(apps = list("not a list"))))
  expect_false(result$valid)
  expect_match(result$error, "App .* must be a list")

  # App missing name
  result <- config$validate_config(c(base_config, list(apps = list(list(path = "/path")))))
  expect_false(result$valid)
  expect_match(result$error, "missing field.*name")

  # App missing path
  result <- config$validate_config(c(base_config, list(apps = list(list(name = "app1")))))
  expect_false(result$valid)
  expect_match(result$error, "missing field.*path")
})

test_that("validate_config validates app resident field", {
  config <- ShinyServerConfig$new()

  # Invalid resident type
  conf <- list(
    apps = list(list(name = "app1", path = "/path", resident = "yes")),
    log_dir = "/var/log",
    starting_port = 3001
  )

  result <- config$validate_config(conf)
  expect_false(result$valid)
  expect_match(result$error, "resident field must be a single logical value")

  # Valid resident values
  conf$apps[[1]]$resident <- TRUE
  result <- config$validate_config(conf)
  expect_true(result$valid)

  conf$apps[[1]]$resident <- FALSE
  result <- config$validate_config(conf)
  expect_true(result$valid)
})

test_that("validate_config validates app name", {
  config <- ShinyServerConfig$new()

  base_config <- list(
    log_dir = "/var/log",
    starting_port = 3001
  )

  # Name not a string
  result <- config$validate_config(c(base_config, list(apps = list(list(name = 123, path = "/path")))))
  expect_false(result$valid)
  expect_match(result$error, "name must be a string")

  # Name with invalid characters
  result <- config$validate_config(c(base_config, list(apps = list(list(name = "app name", path = "/path")))))
  expect_false(result$valid)
  expect_match(result$error, "name contains invalid characters")

  # Name too long
  long_name <- paste0(rep("a", 51), collapse = "")
  result <- config$validate_config(c(base_config, list(apps = list(list(name = long_name, path = "/path")))))
  expect_false(result$valid)
  expect_match(result$error, "name too long")

  # Valid names
  valid_names <- c("app1", "my-app", "my_app", "MyApp123")
  for (name in valid_names) {
    result <- config$validate_config(c(base_config, list(apps = list(list(name = name, path = "/path")))))
    expect_true(result$valid, info = paste("Should accept:", name))
  }
})

test_that("validate_config validates optional proxy_port", {
  config <- ShinyServerConfig$new()

  base_config <- list(
    apps = list(list(name = "app1", path = "/path")),
    log_dir = "/var/log",
    starting_port = 3001
  )

  # Invalid proxy_port
  result <- config$validate_config(c(base_config, list(proxy_port = "8080")))
  expect_false(result$valid)
  expect_match(result$error, "Invalid proxy_port")

  result <- config$validate_config(c(base_config, list(proxy_port = 0)))
  expect_false(result$valid)
  expect_match(result$error, "Invalid proxy_port")

  result <- config$validate_config(c(base_config, list(proxy_port = 65536)))
  expect_false(result$valid)
  expect_match(result$error, "Invalid proxy_port")

  # Valid proxy_port
  result <- config$validate_config(c(base_config, list(proxy_port = 3838)))
  expect_true(result$valid)
})

test_that("validate_config validates optional proxy_host", {
  config <- ShinyServerConfig$new()

  base_config <- list(
    apps = list(list(name = "app1", path = "/path")),
    log_dir = "/var/log",
    starting_port = 3001
  )

  # Invalid type
  result <- config$validate_config(c(base_config, list(proxy_host = 123)))
  expect_false(result$valid)
  expect_match(result$error, "proxy_host must be a string")

  # Invalid host value
  result <- config$validate_config(c(base_config, list(proxy_host = "example.com")))
  expect_false(result$valid)
  expect_match(result$error, "proxy_host must be one of")

  # Valid hosts
  valid_hosts <- c("localhost", "127.0.0.1", "0.0.0.0", "::1", "::")
  for (host in valid_hosts) {
    result <- config$validate_config(c(base_config, list(proxy_host = host)))
    expect_true(result$valid, info = paste("Should accept:", host))
  }
})

# ============================================================================
# Helper method tests
# ============================================================================

test_that("get_proxy_host converts localhost to 127.0.0.1", {
  config <- ShinyServerConfig$new()

  # Set config with localhost
  config$config <- list(proxy_host = "localhost")
  expect_equal(config$get_proxy_host(), "127.0.0.1")

  # Set config with 127.0.0.1
  config$config <- list(proxy_host = "127.0.0.1")
  expect_equal(config$get_proxy_host(), "127.0.0.1")

  # Set config with 0.0.0.0
  config$config <- list(proxy_host = "0.0.0.0")
  expect_equal(config$get_proxy_host(), "0.0.0.0")

  # No proxy_host set (default)
  config$config <- list()
  expect_equal(config$get_proxy_host(), "127.0.0.1")
})

# ============================================================================
# WebSocket connection management tests
# ============================================================================

test_that("add_ws_connection adds new connection and increments count", {
  config <- ShinyServerConfig$new()

  result <- config$add_ws_connection("session1", list(app_name = "app1", client = "test"))
  expect_true(result)

  # Verify connection was added
  conn <- config$get_ws_connection("session1")
  expect_equal(conn$app_name, "app1")
  expect_equal(conn$client, "test")

  # Verify count was incremented
  expect_equal(config$get_app_connection_count("app1"), 1)
})

test_that("add_ws_connection handles multiple connections for same app", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("session1", list(app_name = "app1"))
  config$add_ws_connection("session2", list(app_name = "app1"))
  config$add_ws_connection("session3", list(app_name = "app1"))

  expect_equal(config$get_app_connection_count("app1"), 3)
})

test_that("add_ws_connection handles connections for different apps", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("session1", list(app_name = "app1"))
  config$add_ws_connection("session2", list(app_name = "app2"))
  config$add_ws_connection("session3", list(app_name = "app1"))

  expect_equal(config$get_app_connection_count("app1"), 2)
  expect_equal(config$get_app_connection_count("app2"), 1)
})

test_that("add_ws_connection does not double-count on update", {
  config <- ShinyServerConfig$new()

  # Add initial connection
  config$add_ws_connection("session1", list(app_name = "app1", status = "new"))
  expect_equal(config$get_app_connection_count("app1"), 1)

  # Update same connection (should not increment count)
  config$add_ws_connection("session1", list(app_name = "app1", status = "updated"))
  expect_equal(config$get_app_connection_count("app1"), 1)

  # Verify info was updated
  conn <- config$get_ws_connection("session1")
  expect_equal(conn$status, "updated")
})

test_that("add_ws_connection rejects NULL inputs", {
  config <- ShinyServerConfig$new()

  expect_false(config$add_ws_connection(NULL, list(app_name = "app1")))
  expect_false(config$add_ws_connection("session1", NULL))
})

test_that("add_ws_connection handles connection with NULL app_name", {
  config <- ShinyServerConfig$new()

  result <- config$add_ws_connection("session1", list(app_name = NULL, other = "data"))
  expect_true(result)

  # Connection should be stored
  conn <- config$get_ws_connection("session1")
  expect_equal(conn$other, "data")
})

test_that("remove_ws_connection removes connection and decrements count", {
  config <- ShinyServerConfig$new()

  # Add connection
  config$add_ws_connection("session1", list(app_name = "app1"))
  expect_equal(config$get_app_connection_count("app1"), 1)

  # Remove connection
  result <- config$remove_ws_connection("session1")
  expect_true(result)

  # Verify connection was removed
  expect_null(config$get_ws_connection("session1"))

  # Verify count was decremented
  expect_equal(config$get_app_connection_count("app1"), 0)
})

test_that("remove_ws_connection is idempotent", {
  config <- ShinyServerConfig$new()

  # Add and remove connection
  config$add_ws_connection("session1", list(app_name = "app1"))
  config$remove_ws_connection("session1")

  # Try to remove again - should return FALSE and not cause errors
  result <- config$remove_ws_connection("session1")
  expect_false(result)

  # Count should still be 0
  expect_equal(config$get_app_connection_count("app1"), 0)
})

test_that("remove_ws_connection rejects NULL session_id", {
  config <- ShinyServerConfig$new()

  expect_false(config$remove_ws_connection(NULL))
})

test_that("remove_ws_connection prevents negative counts", {
  config <- ShinyServerConfig$new()

  # Add one connection
  config$add_ws_connection("session1", list(app_name = "app1"))
  expect_equal(config$get_app_connection_count("app1"), 1)

  # Remove it
  config$remove_ws_connection("session1")
  expect_equal(config$get_app_connection_count("app1"), 0)

  # Manually add another connection directly to test edge case
  config$add_ws_connection("session2", list(app_name = "app1"))

  # Manually corrupt the count to test protection
  assign("app1", -5, envir = config$app_connection_counts)

  # Remove should use max(0, ...) to prevent negative
  config$remove_ws_connection("session2")
  expect_gte(config$get_app_connection_count("app1"), 0)
})

test_that("get_all_ws_connections returns all connections", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("session1", list(app_name = "app1"))
  config$add_ws_connection("session2", list(app_name = "app2"))

  all_conns <- config$get_all_ws_connections()
  expect_equal(length(all_conns), 2)
  expect_true("session1" %in% names(all_conns))
  expect_true("session2" %in% names(all_conns))
})

# ============================================================================
# validate_connection_count_consistency tests
# ============================================================================

test_that("validate_connection_count_consistency returns consistent when counts match", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("session1", list(app_name = "app1"))
  config$add_ws_connection("session2", list(app_name = "app1"))
  config$add_ws_connection("session3", list(app_name = "app2"))

  result <- config$validate_connection_count_consistency(fix_errors = FALSE)
  expect_true(result$consistent)
  expect_equal(length(result$inconsistencies), 0)
})

test_that("validate_connection_count_consistency detects inconsistencies", {
  config <- ShinyServerConfig$new()

  # Add connections normally
  config$add_ws_connection("session1", list(app_name = "app1"))
  config$add_ws_connection("session2", list(app_name = "app1"))

  # Manually corrupt the cache
  assign("app1", 10, envir = config$app_connection_counts)

  result <- config$validate_connection_count_consistency(fix_errors = FALSE)
  expect_false(result$consistent)
  expect_true("app1" %in% names(result$inconsistencies))
  expect_equal(result$inconsistencies$app1$cached, 10)
  expect_equal(result$inconsistencies$app1$actual, 2)
})

test_that("validate_connection_count_consistency fixes errors when requested", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("session1", list(app_name = "app1"))

  # Corrupt the cache
  assign("app1", 99, envir = config$app_connection_counts)

  # Fix errors
  result <- config$validate_connection_count_consistency(fix_errors = TRUE)
  expect_false(result$consistent) # Was inconsistent

  # Verify it was fixed
  expect_equal(config$get_app_connection_count("app1"), 1)

  # Run again - should be consistent now
  result2 <- config$validate_connection_count_consistency(fix_errors = FALSE)
  expect_true(result2$consistent)
})

test_that("validate_connection_count_consistency handles empty state", {
  config <- ShinyServerConfig$new()

  result <- config$validate_connection_count_consistency(fix_errors = FALSE)
  expect_true(result$consistent)
  expect_equal(result$total_apps_checked, 0)
})

# ============================================================================
# Backend connection management tests
# ============================================================================

test_that("backend connection management works correctly", {
  config <- ShinyServerConfig$new()

  # Add backend connection
  config$add_backend_connection("session1", list(ws = "mock_ws", app_name = "app1"))

  # Get backend connection
  conn <- config$get_backend_connection("session1")
  expect_equal(conn$ws, "mock_ws")
  expect_equal(conn$app_name, "app1")

  # Get all backend connections
  all_conns <- config$get_all_backend_connections()
  expect_equal(length(all_conns), 1)

  # Remove backend connection
  config$remove_backend_connection("session1")
  expect_null(config$get_backend_connection("session1"))
})

# ============================================================================
# App process management tests
# ============================================================================

test_that("app process management works correctly", {
  config <- ShinyServerConfig$new()

  # Add process
  mock_process <- list(pid = 12345, status = "running")
  config$add_app_process("app1", mock_process)

  # Get process
  proc <- config$get_app_process("app1")
  expect_equal(proc$pid, 12345)

  # Get all processes
  all_procs <- config$get_all_app_processes()
  expect_equal(length(all_procs), 1)
  expect_true("app1" %in% names(all_procs))

  # Remove process
  config$remove_app_process("app1")
  expect_null(config$get_app_process("app1"))
})

# ============================================================================
# App startup state tracking tests
# ============================================================================

test_that("set_app_starting marks app as starting", {
  config <- ShinyServerConfig$new()

  config$set_app_starting("app1")

  state <- config$get_app_startup_state("app1")
  expect_equal(state$state, "starting")
  expect_true(!is.null(state$started_at))
})

test_that("set_app_ready clears startup state", {
  config <- ShinyServerConfig$new()

  config$set_app_starting("app1")
  expect_true(config$is_app_starting("app1"))

  config$set_app_ready("app1")
  expect_false(config$is_app_starting("app1"))
  expect_null(config$get_app_startup_state("app1"))
})

test_that("is_app_starting returns correct state", {
  config <- ShinyServerConfig$new()

  # App not in startup state
  expect_false(config$is_app_starting("app1"))

  # Mark as starting
  config$set_app_starting("app1")
  expect_true(config$is_app_starting("app1"))

  # Mark as ready
  config$set_app_ready("app1")
  expect_false(config$is_app_starting("app1"))
})

test_that("get_app_startup_state detects timeout", {
  config <- ShinyServerConfig$new()

  # Set a very short timeout for testing
  original_timeout <- config$APP_STARTUP_TIMEOUT_SECONDS
  config$APP_STARTUP_TIMEOUT_SECONDS <- 0

  config$set_app_starting("app1")

  # Small delay to ensure timeout
  Sys.sleep(0.1)

  state <- config$get_app_startup_state("app1")
  expect_equal(state$state, "timeout")

  # Restore timeout
  config$APP_STARTUP_TIMEOUT_SECONDS <- original_timeout
})

# ============================================================================
# get_app_config and get_sorted_apps tests
# ============================================================================

test_that("get_app_config returns correct app configuration", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1", port = 3001),
      list(name = "app2", path = "/path2", port = 3002)
    )
  )

  app1 <- config$get_app_config("app1")
  expect_equal(app1$name, "app1")
  expect_equal(app1$path, "/path1")

  app2 <- config$get_app_config("app2")
  expect_equal(app2$name, "app2")

  # Non-existent app
  expect_null(config$get_app_config("nonexistent"))
})

test_that("get_sorted_apps returns apps in alphabetical order", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "zebra", path = "/z"),
      list(name = "alpha", path = "/a"),
      list(name = "middle", path = "/m")
    )
  )

  sorted <- config$get_sorted_apps()
  expect_equal(sorted[[1]]$name, "alpha")
  expect_equal(sorted[[2]]$name, "middle")
  expect_equal(sorted[[3]]$name, "zebra")
})

# ============================================================================
# load_config tests (file-based)
# ============================================================================

test_that("load_config loads valid configuration from file", {
  config <- ShinyServerConfig$new()

  # Create a temporary config file
  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)

  config_data <- list(
    apps = list(
      list(name = "testapp", path = "/tmp/testapp")
    ),
    log_dir = "/tmp/logs",
    starting_port = 4001,
    proxy_port = 4000,
    management_port = 4002
  )

  writeLines(jsonlite::toJSON(config_data, auto_unbox = TRUE, pretty = TRUE), tmp_config)

  # Mock is_port_in_use to always return FALSE (ports available)
  with_mocked_bindings(
    is_port_in_use = function(host, port) FALSE,
    {
      loaded <- config$load_config(tmp_config)
      expect_equal(loaded$apps[[1]]$name, "testapp")
      expect_equal(loaded$proxy_port, 4000)
      expect_equal(loaded$starting_port, 4001)
    }
  )
})

test_that("load_config sets default values for optional fields", {
  config <- ShinyServerConfig$new()

  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)

  # Minimal config without optional fields
  config_data <- list(
    apps = list(
      list(name = "testapp", path = "/tmp/testapp")
    ),
    log_dir = "/tmp/logs",
    starting_port = 5001
  )

  writeLines(jsonlite::toJSON(config_data, auto_unbox = TRUE, pretty = TRUE), tmp_config)

  with_mocked_bindings(
    is_port_in_use = function(host, port) FALSE,
    {
      loaded <- config$load_config(tmp_config)

      # Check defaults were applied
      expect_equal(loaded$proxy_port, 3838)
      expect_equal(loaded$proxy_host, "127.0.0.1")
      expect_equal(loaded$management_port, 3839)
      expect_equal(loaded$restart_delay, 5)
      expect_equal(loaded$health_check_interval, 10)
    }
  )
})

test_that("load_config sets default resident=FALSE for apps", {
  config <- ShinyServerConfig$new()

  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)

  config_data <- list(
    apps = list(
      list(name = "app1", path = "/tmp/app1"),
      list(name = "app2", path = "/tmp/app2", resident = TRUE)
    ),
    log_dir = "/tmp/logs",
    starting_port = 5001
  )

  writeLines(jsonlite::toJSON(config_data, auto_unbox = TRUE, pretty = TRUE), tmp_config)

  with_mocked_bindings(
    is_port_in_use = function(host, port) FALSE,
    {
      loaded <- config$load_config(tmp_config)

      # app1 should get default resident=FALSE
      expect_false(loaded$apps[[1]]$resident)
      # app2 should keep its explicit resident=TRUE
      expect_true(loaded$apps[[2]]$resident)
    }
  )
})

test_that("load_config fails for non-existent file", {
  config <- ShinyServerConfig$new()

  expect_error(
    config$load_config("/nonexistent/path/config.json"),
    "No such file|cannot open|not found|cannot find the path"
  )
})

test_that("load_config fails for invalid JSON", {
  config <- ShinyServerConfig$new()

  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)

  writeLines("{ invalid json }", tmp_config)

  expect_error(config$load_config(tmp_config))
})

test_that("load_config fails for invalid configuration", {
  config <- ShinyServerConfig$new()

  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)

  # Missing required fields
  config_data <- list(
    apps = list()
  )

  writeLines(jsonlite::toJSON(config_data, auto_unbox = TRUE, pretty = TRUE), tmp_config)

  expect_error(
    config$load_config(tmp_config),
    "validation failed"
  )
})

# ============================================================================
# create_server_config tests
# ============================================================================

test_that("create_server_config creates and loads configuration", {
  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)

  config_data <- list(
    apps = list(
      list(name = "testapp", path = "/tmp/testapp")
    ),
    log_dir = "/tmp/logs",
    starting_port = 6001
  )

  writeLines(jsonlite::toJSON(config_data, auto_unbox = TRUE, pretty = TRUE), tmp_config)

  with_mocked_bindings(
    is_port_in_use = function(host, port) FALSE,
    {
      server_config <- create_server_config(tmp_config)

      expect_s4_class(server_config, "ShinyServerConfig")
      expect_equal(server_config$config$apps[[1]]$name, "testapp")
    }
  )
})

# ============================================================================
# assign_app_ports tests
# ============================================================================

test_that("assign_app_ports assigns sequential ports to apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1"),
      list(name = "app2", path = "/path2"),
      list(name = "app3", path = "/path3")
    ),
    starting_port = 5000,
    proxy_port = 3838,
    management_port = 3839
  )

  with_mocked_bindings(
    is_port_in_use = function(host, port) FALSE,
    {
      config$assign_app_ports()

      expect_equal(config$config$apps[[1]]$port, 5000)
      expect_equal(config$config$apps[[2]]$port, 5001)
      expect_equal(config$config$apps[[3]]$port, 5002)
    }
  )
})

test_that("assign_app_ports skips reserved ports", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1"),
      list(name = "app2", path = "/path2")
    ),
    starting_port = 3838, # Same as proxy_port
    proxy_port = 3838,
    management_port = 3839
  )

  with_mocked_bindings(
    is_port_in_use = function(host, port) FALSE,
    {
      config$assign_app_ports()

      # Should skip 3838 (proxy) and 3839 (management)
      expect_equal(config$config$apps[[1]]$port, 3840)
      expect_equal(config$config$apps[[2]]$port, 3841)
    }
  )
})

test_that("assign_app_ports skips ports in use", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1"),
      list(name = "app2", path = "/path2")
    ),
    starting_port = 5000,
    proxy_port = 3838,
    management_port = 3839
  )

  # Simulate port 5000 being in use
  with_mocked_bindings(
    is_port_in_use = function(host, port) port == 5000,
    {
      config$assign_app_ports()

      # Should skip 5000 and start at 5001
      expect_equal(config$config$apps[[1]]$port, 5001)
      expect_equal(config$config$apps[[2]]$port, 5002)
    }
  )
})

# ============================================================================
# validate_port_assignments tests
# ============================================================================

test_that("validate_port_assignments passes with unique ports", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1", port = 5000),
      list(name = "app2", path = "/path2", port = 5001)
    ),
    proxy_port = 3838,
    management_port = 3839
  )

  # Should not throw an error
  expect_no_error(config$validate_port_assignments())
})

test_that("validate_port_assignments detects duplicate app ports", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1", port = 5000),
      list(name = "app2", path = "/path2", port = 5000) # Duplicate!
    ),
    proxy_port = 3838,
    management_port = 3839
  )

  expect_error(
    config$validate_port_assignments(),
    "Port conflict"
  )
})

test_that("validate_port_assignments detects conflict with proxy port", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1", port = 3838) # Same as proxy!
    ),
    proxy_port = 3838,
    management_port = 3839
  )

  expect_error(
    config$validate_port_assignments(),
    "Port conflict"
  )
})

test_that("validate_port_assignments detects conflict with management port", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path1", port = 3839) # Same as management!
    ),
    proxy_port = 3838,
    management_port = 3839
  )

  expect_error(
    config$validate_port_assignments(),
    "Port conflict"
  )
})
