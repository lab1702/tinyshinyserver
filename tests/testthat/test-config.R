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

  # App with an empty path
  for (path in c("", "   ")) {
    result <- config$validate_config(c(base_config, list(apps = list(list(name = "app1", path = path)))))
    expect_false(result$valid)
    expect_match(result$error, "App 1 path must not be empty")
  }
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

  # JSON [null] arrives as a logical NA
  conf$apps[[1]]$resident <- NA
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

test_that("validate_config rejects an empty log_dir", {
  config <- ShinyServerConfig$new()
  conf <- list(apps = list(list(name = "app1", path = "/path")), starting_port = 3001)

  # An empty log_dir would put the logs in the filesystem root
  for (log_dir in list("", "   ", NA_character_, c("a", "b"), 1)) {
    conf$log_dir <- log_dir
    result <- config$validate_config(conf)
    expect_false(result$valid)
    expect_match(result$error, "log_dir must be a non-empty string")
  }

  conf$log_dir <- "logs"
  expect_true(config$validate_config(conf)$valid)
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

test_that("an app's connection count is the number of its tracked sessions", {
  config <- ShinyServerConfig$new()

  # Adding or removing a session twice cannot count it twice
  config$add_ws_connection("session1", list(app_name = "app1"))
  config$add_ws_connection("session1", list(app_name = "app1"))
  config$add_ws_connection("session2", list(app_name = "app1"))
  expect_equal(config$get_app_connection_count("app1"), 2)
  expect_setequal(config$app_sessions[["app1"]], c("session1", "session2"))

  config$remove_ws_connection("session1")
  config$remove_ws_connection("session1")
  expect_equal(config$get_app_connection_count("app1"), 1)

  # A session counts for the app it was last added for
  config$add_ws_connection("session2", list(app_name = "app2"))
  expect_equal(config$get_app_connection_count("app1"), 0)
  expect_equal(config$get_app_connection_count("app2"), 1)

  # An app with no connections leaves nothing behind
  config$remove_ws_connection("session2")
  expect_length(ls(config$app_sessions), 0)
  expect_length(config$get_all_ws_connections(), 0)
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
      expect_equal(loaded$title, "Tiny Shiny Server")
      expect_equal(loaded$max_request_size_mb, 100)
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
    "Configuration file not found: /nonexistent/path/config.json",
    fixed = TRUE
  )
})

test_that("load_config fails for invalid JSON, naming the file", {
  config <- ShinyServerConfig$new()

  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)

  for (text in c("{ invalid json }", "{\"apps\": [],}", " ")) {
    writeLines(text, tmp_config)
    expect_error(config$load_config(tmp_config), paste("Configuration file", tmp_config, "is not valid JSON"),
      fixed = TRUE)
  }
})

test_that("a configuration file that is empty or a directory is named as such", {
  config <- ShinyServerConfig$new()
  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config, recursive = TRUE), add = TRUE)

  file.create(tmp_config)
  expect_error(config$load_config(tmp_config), paste("Configuration file", tmp_config, "is empty"), fixed = TRUE)
  unlink(tmp_config)
  dir.create(tmp_config)
  expect_error(config$load_config(tmp_config), paste("Configuration file", tmp_config, "is a directory"),
    fixed = TRUE)
  expect_error(start_tss(tmp_config), "is a directory", fixed = TRUE)
})

test_that("a \\u escape of half a surrogate pair is rejected, not read as another character", {
  config <- ShinyServerConfig$new()
  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)
  read <- function(title) {
    writeLines(sprintf('{"apps": [{"name": "app", "path": "%s"}], "log_dir": "%s", "starting_port": 3001, "title": "%s"}',
      gsub("\\", "/", unused_app_path(), fixed = TRUE), gsub("\\", "/", tempdir(), fixed = TRUE), title),
      tmp_config)
    config$read_config(tmp_config)
  }

  # A first half alone would be read as "?", a second half alone as bytes
  # that are not UTF-8
  for (title in c("x\\ud800y", "x\\uDBFF", "x\\udfffy", "x\\\\\\ud800y")) {
    expect_error(read(title), "holds a \\u escape of half a UTF-16 surrogate pair", fixed = TRUE, info = title)
  }
  # A whole pair is a character, and an escaped backslash starts no escape
  expect_equal(read("x\\ud83d\\ude00y")$title, "x\U0001F600y")
  expect_equal(read("x\\\\ud800y")$title, "x\\ud800y")
  expect_equal(read("x\\\\udfffy")$title, "x\\udfffy")
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

test_that("scheduler settings reject invalid delays before startup", {
  config <- ShinyServerConfig$new()
  base <- list(apps = list(list(name = "app", path = "/tmp/app")),
    log_dir = "/tmp/logs", starting_port = 5001)
  for (field in c("restart_delay", "health_check_interval")) {
    for (value in list(-1, Inf, -Inf, NA_real_, NaN, "10", TRUE, NULL, numeric(), c(1, 2))) {
      candidate <- base
      candidate[field] <- list(value)
      result <- config$validate_config(candidate)
      expect_false(result$valid, info = field)
      expect_match(result$error, field)
    }
    for (value in c(0.25, 5, 10)) {
      candidate <- base
      candidate[[field]] <- value
      expect_true(config$validate_config(candidate)$valid, info = field)
    }
  }
  base$restart_delay <- 0
  expect_true(config$validate_config(base)$valid)
  base$health_check_interval <- 0
  expect_false(config$validate_config(base)$valid)

  # A timer of 1e12 seconds blocks later::run_now(), and with it every request
  for (field in c("restart_delay", "health_check_interval")) {
    candidate <- base
    candidate$health_check_interval <- 10
    candidate[[field]] <- 86400
    expect_true(config$validate_config(candidate)$valid, info = field)
    for (value in c(86401, 1e12, .Machine$double.xmax)) {
      candidate[[field]] <- value
      result <- config$validate_config(candidate)
      expect_false(result$valid, info = field)
      expect_match(result$error, paste(field, ".*at most 86400"))
    }
  }
})

test_that("a field given twice is rejected rather than read from its first copy", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(paste0('{"apps": [{"name": "app", "path": "/tmp", "resident": false, "resident": "yes"}],',
    ' "log_dir": "logs", "starting_port": 3001, "proxy_port": 4000, "proxy_port": "oops"}'), path)
  expect_error(ShinyServerConfig$new()$read_config(path),
    'Duplicate fields: "proxy_port", "resident" in app 1', fixed = TRUE)
  writeLines('{"apps": [{"name": "app", "path": "/tmp"}], "log_dir": "logs", "starting_port": 3001}', path)
  expect_equal(ShinyServerConfig$new()$read_config(path)$apps[[1]]$name, "app")
})

test_that("max_request_size_mb must be a positive finite number", {
  config <- ShinyServerConfig$new()
  base <- list(apps = list(list(name = "app", path = "/tmp/app")),
    log_dir = "/tmp/logs", starting_port = 5001)
  for (value in list(0, -1, Inf, NA_real_, "10", TRUE, NULL, numeric(), c(1, 2))) {
    candidate <- base
    candidate["max_request_size_mb"] <- list(value)
    result <- config$validate_config(candidate)
    expect_false(result$valid)
    expect_match(result$error, "max_request_size_mb")
  }
  for (value in c(0.5, 5, 1000)) {
    candidate <- base
    candidate$max_request_size_mb <- value
    expect_true(config$validate_config(candidate)$valid)
  }
})

test_that("appstart_timeout validates positive finite seconds", {
  config <- ShinyServerConfig$new()
  data <- list(
    apps = list(list(name = "app", path = "/tmp/app")),
    log_dir = "/tmp/logs", starting_port = 5001
  )
  for (value in list(0, -1, Inf, -Inf, NA_real_, NaN, "2", TRUE, NULL, numeric(), c(1, 2))) {
    data$apps[[1]]["appstart_timeout"] <- list(value)
    result <- config$validate_config(data)
    expect_false(result$valid)
    expect_match(result$error, "appstart_timeout")
  }
  for (value in c(0.5, 2, 60, 86400)) {
    data$apps[[1]]$appstart_timeout <- value
    expect_true(config$validate_config(data)$valid)
  }
  data$apps[[1]]$appstart_timeout <- 1e12
  expect_match(config$validate_config(data)$error, "appstart_timeout .*at most 86400")
})

test_that("load_config defaults appstart_timeout independently for each app", {
  config <- ShinyServerConfig$new()
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  writeLines(jsonlite::toJSON(list(
    apps = list(
      list(name = "default", path = "/tmp/default"),
      list(name = "custom", path = "/tmp/custom", appstart_timeout = 12.5)
    ),
    log_dir = "/tmp/logs", starting_port = 5001
  ), auto_unbox = TRUE), path)
  local_mocked_bindings(is_port_in_use = function(host, port) FALSE)
  config$load_config(path)
  expect_equal(config$get_app_config("default")$appstart_timeout, 2)
  expect_equal(config$get_app_config("custom")$appstart_timeout, 12.5)
})

test_that("long appstart_timeout extends startup state lifetime", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", appstart_timeout = 60)))
  assign("app", list(state = "starting", started_at = Sys.time() - 35),
    envir = config$app_startup_state)
  expect_equal(config$get_app_startup_state("app")$state, "starting")
  assign("app", list(state = "starting", started_at = Sys.time() - 65),
    envir = config$app_startup_state)
  expect_equal(config$get_app_startup_state("app")$state, "timeout")
})

test_that("a clock set back prolongs neither a startup nor the wait after a failure", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", appstart_timeout = 2)), restart_delay = 7200)
  # Times an hour from now, as a clock set back an hour leaves them
  assign("app", list(state = "starting", started_at = Sys.time() + 3600), envir = config$app_startup_state)
  state <- config$get_app_startup_state("app")
  expect_equal(state$state, "starting")
  # Requests wait no longer than the app's appstart_timeout
  expect_equal(state$elapsed, 0)
  # The startup's window starts again, and so ends in time
  restarted <- get("app", envir = config$app_startup_state)$started_at
  expect_lt(abs(as.numeric(difftime(restarted, Sys.time(), units = "secs"))), 5)

  assign("app", Sys.time() + 3600, envir = config$start_failures)
  expect_equal(config$start_backoff_seconds("app"), 0)
  config$note_start_failure("app")
  expect_gt(config$start_backoff_seconds("app"), 7000)
})

test_that("the server's own clock never runs backwards and takes no long step", {
  reading <- 1000
  local_mocked_bindings(raw_elapsed_seconds = function() reading)
  config <- ShinyServerConfig$new()
  expect_equal(config$clock_seconds(), 1000)

  # It follows the clock that it reads, step by step
  reading <- 1012.5
  expect_equal(config$clock_seconds(), 1012.5)
  expect_equal(config$clock_seconds(), 1012.5)

  # A system clock set back on Unix, or proc.time() starting again from zero
  # after 49.7 days on Windows: no time has passed, and it goes on from there
  reading <- 3
  expect_equal(config$clock_seconds(), 1012.5)
  reading <- 13
  expect_equal(config$clock_seconds(), 1022.5)

  # A system clock set two hours forward counts as one long step at most
  reading <- 13 + 7200
  expect_equal(config$clock_seconds(), 1022.5 + CLOCK_MAX_STEP_SECONDS)
  reading <- reading + 1
  expect_equal(config$clock_seconds(), 1022.5 + CLOCK_MAX_STEP_SECONDS + 1)
})

test_that("a system clock set forward or back neither closes sessions nor keeps apps", {
  reading <- 5000
  local_mocked_bindings(raw_elapsed_seconds = function() reading)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = FALSE)))
  cm <- ConnectionManager$new(config)
  cm$add_client_connection("s", list(close = function() NULL), "app", "127.0.0.1", "test")
  session <- config$get_ws_connection("s")

  # Set two hours forward: the session has not been idle for two hours
  reading <- reading + 7200
  expect_lte(config$session_idle_seconds("s", session), CLOCK_MAX_STEP_SECONDS)
  # Set back by more than it ever ran: no negative idle time, which would
  # keep the session open for as long as the clock was set back
  reading <- 10
  idle <- config$session_idle_seconds("s", session)
  expect_gte(idle, 0)
  reading <- 10 + 45
  expect_equal(config$session_idle_seconds("s", session), idle + 45)

  # A large response's download time is counted from the same clock, and is
  # never longer than the longest download allowed for
  cm$end_http_request("app", response_bytes = 1e12)
  expect_equal(cm$transfer_seconds_left("app"), config$HTTP_TRANSFER_MAX_SECONDS)
  reading <- 1
  expect_equal(cm$transfer_seconds_left("app"), config$HTTP_TRANSFER_MAX_SECONDS)
  assign("app", config$clock_seconds() + 4e6, envir = config$http_transfer_ends)
  expect_equal(cm$transfer_seconds_left("app"), config$HTTP_TRANSFER_MAX_SECONDS)

  # Refused handshakes are logged again once their interval has passed,
  # whatever the system clock has done since
  logged <- capture_test_log({
    cm$note_rejected_handshake("origin", "refused")
    reading <- 0
    cm$note_rejected_handshake("origin", "refused")
    reading <- REJECTED_HANDSHAKE_LOG_SECONDS + 1
    cm$note_rejected_handshake("origin", "refused")
  })
  expect_equal(sum(grepl("^WARN", logged)), 2)
})

test_that("title must be a non-empty string of at most 100 characters", {
  config <- ShinyServerConfig$new()
  base_config <- list(apps = list(list(name = "app1", path = "/path")),
    log_dir = "/var/log", starting_port = 3001)

  expect_true(config$validate_config(c(base_config, list(title = "Acme Analytics")))$valid)
  expect_true(config$validate_config(c(base_config, list(title = strrep("x", 100))))$valid)
  for (bad in list(123, TRUE, "", "   ", c("a", "b"), NA_character_, strrep("x", 101))) {
    result <- config$validate_config(c(base_config, list(title = bad)))
    expect_false(result$valid)
    expect_match(result$error, "title must be a non-empty string")
  }
})

test_that("unrecognized configuration fields are reported instead of silently dropped", {
  config <- ShinyServerConfig$new()
  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)
  settings <- list(
    apps = list(
      list(name = "sales", path = "/tmp/sales", resdient = TRUE),
      list(name = "plain", path = "/tmp/plain", resident = TRUE, appstart_timeout = 5)
    ),
    log_dir = "/tmp/logs", starting_port = 5001, max_request_size = 10
  )
  writeLines(jsonlite::toJSON(settings, auto_unbox = TRUE), tmp_config)

  loaded <- config$read_config(tmp_config)
  expect_equal(config$ignored_fields, c('"max_request_size"', '"resdient" in app "sales"'))
  # The misspelled option has no effect
  expect_false(loaded$apps[[1]]$resident)

  warnings <- character()
  local_mocked_bindings(log_warn = function(...) {
    warnings <<- c(warnings, paste(list(...), collapse = " "))
  }, .package = "logger")
  config$log_ignored_fields()
  expect_length(warnings, 1)
  expect_match(warnings, '"max_request_size", "resdient" in app "sales"', fixed = TRUE)

  # A file with only known fields reports nothing
  settings$max_request_size <- NULL
  settings$apps[[1]]$resdient <- NULL
  writeLines(jsonlite::toJSON(settings, auto_unbox = TRUE), tmp_config)
  config$read_config(tmp_config)
  expect_length(config$ignored_fields, 0)
  warnings <- character()
  config$log_ignored_fields()
  expect_length(warnings, 0)
})

test_that("an unrecognized field that extends an option's name does not set that option", {
  config <- ShinyServerConfig$new()
  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)
  # `$` matches a list's names by prefix, so each renamed field could answer
  # for the option it was renamed from, without being validated
  settings <- list(
    apps = list(list(name = "sales", path = "/tmp/sales", appstart_timeout_old = "soon", resident_was = TRUE)),
    log_dir = "/tmp/logs", starting_port = 5001,
    proxy_port_old = "not-a-port", management_port_old = 70000, title_draft = strrep("x", 500),
    base_path_disabled = "\"><script>", health_check_interval_off = 0, log_level_was = "TRACE"
  )
  writeLines(jsonlite::toJSON(settings, auto_unbox = TRUE), tmp_config)

  loaded <- config$read_config(tmp_config)
  expect_length(config$ignored_fields, 8)
  expect_equal(loaded$proxy_port, 3838)
  expect_equal(loaded$management_port, 3839)
  expect_equal(loaded$title, DEFAULT_SERVER_TITLE)
  expect_equal(loaded$base_path, "")
  expect_equal(loaded$health_check_interval, 10)
  expect_equal(loaded$log_level, "INFO")
  expect_equal(loaded$apps[[1]]$appstart_timeout, 2)
  expect_false(loaded$apps[[1]]$resident)

  # The fields are removed, so nothing that reads the configuration later
  # can match them either
  expect_setequal(names(loaded), KNOWN_CONFIG_FIELDS)
  expect_setequal(names(loaded$apps[[1]]), KNOWN_APP_FIELDS)

  # A reserved port named only by a renamed field does not count against the port range
  result <- config$validate_config(list(
    apps = list(list(name = "a", path = "/tmp/a")), log_dir = "/tmp/logs",
    starting_port = 65535, proxy_port_old = 65535
  ))
  expect_true(result$valid)
})

test_that("app names in the configuration follow the rules for names in URLs", {
  config <- ShinyServerConfig$new()
  validate_name <- function(name) {
    config$validate_config(list(
      apps = list(list(name = name, path = "/tmp/a")), log_dir = "/tmp/logs", starting_port = 5001
    ))
  }
  for (name in list("sales", "my-app_2", strrep("a", 50))) {
    expect_true(validate_name(name)$valid)
    expect_true(validate_app_name(name)$valid)
  }
  for (name in list("my app", "", strrep("a", 51), 7, c("a", "b"), NA_character_)) {
    result <- validate_name(name)
    expect_false(result$valid)
    expect_identical(result$error, paste0("App 1: ", validate_app_name(name)$error))
  }
})

test_that("load_config trims the configured title", {
  config <- ShinyServerConfig$new()
  tmp_config <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_config), add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app1", path = "/tmp/app1")),
    log_dir = "/tmp/logs", starting_port = 5001, title = "  Acme Analytics  "),
  auto_unbox = TRUE), tmp_config)

  with_mocked_bindings(is_port_in_use = function(host, port) FALSE, {
    expect_equal(config$load_config(tmp_config)$title, "Acme Analytics")
  })
})

# ============================================================================
# Ports and app names
# ============================================================================

test_that("configuration rejects fractional and non-finite ports before allocation", {
  config <- ShinyServerConfig$new()
  base <- list(apps = list(list(name = "app", path = tempdir())), log_dir = tempdir(),
    starting_port = 3001, proxy_port = 3838, management_port = 3839)
  for (field in c("starting_port", "proxy_port", "management_port")) {
    for (value in list(3001.5, NA_real_, NaN, Inf, -Inf, NULL, "3001", c(3001, 3002))) {
      candidate <- base
      candidate[field] <- list(value)
      result <- config$validate_config(candidate)
      expect_false(result$valid, info = field)
      expect_match(result$error, paste0("Invalid ", field))
    }
  }
  expect_true(config$validate_config(base)$valid)
  expect_false(validate_port(3001.5)$valid)
})

test_that("port-range capacity excludes only reserved ports in that range", {
  config <- ShinyServerConfig$new()
  conf <- list(apps = list(list(name = "app", path = tempdir())), log_dir = tempdir(),
    starting_port = 65535, proxy_port = 3838, management_port = 3839)
  expect_true(config$validate_config(conf)$valid)
  config$config <- conf
  local_mocked_bindings(is_port_in_use = function(...) FALSE)
  config$assign_app_ports()
  expect_equal(config$config$apps[[1]]$port, 65535)
  conf$management_port <- 65535
  expect_false(config$validate_config(conf)$valid)
  conf$starting_port <- 65534
  conf$proxy_port <- 65535
  expect_true(config$validate_config(conf)$valid)
})

test_that("duplicate names fail configuration validation before any app starts", {
  config <- ShinyServerConfig$new()
  data <- list(apps = list(list(name = "app", path = "/tmp/one"), list(name = "app", path = "/tmp/two")),
    log_dir = "/tmp", starting_port = 3001)
  result <- config$validate_config(data)
  expect_false(result$valid)
  expect_match(result$error, "Duplicate app name: app", fixed = TRUE)
  # Names differing only in case would share log files on Windows and macOS
  data$apps[[2]]$name <- "App"
  result <- config$validate_config(data)
  expect_false(result$valid)
  expect_match(result$error, "Duplicate app name: App", fixed = TRUE)
  data$apps[[2]]$name <- "other"
  expect_true(config$validate_config(data)$valid)
})

test_that("port assignments reach the server log", {
  path <- tempfile(fileext = ".json")
  log_dir <- tempfile("tss-logs")
  on.exit({
    logger::log_appender(logger::appender_console, namespace = "tinyshinyserver")
    unlink(c(path, log_dir), recursive = TRUE)
  }, add = TRUE)
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "app", path = unused_app_path())),
    log_dir = log_dir, starting_port = 3001), auto_unbox = TRUE), path)
  local_mocked_bindings(is_port_in_use = function(...) FALSE)
  TinyShinyServer$new(path)
  log <- readLines(file.path(log_dir, "server.log"))
  expect_true(any(grepl("App 'app' -> port 3001", log, fixed = TRUE)))
})

# ============================================================================
# Idle timeout, log level and unusable apps
# ============================================================================

test_that("idle_timeout_minutes defaults to 30 and accepts zero", {
  expect_equal(read_test_config()$idle_timeout_minutes, 30)
  expect_equal(read_test_config(idle_timeout_minutes = 0)$idle_timeout_minutes, 0)
  expect_equal(read_test_config(idle_timeout_minutes = 0.5)$idle_timeout_minutes, 0.5)
  for (value in list(-1, "30", c(1, 2))) {
    expect_error(read_test_config(idle_timeout_minutes = value),
      "idle_timeout_minutes must be a single non-negative finite number of minutes")
  }
})

test_that("log_level defaults to INFO and is normalized to upper case", {
  expect_equal(read_test_config()$log_level, "INFO")
  expect_equal(read_test_config(log_level = "debug")$log_level, "DEBUG")
  expect_equal(read_test_config(log_level = "WARN")$log_level, "WARN")
  for (value in list("TRACE", "", 3, c("INFO", "DEBUG"))) {
    expect_error(read_test_config(log_level = value), "log_level must be one of: DEBUG, INFO, WARN, ERROR")
  }
})

test_that("the new options are known configuration fields", {
  expect_length(unrecognized_config_fields(list(idle_timeout_minutes = 0, log_level = "DEBUG", apps = list())), 0)
})

test_that("on-demand apps that cannot start are reported when the configuration is loaded", {
  log_dir <- tempfile("tss-logging")
  app_dir <- make_test_app_dir()
  on.exit(restore_test_logging(log_dir), add = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  setup_logging(log_dir, "INFO")
  config <- ShinyServerConfig$new()
  missing <- file.path(tempdir(), "tss-no-such-app")
  config$config <- list(apps = list(
    list(name = "good", path = app_dir, resident = FALSE),
    list(name = "ghost", path = missing, resident = FALSE),
    # A resident app reports the problem itself when it is started
    list(name = "resident-ghost", path = missing, resident = TRUE)
  ))
  config$log_unusable_apps()

  warnings <- grep("cannot start", readLines(file.path(log_dir, "server.log")), value = TRUE)
  expect_length(warnings, 1)
  expect_match(warnings, "On-demand app ghost cannot start: App directory not found")
})

# ============================================================================
# Apps sharing a directory
# ============================================================================

test_that("apps may not share a document that is rendered into its directory", {
  root <- tempfile("tss-shared")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  document <- function(dir, file, ...) {
    dir.create(file.path(root, dir))
    writeLines(c("---", ..., "---", "", "Text"), file.path(root, dir, file))
    file.path(root, dir)
  }
  quarto <- document("quarto", "dash.qmd", "server: shiny")
  prerendered <- document("prerendered", "tutorial.Rmd", "runtime: shiny_prerendered")
  shinyrmd <- document("shinyrmd", "tutorial.Rmd", "runtime: shinyrmd")
  server_shiny <- document("server-shiny", "report.Rmd", "server: shiny")
  server_type <- document("server-type", "report.Rmd", "server:", "  type: shiny")
  runtime_shiny <- document("runtime-shiny", "report.Rmd", "runtime: shiny")
  shiny_app <- file.path(root, "app")
  dir.create(shiny_app)
  writeLines("shiny::shinyApp(shiny::fluidPage(), function(input, output) NULL)", file.path(shiny_app, "app.R"))

  read_apps <- function(...) {
    apps <- lapply(list(...), function(pair) list(name = pair[[1]], path = pair[[2]]))
    path <- tempfile(fileext = ".json")
    on.exit(unlink(path), add = TRUE)
    writeLines(jsonlite::toJSON(list(apps = apps, log_dir = tempdir(), starting_port = 3001), auto_unbox = TRUE), path)
    ShinyServerConfig$new()$read_config(path)
  }

  # The same directory however it is spelled
  expect_error(read_apps(c("a", quarto), c("b", paste0(quarto, "/"))),
    "Apps a and b serve the same document, dash.qmd, which is rendered into its directory", fixed = TRUE)
  expect_error(read_apps(c("a", quarto), c("b", file.path(quarto, "..", "quarto")), c("c", quarto)),
    "Apps a, b, and c serve the same document, dash.qmd", fixed = TRUE)
  # rmarkdown::run() renders a prerendered Shiny document in place too
  for (dir in c(prerendered, shinyrmd, server_shiny, server_type)) {
    expect_error(read_apps(c("a", dir), c("b", dir)), "serve the same document", fixed = TRUE)
  }

  # Each document rendered once, a document rendered into a temporary
  # directory, a Shiny app, and a missing directory may be shared
  expect_length(read_apps(c("a", quarto), c("b", prerendered))$apps, 2)
  expect_length(read_apps(c("a", runtime_shiny), c("b", runtime_shiny))$apps, 2)
  expect_length(read_apps(c("a", shiny_app), c("b", shiny_app))$apps, 2)
  missing <- file.path(root, "missing")
  expect_length(read_apps(c("a", missing), c("b", missing))$apps, 2)
})

test_that("a configuration file that is not UTF-8 text is named as such", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  json <- '{"apps": [{"name": "app", "path": "/tmp"}], "log_dir": "/tmp", "starting_port": 3001, "title": "Caf\xe9"}'
  writeBin(charToRaw(json), path)
  expect_error(ShinyServerConfig$new()$read_config(path), "is not valid UTF-8 text")
  # Windows PowerShell 5.1 saves files as UTF-16 by default
  writeBin(c(as.raw(c(0xff, 0xfe)), as.raw(rbind(charToRaw("{}"), as.raw(0)))), path)
  expect_error(ShinyServerConfig$new()$read_config(path), "saved as UTF-16")
  # A byte-order mark is allowed, without a warning
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(enc2utf8(sub("Caf\xe9", "Cafe", json, useBytes = TRUE)))), path)
  expect_no_warning(config <- ShinyServerConfig$new()$read_config(path))
  expect_equal(config$title, "Cafe")
})

test_that("a configuration file with a NUL character is refused, not read in part", {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  json <- function(app_path, after = "") {
    paste0('{"apps": [{"name": "app", "path": "', app_path, '"}], "log_dir": "/tmp", "starting_port": 3001}', after)
  }
  # R would drop the rest of the line after a NUL byte
  writeBin(c(charToRaw(json("/tmp")), as.raw(0), charToRaw(" not JSON")), path)
  expect_error(ShinyServerConfig$new()$read_config(path), "contains a NUL byte")
  # jsonlite would end the path at the escaped NUL
  writeLines(json("/tmp/a\\u0000x"), path)
  expect_error(ShinyServerConfig$new()$read_config(path), "contains the NUL character")
  writeLines(json("/tmp/a\\\\\\u0000x"), path)
  expect_error(ShinyServerConfig$new()$read_config(path), "contains the NUL character")
  # A backslash followed by the text "u0000" is allowed
  writeLines(json("/tmp/a\\\\u0000x"), path)
  expect_equal(ShinyServerConfig$new()$read_config(path)$apps[[1]]$path, "/tmp/a\\u0000x")
})

test_that("a configuration file is read as JSON, never as the name of another file", {
  dir <- tempfile("tss-config")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  other <- file.path(dir, "other.json")
  writeLines(jsonlite::toJSON(list(apps = list(list(name = "other", path = "/tmp")), log_dir = "/tmp",
    starting_port = 3001), auto_unbox = TRUE), other)
  path <- file.path(dir, "config.json")
  writeLines(other, path)
  expect_error(ShinyServerConfig$new()$read_config(path), "lexical error|invalid char")
})

test_that("max_response_size_mb defaults to 1024 and must be a positive number", {
  expect_equal(read_test_config()$max_response_size_mb, 1024)
  expect_equal(read_test_config(max_response_size_mb = 5000)$max_response_size_mb, 5000)
  for (value in list(0, -1, "big", c(1, 2))) {
    expect_error(read_test_config(max_response_size_mb = value), "max_response_size_mb must be", info = format(value))
  }
})

test_that("the configuration file is kept as given, made absolute, without resolving links", {
  dir <- tempfile("tss-config")
  dir.create(file.path(dir, "r1"), recursive = TRUE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  write_config <- function(path, name) {
    writeLines(jsonlite::toJSON(list(apps = list(list(name = name, path = "/tmp")), log_dir = tempdir(),
      starting_port = 3001), auto_unbox = TRUE), path)
  }
  write_config(file.path(dir, "r1", "config.json"), "one")
  local_mocked_bindings(is_port_in_use = function(...) FALSE)

  old_dir <- setwd(dir)
  on.exit(setwd(old_dir), add = TRUE, after = FALSE)
  config <- create_server_config(file.path("r1", "config.json"))
  expect_equal(config$config_file, file.path(getwd(), "r1", "config.json"))

  # A link that a deployment points at a new release, as a Kubernetes ConfigMap does
  link <- file.path(dir, "current")
  linked <- if (.Platform$OS.type == "windows") {
    suppressWarnings(Sys.junction(file.path(dir, "r1"), link))
  } else {
    file.symlink(file.path(dir, "r1"), link)
  }
  skip_if_not(isTRUE(linked), "Cannot create a link")
  config <- create_server_config(file.path(link, "config.json"))
  dir.create(file.path(dir, "r2"))
  write_config(file.path(dir, "r2", "config.json"), "two")
  unlink(link)
  if (.Platform$OS.type == "windows") Sys.junction(file.path(dir, "r2"), link) else file.symlink(file.path(dir, "r2"), link)
  unlink(file.path(dir, "r1"), recursive = TRUE)
  check <- config$read_reload_config()
  expect_true(check$valid)
  expect_equal(check$config$apps[[1]]$name, "two")
})
