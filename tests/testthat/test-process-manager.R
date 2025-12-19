# Test for process management functions
# Tests for ProcessManager class methods

# ============================================================================
# ProcessManager Initialization Tests
# ============================================================================

test_that("ProcessManager initializes with config", {
  config <- ShinyServerConfig$new()
  pm <- ProcessManager$new(config)

  expect_true(inherits(pm, "ProcessManager"))
  expect_identical(pm$config, config)
})

test_that("create_process_manager factory function works", {
  config <- ShinyServerConfig$new()
  pm <- create_process_manager(config)

  expect_true(inherits(pm, "ProcessManager"))
  expect_identical(pm$config, config)
})

# ============================================================================
# get_app_status() Tests
# ============================================================================

test_that("get_app_status returns NULL for unknown app", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  status <- pm$get_app_status("nonexistent")
  expect_null(status)
})

test_that("get_app_status returns 'dormant' for non-resident app without process", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
    )
  )
  pm <- ProcessManager$new(config)

  status <- pm$get_app_status("app1")

  expect_equal(status$name, "app1")
  expect_equal(status$status, "dormant")
  expect_false(status$resident)
  expect_equal(status$port, 3001)
  expect_equal(status$connections, 0)
  expect_null(status$pid)
})

test_that("get_app_status returns 'stopped' for resident app without process", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE)
    )
  )
  pm <- ProcessManager$new(config)

  status <- pm$get_app_status("app1")

  expect_equal(status$name, "app1")
  expect_equal(status$status, "stopped")
  expect_true(status$resident)
})

test_that("get_app_status returns 'running' for app with alive process", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE)
    )
  )

  # Create mock process that reports as alive
  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  # Mock is_process_alive to return TRUE
  local_mocked_bindings(
    is_process_alive = function(process) TRUE
  )

  status <- pm$get_app_status("app1")

  expect_equal(status$status, "running")
  expect_equal(status$pid, 12345)
})

test_that("get_app_status returns 'crashed' for app with dead process", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE)
    )
  )

  # Create mock process that reports as dead
  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() FALSE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  # Mock is_process_alive to return FALSE
  local_mocked_bindings(
    is_process_alive = function(process) FALSE
  )

  status <- pm$get_app_status("app1")

  expect_equal(status$status, "crashed")
  expect_null(status$pid)
})

test_that("get_app_status includes connection count", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
    )
  )

  # Add some connections
  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app1", ws = NULL))

  pm <- ProcessManager$new(config)

  status <- pm$get_app_status("app1")

  expect_equal(status$connections, 2)
})

# ============================================================================
# get_all_app_status() Tests
# ============================================================================

test_that("get_all_app_status returns status for all apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE),
      list(name = "app2", path = "/path/to/app2", port = 3002, resident = FALSE),
      list(name = "app3", path = "/path/to/app3", port = 3003, resident = FALSE)
    )
  )
  pm <- ProcessManager$new(config)

  all_status <- pm$get_all_app_status()

  expect_equal(length(all_status), 3)
  expect_true("app1" %in% names(all_status))
  expect_true("app2" %in% names(all_status))
  expect_true("app3" %in% names(all_status))
})

test_that("get_all_app_status returns empty list when no apps configured", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  all_status <- pm$get_all_app_status()

  expect_equal(length(all_status), 0)
})

# ============================================================================
# get_app_connection_count() Tests
# ============================================================================

test_that("get_app_connection_count delegates to config", {
  config <- ShinyServerConfig$new()
  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s3", list(app_name = "app2", ws = NULL))

  pm <- ProcessManager$new(config)

  expect_equal(pm$get_app_connection_count("app1"), 2)
  expect_equal(pm$get_app_connection_count("app2"), 1)
  expect_equal(pm$get_app_connection_count("app3"), 0)
})

# ============================================================================
# stop_app() Tests
# ============================================================================

test_that("stop_app returns failure for app not running", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  result <- pm$stop_app("nonexistent")

  expect_false(result$success)
  expect_match(result$message, "not found|already stopped")
})

test_that("stop_app stops running process and removes from tracking", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE)
    )
  )

  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  # Mock kill_process_safely to return TRUE
  local_mocked_bindings(
    kill_process_safely = function(process) TRUE
  )

  result <- pm$stop_app("app1")

  expect_true(result$success)
  expect_match(result$message, "stopped successfully")
  expect_null(config$get_app_process("app1"))
})

test_that("stop_app returns failure when kill fails", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE)
    )
  )

  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  # Mock kill_process_safely to return FALSE
  local_mocked_bindings(
    kill_process_safely = function(process) FALSE
  )

  result <- pm$stop_app("app1")

  expect_false(result$success)
  expect_match(result$message, "Failed to stop")
})

# ============================================================================
# stop_app_immediately() Tests
# ============================================================================

test_that("stop_app_immediately returns FALSE for unknown app", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  result <- pm$stop_app_immediately("nonexistent")

  expect_false(result)
})

test_that("stop_app_immediately returns FALSE for resident app", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE)
    )
  )
  pm <- ProcessManager$new(config)

  result <- pm$stop_app_immediately("app1")

  expect_false(result)
})

test_that("stop_app_immediately stops non-resident app", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
    )
  )

  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  # Mock kill_process_safely to return TRUE
  local_mocked_bindings(
    kill_process_safely = function(process) TRUE
  )

  result <- pm$stop_app_immediately("app1")

  expect_true(result)
})

# ============================================================================
# restart_app() Tests
# ============================================================================

test_that("restart_app returns failure for unknown app", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  result <- pm$restart_app("nonexistent")

  expect_false(result$success)
  expect_match(result$message, "not found")
})

test_that("restart_app stops existing process and starts new one", {
  # Create temp app dir for valid path
  temp_app_dir <- file.path(tempdir(), "restart_app_test")
  dir.create(temp_app_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines("# placeholder", file.path(temp_app_dir, "app.R"))

  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = TRUE)
    ),
    log_dir = tempdir(),
    restart_delay = 0 # No delay for testing
  )

  kill_called <- FALSE
  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    kill_process_safely = function(process) {
      kill_called <<- TRUE
      TRUE
    },
    is_process_alive = function(process) TRUE
  )

  result <- pm$restart_app("app1")

  # Verify kill was called on old process
  expect_true(kill_called)
  # Restart should succeed
  expect_true(result$success)
  # New process should be starting
  expect_true(config$is_app_starting("app1"))

  # Cleanup
  unlink(temp_app_dir, recursive = TRUE)
})

test_that("restart_app catches errors and returns failure", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/nonexistent/path", port = 3001, resident = TRUE)
    ),
    log_dir = "/nonexistent/log/dir",
    restart_delay = 0
  )

  # Add a mock process that will cause errors during cleanup
  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  # Force an error by having kill fail and then start_app fail
  local_mocked_bindings(
    kill_process_safely = function(process) {
      stop("Simulated kill error")
    },
    is_process_alive = function(process) TRUE
  )

  result <- pm$restart_app("app1")

  expect_false(result$success)
  expect_match(result$message, "Failed to restart")
})

# ============================================================================
# start_app_on_demand() Tests
# ============================================================================

test_that("start_app_on_demand returns FALSE for unknown app", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  result <- pm$start_app_on_demand("nonexistent")

  expect_false(result)
})

test_that("start_app_on_demand returns TRUE if app already running", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
    )
  )

  mock_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", mock_process)

  pm <- ProcessManager$new(config)

  # Mock is_process_alive
  local_mocked_bindings(
    is_process_alive = function(process) TRUE
  )

  result <- pm$start_app_on_demand("app1")

  expect_true(result)
})

test_that("start_app_on_demand returns TRUE if app is starting", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
    )
  )

  # Mark app as starting
  config$set_app_starting("app1")

  pm <- ProcessManager$new(config)

  result <- pm$start_app_on_demand("app1")

  expect_true(result)
})

test_that("start_app_on_demand attempts to start app if not running", {
  # Create a temp directory with an app.R file so start_app doesn't fail early
  temp_app_dir <- file.path(tempdir(), "test_app_on_demand")
  dir.create(temp_app_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines("# placeholder", file.path(temp_app_dir, "app.R"))

  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = FALSE)
    ),
    log_dir = tempdir()
  )

  pm <- ProcessManager$new(config)

  # The actual start_app will be called - we verify it marks app as starting
  # and returns TRUE (indicating start was initiated)
  result <- pm$start_app_on_demand("app1")

  expect_true(result)
  # App should be marked as starting (async startup)
  expect_true(config$is_app_starting("app1"))

  # Cleanup
  unlink(temp_app_dir, recursive = TRUE)
})

# ============================================================================
# cleanup_app_connections() Tests
# ============================================================================

test_that("cleanup_app_connections removes connections for specific app", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  # Add mixed connections
  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s3", list(app_name = "app2", ws = NULL))

  config$add_backend_connection("b1", list(app_name = "app1", ws = NULL))
  config$add_backend_connection("b2", list(app_name = "app2", ws = NULL))

  pm <- ProcessManager$new(config)

  pm$cleanup_app_connections("app1")

  # app1 connections should be removed
  expect_null(config$get_ws_connection("s1"))
  expect_null(config$get_ws_connection("s2"))
  expect_null(config$get_backend_connection("b1"))

  # app2 connections should remain
  expect_false(is.null(config$get_ws_connection("s3")))
  expect_false(is.null(config$get_backend_connection("b2")))
})

test_that("cleanup_app_connections handles empty connection lists", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  # Should not error
  expect_no_error(pm$cleanup_app_connections("app1"))
})

test_that("cleanup_app_connections closes WebSocket handles", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  # Track if close was called
  ws_closed <- FALSE
  mock_ws <- list(
    close = function() {
      ws_closed <<- TRUE
    }
  )

  config$add_ws_connection("s1", list(app_name = "app1", ws = mock_ws))
  config$add_backend_connection("b1", list(app_name = "app1", ws = mock_ws))

  pm <- ProcessManager$new(config)

  pm$cleanup_app_connections("app1")

  expect_true(ws_closed)
})

# ============================================================================
# cleanup_stale_connections() Tests
# ============================================================================

test_that("cleanup_stale_connections removes old connections", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  config$CONNECTION_TIMEOUT_MINUTES <- 30

  # Add fresh connection
  config$add_ws_connection("fresh", list(
    app_name = "app1",
    ws = NULL,
    last_activity = Sys.time()
  ))

  # Add stale connection (manually, to bypass normal add)
  config$ws_connections[["stale"]] <- list(
    app_name = "app1",
    ws = NULL,
    last_activity = Sys.time() - (35 * 60) # 35 minutes ago
  )

  # Add stale backend connection
  config$add_backend_connection("stale_backend", list(
    app_name = "app1",
    ws = NULL,
    last_activity = Sys.time() - (35 * 60)
  ))

  pm <- ProcessManager$new(config)

  cleaned <- pm$cleanup_stale_connections()

  expect_gte(cleaned, 1)
  expect_null(config$get_backend_connection("stale_backend"))
  # Fresh connection should remain
  expect_false(is.null(config$get_ws_connection("fresh")))
})

test_that("cleanup_stale_connections handles empty connections", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  cleaned <- pm$cleanup_stale_connections()

  expect_equal(cleaned, 0)
})

test_that("cleanup_stale_connections handles connections without last_activity", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  config$CONNECTION_TIMEOUT_MINUTES <- 30

  # Add connection without last_activity
  config$add_ws_connection("no_activity", list(
    app_name = "app1",
    ws = NULL
  ))

  pm <- ProcessManager$new(config)

  # Should not error and should not remove connection without timestamp
  cleaned <- pm$cleanup_stale_connections()

  expect_equal(cleaned, 0)
  expect_false(is.null(config$get_ws_connection("no_activity")))
})

# ============================================================================
# cleanup_dead_processes() Tests
# ============================================================================

test_that("cleanup_dead_processes removes dead processes", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  # Add dead process
  dead_process <- list(
    get_pid = function() 99999,
    is_alive = function() FALSE
  )
  config$add_app_process("dead_app", dead_process)

  # Add live process
  live_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("live_app", live_process)

  pm <- ProcessManager$new(config)

  # Mock is_process_alive
  local_mocked_bindings(
    is_process_alive = function(process) {
      process$is_alive()
    }
  )

  cleaned <- pm$cleanup_dead_processes()

  expect_equal(cleaned, 1)
  expect_null(config$get_app_process("dead_app"))
  expect_false(is.null(config$get_app_process("live_app")))
})

test_that("cleanup_dead_processes handles empty process list", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  pm <- ProcessManager$new(config)

  cleaned <- pm$cleanup_dead_processes()

  expect_equal(cleaned, 0)
})

test_that("cleanup_dead_processes cleans up orphaned connections", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  # Add dead process with connections
  dead_process <- list(
    get_pid = function() 99999,
    is_alive = function() FALSE
  )
  config$add_app_process("dead_app", dead_process)
  config$add_ws_connection("orphan", list(app_name = "dead_app", ws = NULL))

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) FALSE
  )

  pm$cleanup_dead_processes()

  # Connection should be cleaned up
  expect_null(config$get_ws_connection("orphan"))
})

# ============================================================================
# stop_all_apps() Tests
# ============================================================================

test_that("stop_all_apps stops all running processes", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  mock_process1 <- list(get_pid = function() 1, is_alive = function() TRUE)
  mock_process2 <- list(get_pid = function() 2, is_alive = function() TRUE)

  config$add_app_process("app1", mock_process1)
  config$add_app_process("app2", mock_process2)

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    kill_process_safely = function(process) TRUE
  )

  pm$stop_all_apps()

  expect_null(config$get_app_process("app1"))
  expect_null(config$get_app_process("app2"))
})

test_that("stop_all_apps cleans up all connections", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app2", ws = NULL))
  config$add_backend_connection("b1", list(app_name = "app1", ws = NULL))

  pm <- ProcessManager$new(config)

  pm$stop_all_apps()

  expect_equal(length(config$get_all_ws_connections()), 0)
  expect_equal(length(config$get_all_backend_connections()), 0)
})

test_that("stop_all_apps closes WebSocket handles", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  ws_closed <- FALSE
  mock_ws <- list(close = function() { ws_closed <<- TRUE })

  config$add_backend_connection("b1", list(app_name = "app1", ws = mock_ws))

  pm <- ProcessManager$new(config)

  pm$stop_all_apps()

  expect_true(ws_closed)
})

# ============================================================================
# health_check() Tests
# ============================================================================

test_that("health_check removes dead process and cleans connections for resident app", {
  config <- ShinyServerConfig$new()

  # Create temp app dir for valid path
  temp_app_dir <- file.path(tempdir(), "health_check_test1")
  dir.create(temp_app_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines("# placeholder", file.path(temp_app_dir, "app.R"))

  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = TRUE)
    ),
    log_dir = tempdir(),
    restart_delay = 0
  )

  dead_process <- list(
    get_pid = function() 99999,
    is_alive = function() FALSE
  )
  config$add_app_process("app1", dead_process)
  # Add a connection that should be cleaned up
  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) FALSE
  )

  pm$health_check()

  # Dead process should be removed (before restart attempt)
  # Note: health_check will try to restart, which adds new process
  # Connection should be cleaned up
  expect_null(config$get_ws_connection("s1"))

  # Cleanup
  unlink(temp_app_dir, recursive = TRUE)
})

test_that("health_check removes dead non-resident app but does not restart it", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
    ),
    log_dir = tempdir()
  )

  dead_process <- list(
    get_pid = function() 99999,
    is_alive = function() FALSE
  )
  config$add_app_process("app1", dead_process)

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) FALSE
  )

  pm$health_check()

  # Process should be removed
  expect_null(config$get_app_process("app1"))
  # App should NOT be marked as starting (non-resident apps don't auto-restart)
  expect_false(config$is_app_starting("app1"))
})

test_that("health_check starts missing resident app", {
  # Create temp app dir for valid path
  temp_app_dir <- file.path(tempdir(), "health_check_test2")
  dir.create(temp_app_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines("# placeholder", file.path(temp_app_dir, "app.R"))

  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = TRUE)
    ),
    log_dir = tempdir()
  )

  pm <- ProcessManager$new(config)

  # Initially no process
  expect_null(config$get_app_process("app1"))

  pm$health_check()

  # After health_check, resident app should be starting
  expect_true(config$is_app_starting("app1"))

  # Cleanup
  unlink(temp_app_dir, recursive = TRUE)
})

test_that("health_check does not start missing non-resident app", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
    ),
    log_dir = tempdir()
  )

  pm <- ProcessManager$new(config)

  # Initially no process
  expect_null(config$get_app_process("app1"))

  pm$health_check()

  # Non-resident app should NOT be started by health_check
  expect_false(config$is_app_starting("app1"))
  expect_null(config$get_app_process("app1"))
})

test_that("health_check leaves running apps alone", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE)
    ),
    log_dir = tempdir()
  )

  live_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", live_process)

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) TRUE
  )

  pm$health_check()

  # App should NOT be marked as starting (it's already running)
  expect_false(config$is_app_starting("app1"))
  # Process should still be tracked
  expect_false(is.null(config$get_app_process("app1")))
})

# ============================================================================
# check_app_ready() Tests
# ============================================================================

test_that("check_app_ready returns FALSE if process dies", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  dead_process <- list(
    get_pid = function() 99999,
    is_alive = function() FALSE
  )
  config$add_app_process("app1", dead_process)
  config$set_app_starting("app1")

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) FALSE
  )

  result <- pm$check_app_ready("app1", 3001, dead_process)

  expect_false(result)
  # Startup state should be cleared
  expect_false(config$is_app_starting("app1"))
  # Process should be removed
  expect_null(config$get_app_process("app1"))
})

test_that("check_app_ready returns TRUE when port is listening", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  live_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$set_app_starting("app1")

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    is_port_in_use = function(host, port) TRUE
  )

  result <- pm$check_app_ready("app1", 3001, live_process)

  expect_true(result)
  # Startup state should be cleared (app is ready)
  expect_false(config$is_app_starting("app1"))
})

test_that("check_app_ready returns FALSE when max attempts exceeded", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  live_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$set_app_starting("app1")

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    is_port_in_use = function(host, port) FALSE
  )

  # Call with max attempts already reached
  result <- pm$check_app_ready("app1", 3001, live_process, attempt = 10, max_attempts = 10)

  expect_false(result)
  # Startup state should be cleared (timed out)
  expect_false(config$is_app_starting("app1"))
})
