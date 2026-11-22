# Keep scheduled process-manager callbacks out of other test files.
later::with_loop(later::create_loop(), {
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
  app_dir <- make_test_app_dir()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = app_dir, port = 3001, resident = FALSE)
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

test_that("an on-demand app whose directory cannot be served is unavailable until it can", {
  app_dir <- tempfile("tss-app")
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(
    list(name = "ondemand", path = app_dir, port = 3001, resident = FALSE),
    list(name = "resident", path = app_dir, port = 3002, resident = TRUE)
  ))
  pm <- ProcessManager$new(config)

  # A missing directory, then one without an entry point
  expect_equal(pm$get_app_status("ondemand")$status, "unavailable")
  dir.create(app_dir)
  expect_equal(pm$get_app_status("ondemand")$status, "unavailable")
  # A resident app in the same state is reported as before
  expect_equal(pm$get_app_status("resident")$status, "stopped")

  writeLines("# placeholder", file.path(app_dir, "app.R"))
  expect_equal(pm$get_app_status("ondemand")$status, "dormant")
})

test_that("a dormant app's directory is looked at again only once it changes", {
  app_dir <- tempfile("tss-app")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  writeLines("# placeholder", file.path(app_dir, "app.R"))
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "ondemand", path = app_dir, port = 3001, resident = FALSE)))
  pm <- ProcessManager$new(config)

  checks <- 0
  local_mocked_bindings(app_start_problem = function(app_config) {
    checks <<- checks + 1
    tryCatch({
      detect_app_type(normalizePath(app_config$path, mustWork = FALSE))
      NULL
    }, error = function(e) conditionMessage(e))
  })

  # Every landing page asks every few seconds; one look serves them all
  Sys.setFileTime(app_dir, Sys.time() - 60)
  for (i in 1:3) expect_equal(pm$get_app_status("ondemand")$status, "dormant")
  expect_equal(checks, 1)

  # Removing the app's entry point changes the directory
  unlink(file.path(app_dir, "app.R"))
  expect_equal(pm$get_app_status("ondemand")$status, "unavailable")
  expect_equal(checks, 2)

  # A directory changed moments before it was looked at is looked at again,
  # as its time may be recorded in whole seconds. A time ahead of the clock
  # stands for that moment however slowly these lines run.
  writeLines("# placeholder", file.path(app_dir, "app.R"))
  Sys.setFileTime(app_dir, Sys.time() + 60)
  checks <- 0
  expect_equal(pm$get_app_status("ondemand")$status, "dormant")
  expect_equal(pm$get_app_status("ondemand")$status, "dormant")
  expect_equal(checks, 2)

  # A reload forgets what was found
  Sys.setFileTime(app_dir, Sys.time() - 60)
  pm$get_app_status("ondemand")
  checks <- 0
  pm$get_app_status("ondemand")
  expect_equal(checks, 0)
  config$reset_app_state()
  pm$get_app_status("ondemand")
  expect_equal(checks, 1)
})

test_that("a finding about an app directory is reused for a limited time", {
  app_dir <- tempfile("tss-app")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  Sys.setFileTime(app_dir, Sys.time() - 60)
  config <- ShinyServerConfig$new()
  app <- list(name = "ondemand", path = app_dir, port = 3001, resident = FALSE)
  config$config <- list(apps = list(app))

  # The directory is read after the time that the finding records, so the
  # 2-second margin counts from before the read, however long it takes
  entered <- NULL
  local_mocked_bindings(app_start_problem = function(app_config) {
    entered <<- c(entered, as.numeric(Sys.time()))
    "No app.R found"
  })
  expect_equal(config$cached_start_problem(app), "No app.R found")
  expect_lte(as.numeric(config$start_problems$ondemand$read_at), entered[1])

  # A fixed permission, or a file added on a FAT or exFAT drive, can leave the
  # directory's time unchanged, so an old finding is not used
  expect_equal(config$cached_start_problem(app), "No app.R found")
  expect_length(entered, 1)
  config$START_PROBLEM_MAX_AGE_SECONDS <- 0
  expect_equal(config$cached_start_problem(app), "No app.R found")
  expect_length(entered, 2)
})

test_that("an app that is not running has no pid field, which JSON would show as {}", {
  app_dir <- make_test_app_dir()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(
    list(name = "dormant", path = app_dir, port = 3001, resident = FALSE),
    list(name = "stopped", path = app_dir, port = 3002, resident = TRUE),
    list(name = "crashed", path = app_dir, port = 3003, resident = TRUE),
    list(name = "running", path = app_dir, port = 3004, resident = TRUE)
  ))
  config$add_app_process("crashed", list(get_pid = function() 111, is_alive = function() FALSE))
  config$add_app_process("running", list(get_pid = function() 222, is_alive = function() TRUE))
  pm <- ProcessManager$new(config)

  for (name in c("dormant", "stopped", "crashed")) {
    expect_false("pid" %in% names(pm$get_app_status(name)), info = name)
  }
  expect_equal(pm$get_app_status("running")$pid, 222)

  body <- handle_management_apps_api(pm)$body
  expect_no_match(as.character(body), "{}", fixed = TRUE)
  expect_equal(jsonlite::fromJSON(body)$running$pid, 222)
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
  on.exit(stop_test_app_processes(config), add = TRUE)
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

test_that("start_app_on_demand returns FALSE when the app process cannot start", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app1", path = tempdir(), port = 3001, resident = FALSE)))
  pm <- ProcessManager$new(config)
  assign("start_app", function(app_config) stop("cannot start processx process"), envir = pm)
  expect_false(pm$start_app_on_demand("app1"))
})

test_that("start_app_on_demand attempts to start app if not running", {
  # Create a temp directory with an app.R file so start_app doesn't fail early
  temp_app_dir <- file.path(tempdir(), "test_app_on_demand")
  dir.create(temp_app_dir, showWarnings = FALSE, recursive = TRUE)
  writeLines("# placeholder", file.path(temp_app_dir, "app.R"))

  config <- ShinyServerConfig$new()
  on.exit(stop_test_app_processes(config), add = TRUE)
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

test_that("cleanup_stale_connections uses idle_timeout_minutes, and zero keeps idle sessions", {
  stale_config <- function(idle_timeout_minutes) {
    config <- ShinyServerConfig$new()
    config$config <- list(apps = list(), idle_timeout_minutes = idle_timeout_minutes)
    config$add_ws_connection("quiet", list(app_name = "app1", ws = NULL, last_activity = Sys.time() - 45 * 60))
    config
  }

  # Idle for 45 minutes: over the default, under a raised limit
  config <- stale_config(30)
  expect_equal(ProcessManager$new(config)$cleanup_stale_connections(), 1)
  expect_null(config$get_ws_connection("quiet"))

  config <- stale_config(60)
  expect_equal(ProcessManager$new(config)$cleanup_stale_connections(), 0)
  expect_false(is.null(config$get_ws_connection("quiet")))

  config <- stale_config(0)
  expect_equal(ProcessManager$new(config)$cleanup_stale_connections(), 0)
  expect_false(is.null(config$get_ws_connection("quiet")))
})

test_that("a session's latest message decides whether it is idle", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(), idle_timeout_minutes = 30)
  opened <- Sys.time() - 45 * 60
  config$add_ws_connection("busy", list(app_name = "app1", ws = NULL, last_activity = opened))
  config$add_ws_connection("quiet", list(app_name = "app1", ws = NULL, last_activity = opened))
  expect_equal(config$get_session_activity("busy", opened), opened)
  config$touch_session("busy")
  expect_gt(config$get_session_activity("busy", opened), opened)

  expect_equal(ProcessManager$new(config)$cleanup_stale_connections(), 1)
  expect_false(is.null(config$get_ws_connection("busy")))
  expect_null(config$get_ws_connection("quiet"))

  # A removed session leaves no activity record behind
  config$remove_ws_connection("busy")
  expect_null(config$get_session_activity("busy"))
  expect_length(ls(config$session_activity), 0)
})

# ============================================================================
# Dead processes are removed by health_check()
# ============================================================================

test_that("health_check removes dead processes and their connections", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(
    list(name = "dead_app", resident = FALSE),
    list(name = "live_app", resident = FALSE)
  ))
  config$add_app_process("dead_app", list(get_pid = function() 99999, is_alive = function() FALSE))
  config$add_app_process("live_app", list(get_pid = function() 12345, is_alive = function() TRUE))
  config$add_ws_connection("orphan", list(app_name = "dead_app", ws = NULL))
  config$add_ws_connection("kept", list(app_name = "live_app", ws = NULL))

  ProcessManager$new(config)$health_check()

  expect_null(config$get_app_process("dead_app"))
  expect_false(is.null(config$get_app_process("live_app")))
  expect_null(config$get_ws_connection("orphan"))
  expect_false(is.null(config$get_ws_connection("kept")))
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

test_that("stop_all_apps tries a surviving process again and reports what is left", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  config$add_app_process("slow", list(name = "slow"))
  config$add_app_process("stuck", list(name = "stuck"))
  pm <- ProcessManager$new(config)

  attempts <- c(slow = 0, stuck = 0)
  local_mocked_bindings(kill_process_safely = function(process) {
    attempts[[process$name]] <<- attempts[[process$name]] + 1
    # "slow" is gone by the second attempt; "stuck" never stops
    process$name == "slow" && attempts[["slow"]] == 2
  })
  surviving <- NULL
  logged <- capture_test_log(surviving <- pm$stop_all_apps())

  expect_equal(attempts, c(slow = 2, stuck = 2))
  expect_equal(surviving, "stuck")
  expect_null(config$get_app_process("slow"))
  expect_true(any(grepl("Failed to stop app stuck", logged, fixed = TRUE)))
  expect_true(any(grepl("Could not stop apps: stuck", logged, fixed = TRUE)))
  expect_false(any(grepl("All applications stopped", logged, fixed = TRUE)))

  # With nothing left running, the usual message is logged
  config$remove_app_process("stuck")
  logged <- capture_test_log(surviving <- pm$stop_all_apps())
  expect_length(surviving, 0)
  expect_true(any(grepl("All applications stopped", logged, fixed = TRUE)))
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
  on.exit(stop_test_app_processes(config), add = TRUE)

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
  on.exit(stop_test_app_processes(config), add = TRUE)
  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = TRUE)
    ),
    log_dir = tempdir(),
    restart_delay = 0
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
  config$config <- list(apps = list(list(name = "app1", port = 3001)))

  live_process <- list(
    get_pid = function() 12345,
    is_alive = function() TRUE
  )
  config$add_app_process("app1", live_process)
  config$set_app_starting("app1")

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    process_owns_port = function(process, port) TRUE,
    wait_for_backend = function(url, wait_seconds = 0) promises::promise_resolve(TRUE)
  )

  result <- await_response(pm$check_app_ready("app1", 3001, live_process))

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
  config$add_app_process("app1", live_process)
  config$set_app_starting("app1")

  pm <- ProcessManager$new(config)

  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    wait_for_backend = function(url, wait_seconds = 0) promises::promise_resolve(FALSE)
  )

  # Call with max attempts already reached
  result <- await_response(pm$check_app_ready("app1", 3001, live_process, attempt = 10, max_attempts = 10))

  expect_false(result)
  # Startup state should be cleared (timed out)
  expect_false(config$is_app_starting("app1"))
})

test_that("readiness checks continue for apps with a longer appstart_timeout", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app1", appstart_timeout = 40)))
  config$set_app_starting("app1")
  pm <- ProcessManager$new(config)
  config$add_app_process("app1", list())
  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    wait_for_backend = function(url, wait_seconds = 0) promises::promise_resolve(FALSE)
  )
  later::with_loop(later::create_loop(), {
    expect_false(await_response(pm$check_app_ready("app1", 3001, list(), attempt = 60)))
    expect_true(config$is_app_starting("app1"))

    expect_false(await_response(pm$check_app_ready("app1", 3001, list(), attempt = 80)))
    expect_false(config$is_app_starting("app1"))
  })
})

test_that("readiness checks cover the full startup window for short appstart_timeout", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app1")))
  config$set_app_starting("app1")
  pm <- ProcessManager$new(config)
  config$add_app_process("app1", list())
  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    wait_for_backend = function(url, wait_seconds = 0) promises::promise_resolve(FALSE)
  )
  last_attempt <- config$APP_STARTUP_TIMEOUT_SECONDS / 0.5
  later::with_loop(later::create_loop(), {
    # A slow app (e.g. Quarto rendering) is still starting after 10 attempts
    expect_false(await_response(pm$check_app_ready("app1", 3001, list(), attempt = 10)))
    expect_true(config$is_app_starting("app1"))

    expect_false(await_response(pm$check_app_ready("app1", 3001, list(), attempt = last_attempt)))
    expect_false(config$is_app_starting("app1"))
  })
})

test_that("readiness checks keep watching an app that is still starting after the window", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app1", port = 3001)))
  config$set_app_starting("app1")
  pm <- ProcessManager$new(config)
  started <- Sys.time()
  process <- list(get_start_time = function() started)
  config$add_app_process("app1", process)
  ready <- FALSE
  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    process_owns_port = function(process, port) TRUE,
    wait_for_backend = function(url, wait_seconds = 0) promises::promise_resolve(ready)
  )
  last_attempt <- config$APP_STARTUP_TIMEOUT_SECONDS / 0.5
  # Whether another check is scheduled, once the promise callbacks that are
  # already due have run
  check_scheduled <- function() {
    for (i in 1:10) later::run_now(0)
    !later::loop_empty()
  }
  later::with_loop(later::create_loop(), {
    # A long render outlasts the window in which requests wait: the next
    # request reports the timeout, but the app is not given up on
    log <- capture_test_log(expect_false(await_response(pm$check_app_ready("app1", 3001, process,
      attempt = last_attempt))))
    expect_equal(config$get_app_startup_state("app1")$state, "timeout")
    expect_match(paste(log, collapse = "\n"), "still waiting for it to start", fixed = TRUE)
    expect_no_match(paste(log, collapse = "\n"), "failed to become ready", fixed = TRUE)
    expect_true(check_scheduled())

    # Once it listens, it is ready
    ready <- TRUE
    log <- capture_test_log(expect_true(await_response(pm$check_app_ready("app1", 3001, process,
      attempt = last_attempt + 5))))
    expect_null(config$get_app_startup_state("app1"))
    expect_match(paste(log, collapse = "\n"), "App app1 is ready", fixed = TRUE)
  })

  # An app that has not listened for as long as an app may keep starting is given up on
  ready <- FALSE
  started <- Sys.time() - config$ON_DEMAND_START_LIMIT_SECONDS
  later::with_loop(later::create_loop(), {
    log <- capture_test_log(expect_false(await_response(pm$check_app_ready("app1", 3001, process,
      attempt = last_attempt + 300))))
    expect_match(paste(log, collapse = "\n"), "failed to become ready", fixed = TRUE)
    expect_false(check_scheduled())
  })
})

})

# ============================================================================
# App directories and document types
# ============================================================================

test_that("detect_app_type names what a directory holds, in order of preference", {
  app_dir <- tempfile("tss-detect")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  add <- function(...) for (name in c(...)) writeLines("", file.path(app_dir, name))
  clear <- function() unlink(list.files(app_dir, full.names = TRUE))

  add("b.qmd", "a.qmd")
  expect_equal(detect_app_type(app_dir)$type, "qmd")
  expect_equal(basename(detect_app_type(app_dir)$file), "a.qmd")

  add("z.Rmd", "report.Rmd")
  expect_equal(detect_app_type(app_dir)$type, "rmd")
  expect_equal(basename(detect_app_type(app_dir)$file), "report.Rmd")

  add("ui.R")
  expect_equal(detect_app_type(app_dir)$type, "rmd")
  add("server.R")
  expect_equal(detect_app_type(app_dir), list(type = "shiny"))

  clear()
  add("app.R", "notes.Rmd")
  expect_equal(detect_app_type(app_dir), list(type = "shiny"))

  # shiny::runApp() serves a server.R whose page is www/index.html
  clear()
  add("server.R")
  expect_equal(detect_app_type(app_dir), list(type = "shiny"))

  # rmarkdown and Quarto accept a document's extension in any letter case
  clear()
  add("dash.QMD")
  expect_equal(detect_app_type(app_dir)$type, "qmd")
  expect_equal(basename(detect_app_type(app_dir)$file), "dash.QMD")
  add("report.rmd")
  expect_equal(detect_app_type(app_dir)$type, "rmd")
  expect_equal(basename(detect_app_type(app_dir)$file), "report.rmd")
})

test_that("detect_app_type picks the document that rmarkdown would serve", {
  app_dir <- tempfile("tss-detect")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  add <- function(...) for (name in c(...)) writeLines("", file.path(app_dir, name))
  clear <- function() unlink(list.files(app_dir, full.names = TRUE))
  picked <- function() basename(detect_app_type(app_dir)$file)

  # A child document in another letter case does not displace the main one
  add("report.Rmd", "appendix.rmd")
  expect_equal(picked(), "report.Rmd")
  clear()
  add("dash.qmd", "aside.QMD")
  expect_equal(picked(), "dash.qmd")

  # Partials, whose names start with "_", are never served
  clear()
  add("_child.Rmd", "app.Rmd")
  expect_equal(picked(), "app.Rmd")
  clear()
  add("_include.qmd", "Dashboard.qmd")
  expect_equal(picked(), "Dashboard.qmd")
  clear()
  add("_child.Rmd", "_setup.qmd")
  # The reason names the partials, which would otherwise seem to be documents
  expect_error(detect_app_type(app_dir),
    "No app.R, ui.R and server.R, .Rmd, or .qmd file found in .*\\(_child.Rmd, _setup.qmd not served: a name that starts with \"_\" marks a partial")

  # An index document is preferred, in any letter case
  clear()
  add("about.Rmd", "Index.rmd", "notes.Rmd")
  expect_equal(picked(), "Index.rmd")

  # Otherwise, as in rmarkdown::run(), a Shiny document is preferred to a static one
  clear()
  front <- function(name, ...) writeLines(c("---", ..., "---", "", "Text"), file.path(app_dir, name))
  front("appendix.Rmd", "title: Appendix")
  front("report.Rmd", "title: Report", "runtime: shiny")
  expect_equal(picked(), "report.Rmd")
  # Among Shiny documents, the usual spelling of the extension still comes first
  front("notes.rmd", "runtime: shiny_prerendered")
  expect_equal(picked(), "report.Rmd")
  clear()
  front("appendix.Rmd", "title: Appendix")
  front("tutorial.Rmd", "runtime: shiny_prerendered")
  expect_equal(picked(), "tutorial.Rmd")
  clear()
  front("about.qmd", "title: About")
  front("dash.qmd", "server:", "  type: shiny")
  expect_equal(picked(), "dash.qmd")
  front("board.qmd", "server: shiny")
  expect_equal(picked(), "board.qmd")
})

test_that("is_shiny_document reads the runtime that a document's front matter asks for", {
  doc <- tempfile(fileext = ".Rmd")
  on.exit(unlink(doc), add = TRUE)
  shiny <- function(...) {
    writeLines(c(...), doc)
    is_shiny_document(doc)
  }

  expect_true(shiny("---", "runtime: shiny", "---"))
  expect_true(shiny("---", "runtime: shinyrmd", "---"))
  expect_true(shiny("---", "server: shiny", "---"))
  expect_true(shiny("---", "server:", "  type: shiny", "---"))
  expect_false(shiny("---", "title: Static", "---"))
  expect_false(shiny("---", "runtime: static", "server: other", "---"))
  expect_false(shiny("No front matter"))
  # Front matter that cannot be read counts as none
  expect_false(shiny("---", "title: [unclosed", "---"))
  expect_false(shiny("---", "just a string", "---"))
  expect_false(shiny("---", "runtime: [shiny, other]", "---"))
})

test_that("detect_app_type explains why a directory cannot be served", {
  missing <- file.path(tempdir(), "tss-no-such-app")
  expect_error(detect_app_type(missing), "App directory not found: .*tss-no-such-app")

  empty <- tempfile("tss-empty")
  dir.create(empty)
  on.exit(unlink(empty, recursive = TRUE), add = TRUE)
  writeLines("", file.path(empty, "README.md"))
  expect_error(detect_app_type(empty), "No app.R, ui.R and server.R, .Rmd, or .qmd file found in")
})

test_that("an app whose directory is missing starts no process", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "ghost", path = file.path(tempdir(), "tss-no-such-app"),
    port = 1, resident = FALSE)), log_dir = tempdir())
  local_mocked_bindings(r_bg = function(...) stop("No process may be started"), .package = "callr")
  pm <- ProcessManager$new(config)

  expect_error(pm$start_app(config$config$apps[[1]]), "App directory not found")
  expect_null(config$get_app_process("ghost"))
  expect_false(config$is_app_starting("ghost"))
  # A visitor's request reports the failure instead of raising it
  expect_false(pm$start_app_on_demand("ghost"))
})

test_that("a Quarto document is rendered only when its page is out of date", {
  app_dir <- tempfile("tss-qmd")
  dir.create(file.path(app_dir, "doc_files", "libs"), recursive = TRUE)
  dir.create(file.path(app_dir, "data"))
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  html <- file.path(app_dir, "doc.html")
  data <- file.path(app_dir, "data", "sales.csv")
  generated <- file.path(app_dir, "doc_files", "libs", "lib.js")
  for (file in c(qmd, data, generated)) writeLines("", file)
  now <- Sys.time()
  age <- function(file, seconds) Sys.setFileTime(file, now - seconds)

  # Never rendered
  expect_true(quarto_needs_render(qmd))

  writeLines("", html)
  age(qmd, 300)
  age(data, 300)
  age(html, 200)
  # What rendering writes is newer than the page without making it stale
  age(generated, 100)
  expect_false(quarto_needs_render(qmd))

  age(qmd, 100)
  expect_true(quarto_needs_render(qmd))
  age(qmd, 300)
  expect_false(quarto_needs_render(qmd))

  # A file the document reads has changed
  age(data, 100)
  expect_true(quarto_needs_render(qmd))

  # A change in the same instant as the page may have come after it
  age(data, 200)
  expect_true(quarto_needs_render(qmd))
})

test_that("a Quarto document with an upper-case extension finds its rendered page", {
  app_dir <- tempfile("tss-qmd-case")
  dir.create(file.path(app_dir, "doc_files"), recursive = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.QMD")
  html <- file.path(app_dir, "doc.html")
  generated <- file.path(app_dir, "doc_files", "lib.js")
  for (file in c(qmd, generated)) writeLines("", file)
  now <- Sys.time()

  expect_true(quarto_needs_render(qmd))

  # Quarto writes doc.html and doc_files for doc.QMD
  writeLines("", html)
  Sys.setFileTime(qmd, now - 300)
  Sys.setFileTime(html, now - 200)
  Sys.setFileTime(generated, now - 100)
  expect_false(quarto_needs_render(qmd))
})

test_that("the server's logs in an app directory do not make its Quarto page stale", {
  # A rendered page, older than the logs written since
  rendered_app <- function() {
    app_dir <- tempfile("tss-qmd-logs")
    dir.create(file.path(app_dir, "logs"), recursive = TRUE)
    qmd <- file.path(app_dir, "doc.qmd")
    for (file in c(qmd, file.path(app_dir, "doc.html"))) writeLines("", file)
    Sys.setFileTime(qmd, Sys.time() - 300)
    Sys.setFileTime(file.path(app_dir, "doc.html"), Sys.time() - 200)
    app_dir
  }

  app_dir <- rendered_app()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  for (name in c("server.log", "app_output.log", "app_error.prev.log")) {
    writeLines("", file.path(app_dir, "logs", name))
  }
  expect_false(quarto_needs_render(qmd, file.path(app_dir, "logs")))
  # Without log_dir, the logs count as sources
  expect_true(quarto_needs_render(qmd))
  # log_dir as the configuration spells it, relative and with a trailing slash
  old_dir <- setwd(app_dir)
  on.exit(setwd(old_dir), add = TRUE, after = FALSE)
  expect_false(quarto_needs_render(qmd, "logs/"))
  # Only the .log files directly in log_dir are the server's
  writeLines("", file.path(app_dir, "logs", "notes.csv"))
  expect_true(quarto_needs_render(qmd, "logs"))
  unlink(file.path(app_dir, "logs", "notes.csv"))
  dir.create(file.path(app_dir, "logs", "archive"))
  writeLines("", file.path(app_dir, "logs", "archive", "old.log"))
  expect_true(quarto_needs_render(qmd, "logs"))

  # log_dir is the app directory itself
  same_dir <- rendered_app()
  on.exit(unlink(same_dir, recursive = TRUE), add = TRUE)
  writeLines("", file.path(same_dir, "server.log"))
  expect_false(quarto_needs_render(file.path(same_dir, "doc.qmd"), same_dir))
  writeLines("", file.path(same_dir, "logs", "server.log"))
  expect_true(quarto_needs_render(file.path(same_dir, "doc.qmd"), same_dir))

  # A log_dir elsewhere leaves the app directory's files alone
  elsewhere <- rendered_app()
  on.exit(unlink(elsewhere, recursive = TRUE), add = TRUE)
  writeLines("", file.path(elsewhere, "notes.log"))
  expect_true(quarto_needs_render(file.path(elsewhere, "doc.qmd"), tempdir()))
})

test_that("start_app leaves the Quarto render decision to the app's own process", {
  app_dir <- tempfile("tss-qmd")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  writeLines("", qmd)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = 1, resident = TRUE)),
    log_dir = tempdir())
  launched <- NULL
  local_mocked_bindings(r_bg = function(func, args, ...) {
    launched <<- args
    list(is_alive = function() TRUE)
  }, .package = "callr")
  pm <- ProcessManager$new(config)
  start <- function() {
    pm$start_app(config$config$apps[[1]])
    # Retire the fake process so its scheduled readiness check does nothing
    config$remove_app_process("app")
  }

  # The check reads every file of the app directory, so it must not run here,
  # where it would hold up every app's traffic
  checks <- 0
  local_mocked_bindings(quarto_needs_render = function(qmd_file) {
    checks <<- checks + 1
    TRUE
  })
  start()
  expect_equal(checks, 0)
  expect_equal(launched$type, "qmd")
  expect_equal(basename(launched$file), "doc.qmd")
  # The app's process need not load this package to run the check
  expect_identical(environment(launched$needs_render), baseenv())
})

test_that("an app's own process can decide whether its Quarto document needs rendering", {
  skip_on_cran() # Starts an R process
  app_dir <- tempfile("tss-qmd")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  writeLines("", qmd)
  needs_render <- quarto_needs_render
  environment(needs_render) <- baseenv()
  in_app_process <- function() {
    callr::r(function(needs_render, file) needs_render(file), args = list(needs_render = needs_render, file = qmd))
  }
  expect_true(in_app_process())

  writeLines("", file.path(app_dir, "doc.html"))
  Sys.setFileTime(qmd, Sys.time() - 300)
  expect_false(in_app_process())
})

# ============================================================================
# App process launch
# ============================================================================

test_that("start_app recreates a log_dir removed while the server runs", {
  log_dir <- tempfile("tss-logs")
  app_dir <- make_test_app_dir()
  on.exit(unlink(c(log_dir, app_dir), recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = 1, resident = TRUE)),
    log_dir = log_dir)
  local_mocked_bindings(r_bg = function(...) list(is_alive = function() TRUE), .package = "callr")
  ProcessManager$new(config)$start_app(config$config$apps[[1]])
  # Retire the fake process so its scheduled readiness check does nothing
  config$remove_app_process("app")
  expect_true(dir.exists(log_dir))
})

test_that("start_app supervises app processes", {
  app_dir <- make_test_app_dir()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = 1, resident = TRUE)),
    log_dir = tempdir())
  args <- NULL
  local_mocked_bindings(r_bg = function(...) {
    args <<- list(...)
    list(is_alive = function() TRUE)
  }, .package = "callr")
  ProcessManager$new(config)$start_app(config$config$apps[[1]])
  # Retire the fake process so its scheduled readiness check does nothing
  config$remove_app_process("app")
  expect_true(args$supervise)
})

test_that("start_app starts an app process in the app's directory", {
  app_dir <- make_test_app_dir()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = 1, resident = TRUE)),
    log_dir = tempdir())
  args <- NULL
  local_mocked_bindings(r_bg = function(...) {
    args <<- list(...)
    list(is_alive = function() TRUE)
  }, .package = "callr")
  ProcessManager$new(config)$start_app(config$config$apps[[1]])
  config$remove_app_process("app")
  expect_identical(args$wd, normalizePath(app_dir))
  # callr would read the .Rprofile and .Renviron of the server's working directory
  expect_false(args$user_profile)
  expect_identical(args$env[["R_ENVIRON_USER"]], app_environ_file(normalizePath(app_dir)))
  expect_true(all(names(callr::rcmd_safe_env()) %in% names(args$env)))
  # The app's process need not load this package to find its profile
  expect_identical(environment(args$args$profile_file), baseenv())
  # Paths stay valid from the app's directory
  expect_identical(args$stdout, file.path(normalizePath(tempdir()), "app_output.log"))
  expect_identical(args$args$log_dir, normalizePath(tempdir()))
})

test_that("an app reads the environment file that R reads in its directory", {
  app_dir <- tempfile("tss-environ-app")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  old <- Sys.getenv("R_ENVIRON_USER", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("R_ENVIRON_USER") else Sys.setenv(R_ENVIRON_USER = old), add = TRUE)
  Sys.unsetenv("R_ENVIRON_USER")

  # Without a file of its own, the user's
  expect_identical(app_environ_file(app_dir), path.expand("~/.Renviron"))
  writeLines("TSS_TEST=1", file.path(app_dir, ".Renviron"))
  expect_identical(app_environ_file(app_dir), file.path(app_dir, ".Renviron"))
  # As at R's startup, a file that R_ENVIRON_USER names takes the place of both
  Sys.setenv(R_ENVIRON_USER = file.path(tempdir(), "shared.Renviron"))
  expect_identical(app_environ_file(app_dir), file.path(tempdir(), "shared.Renviron"))
})

test_that("an app runs the profile that R runs in its directory", {
  dir <- tempfile("tss-profile-dir")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  old_dir <- setwd(dir)
  on.exit(setwd(old_dir), add = TRUE, after = FALSE)
  old <- Sys.getenv("R_PROFILE_USER", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("R_PROFILE_USER") else Sys.setenv(R_PROFILE_USER = old), add = TRUE)
  Sys.unsetenv("R_PROFILE_USER")

  # Without a profile of its own, the user's
  expect_identical(startup_profile_file(), path.expand("~/.Rprofile"))
  writeLines("", ".Rprofile")
  expect_identical(startup_profile_file(), ".Rprofile")
  # As at R's startup, a file that R_PROFILE_USER names takes the place of both
  Sys.setenv(R_PROFILE_USER = "~/shared.Rprofile")
  expect_identical(startup_profile_file(), path.expand("~/shared.Rprofile"))
})

test_that("an app process reads its own .Rprofile and .Renviron, not the server's", {
  skip_on_cran() # Starts an R session that loads shiny
  server_dir <- tempfile("tss-server-dir")
  app_dir <- tempfile("tss-profile-app")
  dir.create(server_dir)
  dir.create(app_dir)
  on.exit(unlink(c(server_dir, app_dir), recursive = TRUE), add = TRUE)
  old_profile <- Sys.getenv("R_PROFILE_USER", unset = NA)
  old_environ <- Sys.getenv("R_ENVIRON_USER", unset = NA)
  on.exit({
    if (is.na(old_profile)) Sys.unsetenv("R_PROFILE_USER") else Sys.setenv(R_PROFILE_USER = old_profile)
    if (is.na(old_environ)) Sys.unsetenv("R_ENVIRON_USER") else Sys.setenv(R_ENVIRON_USER = old_environ)
  }, add = TRUE)
  Sys.unsetenv(c("R_PROFILE_USER", "R_ENVIRON_USER"))
  marker <- file.path(app_dir, "profiles")
  writeLines("options(tss.test.profile = 'server')", file.path(server_dir, ".Rprofile"))
  writeLines("options(tss.test.profile = 'app')", file.path(app_dir, ".Rprofile"))
  writeLines("TSS_TEST_ENVIRON=server", file.path(server_dir, ".Renviron"))
  writeLines("TSS_TEST_ENVIRON=app", file.path(app_dir, ".Renviron"))
  writeLines(c(
    sprintf("writeLines(c(getOption('tss.test.profile', 'none'), Sys.getenv('TSS_TEST_ENVIRON', 'none')), %s)",
      deparse(marker)),
    "Sys.sleep(60)"
  ), file.path(app_dir, "app.R"))

  # A relative log_dir is still found from the app's directory
  old_dir <- setwd(server_dir)
  on.exit(setwd(old_dir), add = TRUE, after = FALSE)
  config <- ShinyServerConfig$new()
  on.exit(stop_test_app_processes(config), add = TRUE, after = FALSE)
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = 1, resident = TRUE)),
    log_dir = "logs")
  ProcessManager$new(config)$start_app(config$config$apps[[1]])

  read_marker <- function() {
    deadline <- Sys.time() + 30
    while (length(if (file.exists(marker)) readLines(marker)) < 2 && Sys.time() < deadline) Sys.sleep(0.1)
    readLines(marker)
  }
  restart <- function() {
    stop_test_app_processes(config)
    unlink(marker)
    ProcessManager$new(config)$start_app(config$config$apps[[1]])
  }
  expect_equal(read_marker(), c("app", "app"))
  expect_true(file.exists(file.path(server_dir, "logs", "app_error.log")))

  # An error in the profile is reported, and the app still starts
  writeLines("stop('broken profile')", file.path(app_dir, ".Rprofile"))
  restart()
  expect_equal(read_marker(), c("none", "app"))
  expect_match(paste(readLines(file.path(server_dir, "logs", "app_error.log")), collapse = "\n"),
    "broken profile", fixed = TRUE)

  # Without a profile of its own, the app runs the user's, as R does
  unlink(file.path(app_dir, ".Rprofile"))
  user_profile <- file.path(server_dir, "user.Rprofile")
  writeLines("options(tss.test.profile = 'user')", user_profile)
  Sys.setenv(R_PROFILE_USER = user_profile)
  restart()
  expect_equal(read_marker(), c("user", "app"))
})

test_that("app output from child processes reaches the log without blocking", {
  # Starts an R session that loads shiny and runs a further Rscript: how long
  # that takes on a loaded check machine is not this package's to bound
  skip_on_cran()
  app_dir <- tempfile("tss-output-app")
  log_dir <- tempfile("tss-output-logs")
  dir.create(app_dir)
  dir.create(log_dir)
  on.exit(unlink(c(app_dir, log_dir), recursive = TRUE), add = TRUE)
  marker <- file.path(app_dir, "done")
  writeLines("cat(strrep('x', 200000))", file.path(app_dir, "writer.R"))
  writeLines(c(
    sprintf("system2(%s, %s)", deparse(file.path(R.home("bin"), "Rscript")),
      deparse(shQuote(file.path(app_dir, "writer.R")))),
    sprintf("writeLines('done', %s)", deparse(marker)),
    "Sys.sleep(60)"
  ), file.path(app_dir, "app.R"))
  writeLines("previous run", file.path(log_dir, "app_output.log"))

  config <- ShinyServerConfig$new()
  on.exit(stop_test_app_processes(config), add = TRUE, after = FALSE)
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = 1, resident = TRUE)),
    log_dir = log_dir)
  pm <- ProcessManager$new(config)
  pm$start_app(config$config$apps[[1]])

  deadline <- Sys.time() + 30
  while (!file.exists(marker) && Sys.time() < deadline) Sys.sleep(0.1)
  expect_true(file.exists(marker))
  output <- paste(readLines(file.path(log_dir, "app_output.log"), warn = FALSE), collapse = "")
  expect_gte(nchar(output), 200000)
  expect_equal(readLines(file.path(log_dir, "app_output.prev.log")), "previous run")
})

# ============================================================================
# Readiness callbacks and on-demand starts
# ============================================================================

test_that("old readiness callbacks cannot remove or mark replacements ready", {
  config <- ShinyServerConfig$new()
  old <- list(is_alive = function() FALSE)
  replacement <- list(is_alive = function() TRUE)
  config$add_app_process("app", replacement)
  config$set_app_starting("app")
  pm <- ProcessManager$new(config)
  expect_false(pm$check_app_ready("app", 3001, old))
  expect_identical(config$get_app_process("app"), replacement)
  expect_true(config$is_app_starting("app"))

  old <- list(is_alive = function() TRUE, generation = 1)
  config$add_app_process("app", old)
  resolve_probe <- NULL
  local_mocked_bindings(wait_for_backend = function(...) promises::promise(function(resolve, reject) {
    resolve_probe <<- resolve
  }))
  result <- pm$check_app_ready("app", 3001, old)
  config$add_app_process("app", replacement)
  config$set_app_starting("app")
  resolve_probe(TRUE)
  expect_false(await_response(result))
  expect_identical(config$get_app_process("app"), replacement)
  expect_true(config$is_app_starting("app"))
  config$remove_app_process("app")
  expect_false(pm$check_app_ready("app", 3001, replacement))
})

test_that("removing a starting process allows the next on-demand launch", {
  for (removal in c("health", "stop")) {
    config <- ShinyServerConfig$new()
    config$config <- list(apps = list(list(name = "app", port = 3001,
      resident = FALSE, appstart_timeout = 60)))
    old <- list(is_alive = function() FALSE)
    config$add_app_process("app", old)
    config$set_app_starting("app")
    pm <- ProcessManager$new(config)
    starts <- 0
    replacement <- list(is_alive = function() TRUE)
    assign("start_app", function(app_config) {
      starts <<- starts + 1
      config$add_app_process("app", replacement)
      config$set_app_starting("app")
      TRUE
    }, envir = pm)

    switch(removal, health = pm$health_check(), stop = pm$stop_app("app"))
    expect_null(config$get_app_process("app"), info = removal)
    expect_false(config$is_app_starting("app"), info = removal)
    expect_false(pm$check_app_ready("app", 3001, old))
    expect_true(pm$start_app_on_demand("app"))
    expect_equal(starts, 1, info = removal)
    expect_identical(config$get_app_process("app"), replacement)
    # A delayed callback from the old process must not clear the new launch.
    expect_false(pm$check_app_ready("app", 3001, old))
    expect_true(config$is_app_starting("app"), info = removal)
  }
})

# ============================================================================
# Crash cleanup
# ============================================================================

test_that("crash cleanup terminates workers after their parent has exited", {
  # Starts two real R processes for each action; too slow and timing-dependent
  # for a loaded check machine
  skip_on_cran()
  check_cleanup <- function(action) {
    marker <- tempfile()
    pid_file <- tempfile()
    parent <- callr::r_bg(function(marker, pid_file) {
      child <- callr::r_bg(function(marker) {
        repeat {
          cat("tick\n", file = marker, append = TRUE)
          Sys.sleep(.02)
        }
      }, args = list(marker = marker), supervise = FALSE)
      writeLines(as.character(child$get_pid()), pid_file)
      Sys.sleep(30)
    }, args = list(marker = marker, pid_file = pid_file))
    on.exit({
      parent$kill_tree()
      if (file.exists(pid_file)) try(tools::pskill(as.integer(readLines(pid_file))), silent = TRUE)
      unlink(c(marker, pid_file))
    }, add = TRUE)
    deadline <- Sys.time() + 30
    while ((!file.exists(marker) || !file.exists(pid_file)) && Sys.time() < deadline) Sys.sleep(.02)
    expect_true(file.exists(marker), info = action)
    parent$kill()
    parent$wait()
    expect_false(parent$is_alive())
    config <- ShinyServerConfig$new()
    config$config <- list(apps = list(list(name = "app", resident = FALSE)), restart_delay = 0)
    config$add_app_process("app", parent)
    config$set_app_starting("app")
    pm <- ProcessManager$new(config)
    assign("start_app", function(...) TRUE, envir = pm)
    switch(action, health = pm$health_check(),
      readiness = pm$check_app_ready("app", 3001, parent), restart = pm$restart_app("app"),
      shutdown = pm$stop_all_apps(), on_demand = pm$start_app_on_demand("app"))
    expect_null(config$get_app_process("app"), info = action)
    Sys.sleep(.1)
    size <- file.info(marker)$size
    Sys.sleep(.15)
    expect_equal(file.info(marker)$size, size, info = action)
  }
  for (action in c("health", "readiness", "restart", "shutdown", "on_demand")) check_cleanup(action)
})

test_that("crash cleanup retains ownership if descendant cleanup fails", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  process <- list(is_alive = function() FALSE, kill_tree = function() stop("cleanup failed"))
  config$add_app_process("app", process)
  pm <- ProcessManager$new(config)
  pm$health_check()
  expect_identical(config$get_app_process("app"), process)
  expect_false(pm$check_app_ready("app", 3001, process))
  expect_identical(config$get_app_process("app"), process)
  expect_false(pm$restart_app("app")$success)
  expect_identical(config$get_app_process("app"), process)
  assign("start_app", function(...) stop("must retain failed process"), envir = pm)
  expect_false(pm$start_app_on_demand("app"))
  expect_identical(config$get_app_process("app"), process)
})

test_that("failed termination retains the process and prevents replacement", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = 0)
  process <- list(is_alive = function() TRUE)
  config$add_app_process("app", process)
  config$set_app_starting("app")
  local_mocked_bindings(kill_process_safely = function(process) FALSE)
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  expect_false(pm$stop_app("app")$success)
  expect_identical(config$get_app_process("app"), process)
  expect_true(config$is_app_starting("app"))
  expect_false(pm$restart_app("app")$success)
  expect_identical(config$get_app_process("app"), process)
  expect_true(config$is_app_starting("app"))
  expect_equal(starts, 0)
})

# ============================================================================
# Requested restarts
# ============================================================================

test_that("a requested restart does not wait for restart_delay", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = 60)
  alive <- TRUE
  config$add_app_process("app", list(is_alive = function() alive, kill_tree = function() alive <<- FALSE))
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  scheduled <- 0
  local_mocked_bindings(later = function(...) scheduled <<- scheduled + 1, .package = "later")

  result <- pm$restart_app("app")
  expect_true(result$success)
  expect_match(result$message, "restarted")
  expect_false(alive)
  expect_equal(starts, 1)
  expect_equal(scheduled, 0)
})

test_that("a requested restart replaces the one a failure had scheduled", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = 0.05)
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  pm$health_check()
  expect_equal(starts, 0)
  expect_true(pm$restart_app("app")$success)
  expect_equal(starts, 1)
  await_response(promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), .1)))
  expect_equal(starts, 1)
})

test_that("restarting an on-demand app stops it until it is next opened", {
  app_dir <- make_test_app_dir()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", path = app_dir, resident = FALSE)), restart_delay = 0)
  alive <- TRUE
  config$add_app_process("app", list(is_alive = function() alive, kill_tree = function() alive <<- FALSE))
  closed <- 0
  config$add_ws_connection("s", list(app_name = "app", ws = list(close = function() closed <<- closed + 1)))
  pm <- ProcessManager$new(config)
  assign("start_app", function(app_config) stop("An idle on-demand app must not be started"), envir = pm)

  result <- pm$restart_app("app")
  expect_true(result$success)
  expect_match(result$message, "starts again when it is next opened")
  expect_false(alive)
  expect_equal(closed, 1)
  expect_null(config$get_app_process("app"))
  expect_equal(pm$get_app_status("app")$status, "dormant")
})

test_that("a restart that cannot start the app reports the reason", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE,
    path = file.path(tempdir(), "tss-no-such-app"))), restart_delay = 0, log_dir = tempdir())
  result <- ProcessManager$new(config)$restart_app("app")
  expect_false(result$success)
  # The dashboard shows this message as it is, so it names the app
  expect_match(result$message, "^Failed to restart app app: App directory not found")
  expect_null(config$get_app_process("app"))
})

# ============================================================================
# Health checks and restart delays
# ============================================================================

test_that("restart delays yield and are cancelled by stop and shutdown", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = 0.05)
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  tick <- FALSE
  later::later(function() tick <<- TRUE, 0)
  # A resident app without a process is restarted by the health check
  pm$health_check()
  expect_equal(starts, 0)
  later::run_now(0)
  expect_true(tick)
  pm$health_check()
  expect_equal(starts, 0)
  await_response(promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), .1)))
  expect_equal(starts, 1)

  pm$health_check()
  pm$stop_app("app")
  await_response(promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), .1)))
  expect_equal(starts, 1)
  pm$health_check()
  pm$stop_all_apps()
  await_response(promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), .1)))
  expect_equal(starts, 1)
})

test_that("health checks delay crashed resident app restarts without duplicates", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = .05)
  config$add_app_process("app", list(is_alive = function() FALSE))
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  pm$health_check()
  pm$health_check()
  expect_equal(starts, 0)
  await_response(promises::promise(function(resolve, reject) later::later(function() resolve(TRUE), .1)))
  expect_equal(starts, 1)
})

test_that("health checks isolate startup errors and retry failed apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = lapply(c("bad", "next", "healthy"), function(name) list(name = name, resident = TRUE)),
    restart_delay = 0)
  healthy <- list(is_alive = function() TRUE)
  config$add_app_process("healthy", healthy)
  pm <- ProcessManager$new(config)
  attempted <- character()
  fail <- TRUE
  assign("start_app", function(app_config) {
    attempted <<- c(attempted, app_config$name)
    if (app_config$name == "bad" && fail) stop("Resource temporarily unavailable")
    config$add_app_process(app_config$name, list(is_alive = function() TRUE))
    TRUE
  }, envir = pm)
  expect_no_error(pm$health_check())
  expect_equal(attempted, c("bad", "next"))
  expect_identical(config$get_app_process("healthy"), healthy)
  fail <- FALSE
  pm$health_check()
  expect_equal(attempted, c("bad", "next", "bad"))
  expect_true(config$get_app_process("bad")$is_alive())
})

test_that("health checks restart a missing resident app only after restart_delay", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = 60)
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) {
    starts <<- starts + 1
    TRUE
  }, envir = pm)
  scheduled <- list()
  local_mocked_bindings(later = function(func, delay = 0, ...) {
    scheduled[[length(scheduled) + 1]] <<- list(func = func, delay = delay)
  }, .package = "later")
  pm$health_check()
  pm$health_check()
  expect_equal(starts, 0)
  # Later health checks keep the pending restart instead of postponing it
  expect_length(scheduled, 1)
  expect_equal(scheduled[[1]]$delay, 60)
  scheduled[[1]]$func()
  expect_equal(starts, 1)
})

# ============================================================================
# Idle session cleanup
# ============================================================================

test_that("stale cleanup closes sockets and stops idle on-demand apps once", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  pm <- ProcessManager$new(config)
  killed <- 0
  config$add_app_process("app", list(is_alive = function() TRUE))
  local_mocked_bindings(kill_process_safely = function(process) { killed <<- killed + 1; TRUE })
  callbacks <- list()
  local_mocked_bindings(later = function(func, delay, ...) {
    callbacks[[length(callbacks) + 1L]] <<- func
  }, .package = "later")
  cm <- ConnectionManager$new(config, pm)
  client_closed <- 0
  backend_closed <- 0
  config$add_ws_connection("s", list(app_name = "app", last_activity = Sys.time() - 3600,
    ws = list(close = function() {
      client_closed <<- client_closed + 1
      cm$remove_client_connection("s")
    })))
  config$add_backend_connection("s", list(app_name = "app", last_activity = Sys.time(),
    ws = list(close = function() { backend_closed <<- backend_closed + 1 })))
  expect_equal(pm$cleanup_stale_connections(), 1)
  expect_equal(client_closed, 1)
  expect_equal(backend_closed, 1)
  # The idle app stops once the reconnect grace period passes
  expect_equal(killed, 0)
  expect_length(callbacks, 1)
  callbacks[[1]]()
  expect_equal(killed, 1)
  expect_null(config$get_ws_connection("s"))
  expect_null(config$get_backend_connection("s"))
  expect_equal(config$get_app_connection_count("app"), 0)
  cm$close_client_connection("s")
  expect_equal(killed, 1)
  expect_equal(client_closed, 1)
})

test_that("stale backends also close fresh client sockets", {
  config <- ShinyServerConfig$new()
  closed <- 0
  config$add_ws_connection("s", list(app_name = "app", last_activity = Sys.time(),
    ws = list(close = function() {closed <<- closed + 1})))
  config$add_backend_connection("s", list(app_name = "app", last_activity = Sys.time() - 3600,
    ws = list(close = function() NULL)))
  expect_equal(ProcessManager$new(config)$cleanup_stale_connections(), 1)
  expect_equal(closed, 1)
  expect_null(config$get_ws_connection("s"))
  expect_null(config$get_backend_connection("s"))
})
