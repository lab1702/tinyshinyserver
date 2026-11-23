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

  # A finding from a time after now, as a clock set back leaves it, is not
  # used for as long as the clock was set back
  config$START_PROBLEM_MAX_AGE_SECONDS <- 30
  finding <- config$start_problems$ondemand
  finding$read_at <- Sys.time() + 3600
  assign("ondemand", finding, envir = config$start_problems)
  expect_equal(config$cached_start_problem(app), "No app.R found")
  expect_length(entered, 3)
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

test_that("an on-demand app that the idle check finds dead waits restart_delay", {
  # A crash closes the app's sessions, and the idle check that follows can
  # come before the health check that would record the failure
  config <- ShinyServerConfig$new()
  config$config <- list(restart_delay = 60, apps = list(
    list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
  ))
  config$add_app_process("app1", list(get_pid = function() 12345, is_alive = function() FALSE))
  cleaned <- 0
  pm <- ProcessManager$new(config)
  assign("cleanup_app_connections", function(app_name) cleaned <<- cleaned + 1, envir = pm)
  local_mocked_bindings(kill_process_safely = function(process) TRUE)

  expect_true(pm$stop_app_immediately("app1"))
  expect_null(config$get_app_process("app1"))
  expect_equal(cleaned, 1)
  expect_gt(config$start_backoff_seconds("app1"), 59)
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
  temp_app_dir <- make_test_app_dir()
  log_dir <- tempfile("tss-logs")
  on.exit(unlink(c(temp_app_dir, log_dir), recursive = TRUE), add = TRUE)

  config <- ShinyServerConfig$new()
  # Retire the new fake process, so that its scheduled readiness check does nothing
  on.exit(stop_test_app_processes(config), add = TRUE)
  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = TRUE)
    ),
    log_dir = log_dir,
    restart_delay = 0 # No delay for testing
  )
  # The new process is not needed to see the restart begin
  local_mocked_bindings(r_bg = function(...) list(is_alive = function() TRUE), .package = "callr")

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
  app_dir <- make_test_app_dir()
  log_dir <- tempfile("tss-logs")
  on.exit(unlink(c(app_dir, log_dir), recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app1", path = app_dir, port = 3001, resident = FALSE)),
    log_dir = log_dir)
  pm <- ProcessManager$new(config)
  local_mocked_bindings(r_bg = function(...) stop("cannot start processx process"), .package = "callr")
  logged <- capture_test_log(expect_false(pm$start_app_on_demand("app1")))
  # The process failed to start, not the check of the app's directory
  expect_true(any(grepl("Error starting app app1: cannot start processx process", logged, fixed = TRUE)))
  expect_null(config$get_app_process("app1"))
})

test_that("start_app_on_demand attempts to start app if not running", {
  temp_app_dir <- make_test_app_dir()
  log_dir <- tempfile("tss-logs")
  on.exit(unlink(c(temp_app_dir, log_dir), recursive = TRUE), add = TRUE)

  config <- ShinyServerConfig$new()
  # Retire the fake process, so that its scheduled readiness check does nothing
  on.exit(stop_test_app_processes(config), add = TRUE)
  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = FALSE)
    ),
    log_dir = log_dir
  )
  # The app's process is not needed to see its start begin
  local_mocked_bindings(r_bg = function(...) list(is_alive = function() TRUE), .package = "callr")

  pm <- ProcessManager$new(config)

  # The actual start_app will be called - we verify it marks app as starting
  # and returns TRUE (indicating start was initiated)
  result <- pm$start_app_on_demand("app1")

  expect_true(result)
  # App should be marked as starting (async startup)
  expect_true(config$is_app_starting("app1"))
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
  temp_app_dir <- make_test_app_dir()
  log_dir <- tempfile("tss-logs")
  on.exit(unlink(c(temp_app_dir, log_dir), recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  # Retire the restarted fake process, so that its scheduled readiness check does nothing
  on.exit(stop_test_app_processes(config), add = TRUE)

  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = TRUE)
    ),
    log_dir = log_dir,
    restart_delay = 0
  )
  # The restarted process is not needed to see the cleanup
  local_mocked_bindings(r_bg = function(...) list(is_alive = function() TRUE), .package = "callr")

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
  temp_app_dir <- make_test_app_dir()
  log_dir <- tempfile("tss-logs")
  on.exit(unlink(c(temp_app_dir, log_dir), recursive = TRUE), add = TRUE)

  config <- ShinyServerConfig$new()
  # Retire the fake process, so that its scheduled readiness check does nothing
  on.exit(stop_test_app_processes(config), add = TRUE)
  config$config <- list(
    apps = list(
      list(name = "app1", path = temp_app_dir, port = 3001, resident = TRUE)
    ),
    log_dir = log_dir,
    restart_delay = 0
  )
  # The app's process is not needed to see its start begin
  local_mocked_bindings(r_bg = function(...) list(is_alive = function() TRUE), .package = "callr")

  pm <- ProcessManager$new(config)

  # Initially no process
  expect_null(config$get_app_process("app1"))

  pm$health_check()

  # After health_check, resident app should be starting
  expect_true(config$is_app_starting("app1"))
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
    wait_for_backend = function(url, wait_seconds = 0, ...) promises::promise_resolve(TRUE)
  )

  result <- await_response(pm$check_app_ready("app1", 3001, live_process))

  expect_true(result)
  # Startup state should be cleared (app is ready)
  expect_false(config$is_app_starting("app1"))
})

test_that("check_app_ready returns FALSE once the startup window has passed", {
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
    wait_for_backend = function(url, wait_seconds = 0, ...) promises::promise_resolve(FALSE)
  )

  # The app started longer ago than the startup window
  started_at <- Sys.time() - config$APP_STARTUP_TIMEOUT_SECONDS - 1
  result <- await_response(pm$check_app_ready("app1", 3001, live_process, started_at = started_at))

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
    wait_for_backend = function(url, wait_seconds = 0, ...) promises::promise_resolve(FALSE)
  )
  later::with_loop(later::create_loop(), {
    expect_false(await_response(pm$check_app_ready("app1", 3001, list(), started_at = Sys.time() - 31)))
    expect_true(config$is_app_starting("app1"))

    expect_false(await_response(pm$check_app_ready("app1", 3001, list(), started_at = Sys.time() - 41)))
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
    wait_for_backend = function(url, wait_seconds = 0, ...) promises::promise_resolve(FALSE)
  )
  window <- config$APP_STARTUP_TIMEOUT_SECONDS
  later::with_loop(later::create_loop(), {
    # A slow app (e.g. Quarto rendering) is still starting after 5 seconds
    expect_false(await_response(pm$check_app_ready("app1", 3001, list(), started_at = Sys.time() - 5)))
    expect_true(config$is_app_starting("app1"))

    expect_false(await_response(pm$check_app_ready("app1", 3001, list(), started_at = Sys.time() - window - 1)))
    expect_false(config$is_app_starting("app1"))
  })
})

test_that("a clock set back during a startup starts its readiness window again", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app1")))
  config$set_app_starting("app1")
  pm <- ProcessManager$new(config)
  config$add_app_process("app1", list())
  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    wait_for_backend = function(url, wait_seconds = 0, ...) promises::promise_resolve(FALSE)
  )
  # The next check is kept, not run; promises still run their callbacks
  rechecks <- list()
  real_later <- later::later
  local_mocked_bindings(later = function(func, delay = 0, ...) {
    if (identical(delay, 0.5)) {
      rechecks[[length(rechecks) + 1]] <<- func
      return(invisible(function() FALSE))
    }
    real_later(func, delay, ...)
  }, .package = "later")

  # A start recorded an hour from now, as a clock set back an hour leaves it,
  # would otherwise keep the window open for that hour
  expect_false(await_response(pm$check_app_ready("app1", 3001, list(), started_at = Sys.time() + 3600)))
  expect_true(config$is_app_starting("app1"))
  expect_length(rechecks, 1)
  started_at <- get("started_at", envir = environment(rechecks[[1]]))
  expect_lt(abs(as.numeric(difftime(started_at, Sys.time(), units = "secs"))), 5)
})

test_that("the readiness window is measured in time, not in probes", {
  # A probe of a port that nothing listens on can take a second, as on
  # Windows, so counting probes would stretch the window
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app1")))
  config$set_app_starting("app1")
  pm <- ProcessManager$new(config)
  config$add_app_process("app1", list())
  local_mocked_bindings(
    is_process_alive = function(process) TRUE,
    wait_for_backend = function(url, wait_seconds = 0, ...) promises::promise_resolve(FALSE)
  )
  started_at <- Sys.time() - config$APP_STARTUP_TIMEOUT_SECONDS - 1
  later::with_loop(later::create_loop(), {
    # Only the second probe, long before 60 half-second attempts
    expect_false(await_response(pm$check_app_ready("app1", 3001, list(), attempt = 2, started_at = started_at)))
    expect_equal(config$get_app_startup_state("app1")$state, "timeout")
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
    wait_for_backend = function(url, wait_seconds = 0, ...) promises::promise_resolve(ready)
  )
  after_window <- Sys.time() - config$APP_STARTUP_TIMEOUT_SECONDS - 1
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
      started_at = after_window))))
    expect_equal(config$get_app_startup_state("app1")$state, "timeout")
    expect_match(paste(log, collapse = "\n"), "still waiting for it to start", fixed = TRUE)
    expect_no_match(paste(log, collapse = "\n"), "failed to become ready", fixed = TRUE)
    expect_true(check_scheduled())

    # Later checks do not report the timeout again
    log <- capture_test_log(expect_false(await_response(pm$check_app_ready("app1", 3001, process,
      started_at = after_window, timed_out = TRUE))))
    expect_no_match(paste(log, collapse = "\n"), "still waiting for it to start", fixed = TRUE)

    # Once it listens, it is ready
    ready <- TRUE
    log <- capture_test_log(expect_true(await_response(pm$check_app_ready("app1", 3001, process,
      started_at = after_window, timed_out = TRUE))))
    expect_null(config$get_app_startup_state("app1"))
    expect_match(paste(log, collapse = "\n"), "App app1 is ready", fixed = TRUE)
  })

  # An app that has not listened for as long as an app may keep starting is given up on
  ready <- FALSE
  started <- Sys.time() - config$ON_DEMAND_START_LIMIT_SECONDS
  later::with_loop(later::create_loop(), {
    log <- capture_test_log(expect_false(await_response(pm$check_app_ready("app1", 3001, process,
      started_at = started, timed_out = TRUE))))
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

test_that("hidden files and renv's packages do not make a Quarto page stale", {
  app_dir <- tempfile("tss-qmd-hidden")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  html <- file.path(app_dir, "doc.html")
  for (file in c(qmd, html)) writeLines("", file)
  Sys.setFileTime(qmd, Sys.time() - 300)
  Sys.setFileTime(html, Sys.time() - 200)
  add <- function(name) {
    path <- file.path(app_dir, name)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines("", path)
    path
  }

  # Version control, the IDE, and renv write these without the document changing
  for (name in c(".Rhistory", ".git/FETCH_HEAD", ".Rproj.user/shared/notebooks/state", "data/.DS_Store",
    "renv/library/R-4.6/x86_64/pkg/DESCRIPTION", "renv/staging/1/pkg/DESCRIPTION")) {
    add(name)
  }
  expect_false(quarto_needs_render(qmd))

  # The profile and environment of the rendering R session, and renv's own
  # files, are sources
  for (name in c(".Rprofile", ".Renviron", "renv.lock", "renv/activate.R", "_quarto.yml")) {
    path <- add(name)
    expect_true(quarto_needs_render(qmd), info = name)
    Sys.setFileTime(path, Sys.time() - 300)
  }
  expect_false(quarto_needs_render(qmd))
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
  options <- NULL
  local_mocked_bindings(r_bg = function(func, args, ...) {
    launched <<- args
    options <<- list(...)
    list(is_alive = function() TRUE)
  }, .package = "callr")
  local_mocked_bindings(quarto_path = function(...) "/opt/quarto/bin/quarto", .package = "quarto")
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
  # The app's process need not load this package to run the check, render
  # the document, or set its library paths
  for (helper in c("needs_render", "render_document", "environ_libraries", "profile_file")) {
    expect_identical(environment(launched[[helper]]), baseenv(), info = helper)
  }
  # It renders with the Quarto CLI that this session finds, and records the
  # render in log_dir
  expect_equal(launched$quarto_bin, "/opt/quarto/bin/quarto")
  expect_equal(basename(launched$state_file), "app_render.rds")
  # Without its own .Renviron, the app keeps this session's library paths,
  # whatever ~/.Renviron sets
  expect_equal(launched$library_file, "")
  writeLines("", file.path(app_dir, ".Renviron"))
  withCallingHandlers(start(), warning = function(w) invokeRestart("muffleWarning"))
  if (is.na(Sys.getenv("R_ENVIRON_USER", unset = NA))) {
    expect_equal(launched$library_file, file.path(normalizePath(app_dir), ".Renviron"))
  }
  # The site profile runs in apps, as at R's startup
  expect_true(options$system_profile)
  expect_false(options$user_profile)
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
  expect_identical(args$args$environ_file, app_environ_file(normalizePath(app_dir)))
  expect_true(all(names(callr::rcmd_safe_env()) %in% names(args$env)))
  # The app's process need not load this package to find its profile
  expect_identical(environment(args$args$profile_file), baseenv())
  # callr drops an empty R_PROFILE_USER, so the server's own setting is passed
  expect_identical(args$args$profile_setting, Sys.getenv("R_PROFILE_USER", unset = NA))
  # Paths stay valid from the app's directory
  expect_identical(args$stdout, file.path(normalizePath(tempdir()), "app_output.log"))
  expect_identical(args$args$log_dir, normalizePath(tempdir()))
  expect_identical(config$get_app_document("app"), list(type = "shiny"))
})

test_that("an R Markdown app serves the document that the server chose at its root", {
  app_dir <- tempfile("tss-rmd-app")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  writeLines(c("---", "runtime: shiny", "---"), file.path(app_dir, "report.Rmd"))
  # rmarkdown::run() would serve an index document of either kind first
  writeLines(c("---", "server: shiny", "---"), file.path(app_dir, "index.qmd"))
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

  expect_equal(args$args$type, "rmd")
  expect_equal(basename(args$args$file), "report.Rmd")
  expect_equal(config$get_app_document("app")$file, args$args$file)
  ran <- NULL
  local_mocked_bindings(run = function(...) ran <<- list(...), .package = "rmarkdown")
  do.call(args[[1]], utils::modifyList(args$args, list(profile_setting = "")))
  expect_equal(ran$default_file, "report.Rmd")
})

test_that("a log that cannot be renamed is copied to the previous run's log", {
  # Only Windows refuses to rename a file that a program holds open
  skip_if_not(.Platform$OS.type == "windows")
  app_dir <- make_test_app_dir()
  log_dir <- tempfile("tss-rotate-logs")
  dir.create(log_dir)
  on.exit(unlink(c(app_dir, log_dir), recursive = TRUE), add = TRUE)
  error_log <- file.path(log_dir, "app_error.log")
  writeLines("crash traceback", error_log)
  writeLines("older run", file.path(log_dir, "app_error.prev.log"))
  reader <- file(error_log, "r")
  on.exit(close(reader), add = TRUE, after = FALSE)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = 1, resident = TRUE)),
    log_dir = log_dir)
  local_mocked_bindings(r_bg = function(...) list(is_alive = function() TRUE), .package = "callr")
  ProcessManager$new(config)$start_app(config$config$apps[[1]])
  config$remove_app_process("app")

  expect_equal(readLines(file.path(log_dir, "app_error.prev.log")), "crash traceback")
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
  # Set but empty, it names no file: the app reads the null device. Windows
  # cannot hold an empty variable, so the value is passed in.
  expect_identical(app_environ_file(app_dir, named = ""), nullfile())
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
  # Set but empty, it names no profile, even with one in the directory
  expect_identical(startup_profile_file(named = ""), "")
})

test_that("an app whose environment file sets R_PROFILE_USER runs that profile and no other", {
  app_dir <- make_test_app_dir()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  saved <- Sys.getenv(c("R_ENVIRON_USER", "R_PROFILE_USER", "TSS_APP_PROFILE_USER"), unset = NA, names = TRUE)
  on.exit(for (name in names(saved)) {
    if (is.na(saved[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, structure(list(saved[[name]]), names = name))
  }, add = TRUE)
  Sys.unsetenv("R_ENVIRON_USER")
  writeLines("options(tss.test.profile = 'app')", file.path(app_dir, ".Rprofile"))
  writeLines("options(tss.test.profile = 'named')", file.path(app_dir, "named.Rprofile"))
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = 1, resident = TRUE)),
    log_dir = tempdir())
  args <- NULL
  local_mocked_bindings(r_bg = function(...) {
    args <<- list(...)
    list(is_alive = function() TRUE)
  }, .package = "callr")
  app_environ <- file.path(normalizePath(app_dir), ".Renviron")
  # The arguments that the app's process gets, and the profile that its
  # function then runs, here in the app's directory. R would have read the
  # environment file that the process is given; here only the profile that
  # it keeps is set.
  start <- function(environ, kept_profile = NA) {
    writeLines(environ, file.path(app_dir, ".Renviron"))
    ProcessManager$new(config)$start_app(config$config$apps[[1]])
    config$remove_app_process("app")
    if (is.na(kept_profile)) Sys.unsetenv("TSS_APP_PROFILE_USER") else Sys.setenv(TSS_APP_PROFILE_USER = kept_profile)
    old_dir <- setwd(app_dir)
    on.exit(setwd(old_dir), add = TRUE)
    old_option <- options(tss.test.profile = NULL)
    on.exit(options(old_option), add = TRUE)
    local_mocked_bindings(runApp = function(...) getOption("tss.test.profile", "none"), .package = "shiny")
    process_environ <- args$env[["R_ENVIRON_USER"]]
    list(chosen = args$args$profile_chosen, read = identical(process_environ, app_environ),
      ran = do.call(args[[1]], utils::modifyList(args$args, list(profile_setting = NA))))
  }

  expect_equal(start("TSS_TEST=1"), list(chosen = FALSE, read = TRUE, ran = "app"))
  expect_equal(start("# R_PROFILE_USER=named.Rprofile"), list(chosen = FALSE, read = TRUE, ran = "app"))
  # R reads the app's file through one that keeps callr's own profile, and
  # the app's process runs the profile that the app's file chose
  expect_equal(start("R_PROFILE_USER=named.Rprofile", "named.Rprofile"), list(chosen = TRUE, read = FALSE, ran = "named"))
  # Processes that the app starts see the app's own file and profile, and the
  # file that the process read is gone
  expect_identical(Sys.getenv("R_PROFILE_USER"), "named.Rprofile")
  expect_identical(Sys.getenv("R_ENVIRON_USER"), app_environ)
  expect_false(file.exists(args$env[["R_ENVIRON_USER"]]))
  expect_true(is.na(Sys.getenv("TSS_APP_PROFILE_USER", unset = NA)))
  expect_identical(environ_assigned_names(nullfile()), character())
  expect_identical(environ_assigned_names(file.path(app_dir, "missing")), character())
})

test_that("the environment file that keeps callr's profile applies the app's file", {
  app_environ <- tempfile(fileext = ".Renviron")
  wrapper <- tempfile(fileext = ".Renviron")
  on.exit(unlink(c(app_environ, wrapper)), add = TRUE)
  names <- c("R_PROFILE_USER", "TSS_APP_PROFILE_USER", "TSS_CALLR_PROFILE_USER", "TSS_TEST_APP_VAR")
  saved <- Sys.getenv(names, unset = NA, names = TRUE)
  on.exit(for (name in names(saved)) {
    if (is.na(saved[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, structure(list(saved[[name]]), names = name))
  }, add = TRUE)
  # Without a newline at its end, as an editor may save it
  cat('TSS_TEST_APP_VAR=from app\nR_PROFILE_USER="C:\\app\\named.Rprofile"', file = app_environ)
  expect_identical(profile_keeping_environ_file(app_environ, wrapper), wrapper)

  # callr names its own profile before R reads the user environment file
  callr_profile <- file.path(tempdir(), "callr-upr-test")
  Sys.setenv(R_PROFILE_USER = callr_profile)
  readRenviron(wrapper)
  expect_identical(Sys.getenv("R_PROFILE_USER"), callr_profile)
  expect_identical(Sys.getenv("TSS_APP_PROFILE_USER"), "C:\\app\\named.Rprofile")
  expect_identical(Sys.getenv("TSS_TEST_APP_VAR"), "from app")
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

  # A profile that the app's .Renviron names is the one that runs, as at R's
  # startup: the app's .Rprofile does not run after it
  Sys.unsetenv("R_PROFILE_USER")
  writeLines("options(tss.test.profile = 'app')", file.path(app_dir, ".Rprofile"))
  writeLines("options(tss.test.profile = 'named')", file.path(app_dir, "named.Rprofile"))
  writeLines(c("TSS_TEST_ENVIRON=app", "R_PROFILE_USER=named.Rprofile"), file.path(app_dir, ".Renviron"))
  restart()
  expect_equal(read_marker(), c("named", "app"))

  # callr still finishes setting up such an app's process, so an error that
  # ends the app reaches its error log
  writeLines("stop('the app failed')", file.path(app_dir, "app.R"))
  stop_test_app_processes(config)
  ProcessManager$new(config)$start_app(config$config$apps[[1]])
  config$get_app_process("app")$wait(30000)
  expect_match(paste(readLines(file.path(server_dir, "logs", "app_error.log")), collapse = "\n"),
    "the app failed", fixed = TRUE)
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
    # A failed process is replaced only after restart_delay, which a later
    # test covers; this one is about the state that the removal leaves
    config$config <- list(apps = list(list(name = "app", port = 3001,
      resident = FALSE, appstart_timeout = 60)), restart_delay = 0)
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

test_that("a restart that cannot stop the app leaves its users connected", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = 0)
  config$add_app_process("app", list(is_alive = function() TRUE))
  closed <- 0
  socket <- list(close = function() closed <<- closed + 1)
  config$add_ws_connection("s", list(app_name = "app", ws = socket))
  config$add_backend_connection("s", list(app_name = "app", ws = socket))
  local_mocked_bindings(kill_process_safely = function(process) FALSE)
  pm <- ProcessManager$new(config)

  expect_false(pm$restart_app("app")$success)
  expect_equal(closed, 0)
  expect_false(is.null(config$get_ws_connection("s")))
  expect_false(is.null(config$get_backend_connection("s")))
  expect_equal(config$get_app_connection_count("app"), 1)
})

# ============================================================================
# On-demand starts after a failure
# ============================================================================

test_that("an on-demand app whose process failed starts again only after restart_delay", {
  # The failure is found by the readiness check, the health check, or the
  # next on-demand start
  for (found_by in c("readiness", "health", "on_demand")) {
    config <- ShinyServerConfig$new()
    config$config <- list(apps = list(list(name = "app", port = 3001, resident = FALSE)), restart_delay = 60)
    dead <- list(is_alive = function() FALSE)
    config$add_app_process("app", dead)
    config$set_app_starting("app")
    pm <- ProcessManager$new(config)
    starts <- 0
    assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)

    switch(found_by, readiness = pm$check_app_ready("app", 3001, dead), health = pm$health_check(),
      on_demand = NULL)
    # Every request in the delay is refused without starting a process
    expect_false(pm$start_app_on_demand("app"), info = found_by)
    expect_false(pm$start_app_on_demand("app"), info = found_by)
    expect_equal(starts, 0, info = found_by)
    expect_null(config$get_app_process("app"), info = found_by)
    expect_gt(config$start_backoff_seconds("app"), 55)

    # Once restart_delay has passed, the next request starts it
    assign("app", Sys.time() - 61, envir = config$start_failures)
    expect_equal(config$start_backoff_seconds("app"), 0)
    expect_true(pm$start_app_on_demand("app"), info = found_by)
    expect_equal(starts, 1, info = found_by)
  }
})

test_that("an app forgets its last failure once it listens, is restarted, or is reloaded", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = FALSE)), restart_delay = 60)
  pm <- ProcessManager$new(config)

  # A new process that listens on the app's port has started
  config$note_start_failure("app")
  config$add_app_process("app", list(is_alive = function() TRUE))
  local_mocked_bindings(process_owns_port = function(process, port) TRUE)
  expect_true(config$app_backend_verified("app"))
  expect_equal(config$start_backoff_seconds("app"), 0)
  config$remove_app_process("app")

  # A restart from the dashboard does not wait
  config$note_start_failure("app")
  expect_true(pm$restart_app("app")$success)
  expect_equal(config$start_backoff_seconds("app"), 0)

  # Neither does the first start after a reload
  config$note_start_failure("app")
  config$reset_app_state()
  expect_equal(config$start_backoff_seconds("app"), 0)

  # Stopping a process is not a failure
  config$add_app_process("app", list(is_alive = function() FALSE))
  expect_true(pm$stop_app("app")$success)
  expect_equal(config$start_backoff_seconds("app"), 0)
})

test_that("an on-demand app whose process has exited is shown as dormant, not crashed", {
  app_dir <- make_test_app_dir()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(
    list(name = "on_demand", path = app_dir, port = 3001, resident = FALSE),
    list(name = "broken", path = file.path(tempdir(), "tss-no-such-app"), port = 3002, resident = FALSE),
    list(name = "resident", path = app_dir, port = 3003, resident = TRUE)
  ))
  for (name in c("on_demand", "broken", "resident")) {
    config$add_app_process(name, list(get_pid = function() 111, is_alive = function() FALSE))
  }
  pm <- ProcessManager$new(config)

  # Opening it starts it again, so the landing page links to it
  expect_equal(pm$get_app_status("on_demand")$status, "dormant")
  expect_equal(pm$get_app_status("broken")$status, "unavailable")
  # A resident app is restarted by the health check, or from the dashboard
  expect_equal(pm$get_app_status("resident")$status, "crashed")
  for (name in c("on_demand", "broken", "resident")) {
    expect_null(pm$get_app_status(name)$pid, info = name)
  }
})

# ============================================================================
# Health checks and restart delays
# ============================================================================

test_that("restart delays yield and are cancelled by stop and shutdown", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = TRUE)), restart_delay = 5)
  pm <- ProcessManager$new(config)
  starts <- 0
  assign("start_app", function(app_config) { starts <<- starts + 1; TRUE }, envir = pm)
  # The test runs each scheduled restart when it chooses, so that no timing
  # on a loaded machine can run one early
  scheduled <- list()
  local_mocked_bindings(later = function(func, delay = 0, ...) {
    scheduled[[length(scheduled) + 1]] <<- list(func = func, delay = delay)
    invisible(function() FALSE)
  }, .package = "later")
  restarts <- function() Filter(function(entry) entry$delay == 5, scheduled)

  # A resident app without a process is restarted by the health check, after
  # restart_delay rather than at once
  pm$health_check()
  expect_equal(starts, 0)
  expect_length(restarts(), 1)
  # Another health check keeps the pending restart
  pm$health_check()
  expect_length(restarts(), 1)
  restarts()[[1]]$func()
  expect_equal(starts, 1)

  # Stopping the app, or every app, cancels a pending restart
  for (stop in list(function() pm$stop_app("app"), function() pm$stop_all_apps())) {
    scheduled <- list()
    pm$health_check()
    expect_length(restarts(), 1)
    stop()
    restarts()[[1]]$func()
    expect_equal(starts, 1)
  }
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

# ============================================================================
# Records of Quarto renders
# ============================================================================

# An app directory with a document, a data file, and a page rendered after them
rendered_quarto_app <- function() {
  app_dir <- tempfile("tss-qmd-state")
  dir.create(file.path(app_dir, "data"), recursive = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  for (file in c(qmd, file.path(app_dir, "data", "a.csv"), file.path(app_dir, "doc.html"))) writeLines("x", file)
  Sys.setFileTime(qmd, Sys.time() - 300)
  Sys.setFileTime(file.path(app_dir, "data", "a.csv"), Sys.time() - 300)
  Sys.setFileTime(file.path(app_dir, "doc.html"), Sys.time() - 200)
  app_dir
}

# Record a finished render of the document, as render_quarto_document() does
record_quarto_render <- function(qmd, state_file, log_dir = NULL) {
  check <- quarto_needs_render(qmd, log_dir, state_file)
  page <- file.info(sub("\\.qmd$", ".html", qmd))
  saveRDS(list(document = qmd, status = "rendered", sources = attr(check, "sources"),
    page = list(mtime = as.numeric(page$mtime), size = as.numeric(page$size))), state_file)
}

test_that("a render that did not finish makes a Quarto page stale", {
  app_dir <- rendered_quarto_app()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  state_file <- tempfile(fileext = ".rds")
  on.exit(unlink(state_file), add = TRUE)

  # Without a record, a page newer than the sources is current
  expect_false(quarto_needs_render(qmd, NULL, state_file))
  # A render stopped part-way leaves an incomplete page, newer than the sources
  saveRDS(list(document = qmd, status = "rendering"), state_file)
  expect_true(quarto_needs_render(qmd, NULL, state_file))
  record_quarto_render(qmd, state_file)
  expect_false(quarto_needs_render(qmd, NULL, state_file))
  # A record of another document says nothing about this one
  saveRDS(list(document = file.path(app_dir, "other.qmd"), status = "rendering"), state_file)
  expect_false(quarto_needs_render(qmd, NULL, state_file))
})

test_that("a recorded render notices sources that are older, removed, or added", {
  app_dir <- rendered_quarto_app()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  data <- file.path(app_dir, "data", "a.csv")
  state_file <- tempfile(fileext = ".rds")
  on.exit(unlink(state_file), add = TRUE)
  record_quarto_render(qmd, state_file)

  # A copy that keeps an older modification time, as Copy-Item and cp -p do
  Sys.setFileTime(data, Sys.time() - 3000)
  expect_true(quarto_needs_render(qmd, NULL, state_file))
  # Without the record, the page seems current
  expect_false(quarto_needs_render(qmd))
  record_quarto_render(qmd, state_file)
  expect_false(quarto_needs_render(qmd, NULL, state_file))

  unlink(data)
  expect_true(quarto_needs_render(qmd, NULL, state_file))
  record_quarto_render(qmd, state_file)

  writeLines("x", file.path(app_dir, "data", "b.csv"))
  Sys.setFileTime(file.path(app_dir, "data", "b.csv"), Sys.time() - 3000)
  expect_true(quarto_needs_render(qmd, NULL, state_file))
  record_quarto_render(qmd, state_file)

  # A page replaced since the render, such as one rendered elsewhere and
  # deployed, is judged as a page that the server did not render
  writeLines("other page", file.path(app_dir, "doc.html"))
  expect_false(quarto_needs_render(qmd, NULL, state_file))
  Sys.setFileTime(file.path(app_dir, "doc.html"), Sys.time() - 6000)
  expect_true(quarto_needs_render(qmd, NULL, state_file))
})

test_that("what a Quarto render writes beside its page is not a source", {
  app_dir <- rendered_quarto_app()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  state_file <- tempfile(fileext = ".rds")
  on.exit(unlink(state_file), add = TRUE)
  record_quarto_render(qmd, state_file)
  # The data of "context: data" chunks and the knitr cache, written by each
  # render, and the intermediate files that a render stopped part-way leaves
  for (name in c("doc_data/chunk.RData", "doc_cache/html/x.rdb", "doc_files/libs/a.js",
    "doc.rmarkdown", "doc.knit.md", "doc.quarto_ipynb")) {
    dir.create(dirname(file.path(app_dir, name)), recursive = TRUE, showWarnings = FALSE)
    writeLines("x", file.path(app_dir, name))
  }
  expect_false(quarto_needs_render(qmd, NULL, state_file))
  expect_false(quarto_needs_render(qmd))
})

test_that("the record of a render in an app directory's log_dir is not a source", {
  app_dir <- rendered_quarto_app()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  log_dir <- file.path(app_dir, "logs")
  dir.create(log_dir)
  state_file <- file.path(log_dir, "app_render.rds")
  record_quarto_render(qmd, state_file, log_dir)
  sources <- attr(quarto_needs_render(qmd, log_dir, state_file), "sources")$files
  expect_false("logs/app_render.rds" %in% sources)
  expect_false(quarto_needs_render(qmd, log_dir, state_file))
  # Another app's record there is the server's too, but other files are not
  writeLines("", file.path(log_dir, "other_render.rds"))
  expect_false(quarto_needs_render(qmd, log_dir, state_file))
  writeLines("", file.path(log_dir, "notes.rds"))
  expect_true(quarto_needs_render(qmd, log_dir, state_file))
})

test_that("render_quarto_document records a render that fails as unfinished", {
  app_dir <- rendered_quarto_app()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  state_file <- tempfile(fileext = ".rds")
  on.exit(unlink(state_file), add = TRUE)
  # render_quarto_document() also sets QUARTO_R, as it does in an app's process
  saved <- Sys.getenv(c("QUARTO_PATH", "PATH", "QUARTO_R"), unset = NA)
  on.exit(for (name in names(saved)) {
    if (is.na(saved[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, structure(list(saved[[name]]), names = name))
  }, add = TRUE)
  Sys.unsetenv("QUARTO_PATH")
  Sys.setenv(PATH = "")
  expect_error(render_quarto_document(qmd, list(), state_file, ""), "Quarto CLI was not found")
  Sys.setenv(PATH = saved[["PATH"]])

  # A program that exits with an error stands in for the CLI. The app's own
  # QUARTO_PATH is used before the CLI that the server found.
  failing <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  Sys.setenv(QUARTO_PATH = failing)
  expect_error(suppressWarnings(render_quarto_document(qmd, list(), state_file, "/no/such/quarto")),
    "failed with exit status")
  expect_equal(readRDS(state_file)$status, "rendering")
  expect_true(quarto_needs_render(qmd, NULL, state_file))
})

test_that("files that a Quarto document writes as it renders do not make its page stale", {
  app_dir <- rendered_quarto_app()
  runner <- tempfile("tss-render-runner")
  dir.create(runner)
  on.exit(unlink(c(app_dir, runner), recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  state_file <- tempfile(fileext = ".rds")
  on.exit(unlink(state_file), add = TRUE)
  saved <- Sys.getenv(c("QUARTO_PATH", "QUARTO_R", "R_LIBS", "R_ENVIRON_USER"), unset = NA)
  on.exit(for (name in names(saved)) {
    if (is.na(saved[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, structure(list(saved[[name]]), names = name))
  }, add = TRUE)
  Sys.setenv(QUARTO_PATH = file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"))
  old_dir <- setwd(runner)
  on.exit(setwd(old_dir), add = TRUE, after = FALSE)
  # Rscript stands in for the CLI: "render doc.qmd" runs the script named
  # "render" in the working directory, which writes the page and, as a
  # document's code may, a file of its own and an update of a data file
  render <- function(...) {
    writeLines(c(
      sprintf("writeLines('summary', %s)", deparse(file.path(app_dir, "summary.txt"))),
      sprintf("writeLines('updated', %s)", deparse(file.path(app_dir, "data", "a.csv"))),
      ...,
      sprintf("writeLines('<html></html>', %s)", deparse(file.path(app_dir, "doc.html")))
    ), file.path(runner, "render"))
    check <- quarto_needs_render(qmd, NULL, state_file)
    render_quarto_document(qmd, attr(check, "sources"), state_file, "", attr(check, "snapshot"))
  }

  render()
  expect_true("summary.txt" %in% readRDS(state_file)$sources$files)
  expect_false(quarto_needs_render(qmd, NULL, state_file))

  # The document is recorded as it was when the render began, so that an
  # edit made to it while it rendered is rendered at the next start
  render(sprintf("writeLines('edited', %s)", deparse(qmd)))
  expect_true(quarto_needs_render(qmd, NULL, state_file))
})

test_that("a Quarto page whose resources are missing is rendered again", {
  app_dir <- rendered_quarto_app()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  page <- file.path(app_dir, "doc.html")
  state_file <- tempfile(fileext = ".rds")
  on.exit(unlink(state_file), add = TRUE)
  write_page <- function(lines) {
    writeLines(c("<html>", lines, "</html>"), page)
    Sys.setFileTime(page, Sys.time() - 200)
  }
  dir.create(file.path(app_dir, "doc_files", "libs"), recursive = TRUE)
  write_page("<script src=\"doc_files/libs/shiny.js\"></script>")
  expect_false(quarto_needs_render(qmd))
  record_quarto_render(qmd, state_file)
  expect_false(quarto_needs_render(qmd, NULL, state_file))

  # Deployed without its resources, or with them cleaned away
  unlink(file.path(app_dir, "doc_files"), recursive = TRUE)
  expect_true(quarto_needs_render(qmd))
  expect_true(quarto_needs_render(qmd, NULL, state_file))

  # A page that embeds its resources needs no such directory
  write_page("<script>/* embedded */</script>")
  expect_false(quarto_needs_render(qmd))
})

test_that("a Quarto render's R session gets the app's library paths", {
  app_dir <- rendered_quarto_app()
  lib <- tempfile("tss-render-lib")
  runner <- tempfile("tss-render-runner")
  dir.create(lib)
  dir.create(runner)
  on.exit(unlink(c(app_dir, lib, runner), recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  seen <- file.path(runner, "seen.rds")
  state_file <- tempfile(fileext = ".rds")
  on.exit(unlink(state_file), add = TRUE)
  saved <- Sys.getenv(c("QUARTO_PATH", "QUARTO_R", "R_LIBS", "R_ENVIRON_USER", "R_ENVIRON", "R_PROFILE",
    "R_PROFILE_USER"), unset = NA)
  on.exit(for (name in names(saved)) {
    if (is.na(saved[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, structure(list(saved[[name]]), names = name))
  }, add = TRUE)
  # R CMD check run through callr, as devtools::check() and rcmdcheck run it,
  # leaves callr's startup files in R_ENVIRON, R_PROFILE, and R_PROFILE_USER,
  # which set the library paths of every R started below it. An app's
  # process, which callr starts with r_bg(), has them restored to the
  # server's own, so the stand-in for Quarto starts without them, and without
  # a user profile that could set the library paths itself.
  Sys.unsetenv(c("R_ENVIRON", "R_PROFILE"))
  Sys.setenv(R_PROFILE_USER = nullfile())
  old_paths <- .libPaths()
  on.exit(.libPaths(old_paths), add = TRUE)
  # As an app's .Renviron or profile may leave them
  .libPaths(c(lib, old_paths))

  # Rscript stands in for the CLI: "render doc.qmd" runs the script named
  # "render" in the working directory, which records what the R session
  # that Quarto would start finds
  writeLines(sprintf("saveRDS(list(paths = .libPaths(), environ = Sys.getenv('R_ENVIRON_USER')), %s)",
    deparse(seen)), file.path(runner, "render"))
  Sys.setenv(QUARTO_PATH = file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"),
    R_LIBS = "elsewhere", R_ENVIRON_USER = "app.Renviron")
  old_dir <- setwd(runner)
  on.exit(setwd(old_dir), add = TRUE, after = FALSE)
  render_quarto_document(qmd, list(), state_file, "")
  .libPaths(old_paths)

  render <- readRDS(seen)
  expect_true(normalizePath(lib, winslash = "/") %in% normalizePath(render$paths, winslash = "/"))
  # What the user environment file set, the app's own process already holds
  expect_equal(render$environ, nullfile())
  expect_equal(readRDS(state_file)$status, "rendered")
  # The app's own process keeps its variables
  expect_equal(Sys.getenv(c("R_LIBS", "R_ENVIRON_USER"), names = FALSE), c("elsewhere", "app.Renviron"))
})

test_that("a Quarto page is rendered again once a package that it was rendered with has changed", {
  app_dir <- rendered_quarto_app()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  page <- file.path(app_dir, "doc.html")
  state_file <- tempfile(fileext = ".rds")
  on.exit(unlink(state_file), add = TRUE)
  # A page that records the versions of the packages it was rendered with,
  # as rmarkdown records them: its HTML dependencies, and the packages that
  # its code loaded, among them base, whose version is R's
  write_page <- function(dependency, base, other = "stats") {
    context <- function(name, value) {
      c(sprintf("<script type=\"application/shiny-prerendered\" data-context=\"%s\">", name),
        as.character(jsonlite::serializeJSON(value)), "</script>")
    }
    dependencies <- list(list(name = "dep", version = "1.0", package = "stats", pkgVersion = dependency))
    execution <- list(packages = data.frame(packages = c("base", other), version = c(base, dependency)))
    writeLines(c("<html><body>", context("dependencies", dependencies),
      context("execution_dependencies", execution), "</body></html>"), page)
    Sys.setFileTime(page, Sys.time() - 200)
  }
  current <- as.character(getRversion())

  write_page(current, current)
  expect_false(quarto_needs_render(qmd))
  record_quarto_render(qmd, state_file)
  expect_false(quarto_needs_render(qmd, NULL, state_file))

  # After an update of R, of a package, or with a package removed, whether or
  # not the server rendered the page
  for (versions in list(c(current, "1.0.0"), c("1.0.0", current), c(current, current, "tss.no.such.package"))) {
    do.call(write_page, as.list(versions))
    expect_true(quarto_needs_render(qmd), info = toString(versions))
    record_quarto_render(qmd, state_file)
    expect_true(quarto_needs_render(qmd, NULL, state_file), info = toString(versions))
  }

  # A record that cannot be read leaves the page to be judged by its files
  writeLines(c("<script type=\"application/shiny-prerendered\" data-context=\"execution_dependencies\">",
    "not JSON", "</script>"), page)
  Sys.setFileTime(page, Sys.time() - 200)
  expect_false(quarto_needs_render(qmd))
})

test_that("an app's own process renders its Quarto document and records the render", {
  skip_on_cran() # Starts an R process and the Quarto CLI
  quarto_bin <- quarto::quarto_path()
  skip_if(is.null(quarto_bin), "The Quarto CLI is not installed")
  app_dir <- tempfile("tss-qmd-render")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  qmd <- file.path(app_dir, "doc.qmd")
  writeLines(c("---", "title: Test", "---", "", "Some text."), qmd)
  state_file <- file.path(app_dir, ".render.rds")
  needs_render <- quarto_needs_render
  environment(needs_render) <- baseenv()
  render_document <- render_quarto_document
  environment(render_document) <- baseenv()
  in_app_process <- function() {
    callr::r(function(needs_render, render_document, file, state_file, quarto_bin) {
      render <- needs_render(file, NULL, state_file)
      if (render) render_document(file, attr(render, "sources"), state_file, quarto_bin)
      as.logical(render)
    }, args = list(needs_render = needs_render, render_document = render_document, file = qmd,
      state_file = state_file, quarto_bin = quarto_bin))
  }
  expect_true(in_app_process())
  expect_true(file.exists(file.path(app_dir, "doc.html")))
  expect_equal(readRDS(state_file)$status, "rendered")
  expect_false(in_app_process())
})

test_that("an app's environment file sets its library paths, and only them, again", {
  skip_on_cran() # Starts an R process
  app_dir <- tempfile("tss-libs")
  dir.create(file.path(app_dir, "lib"), recursive = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  environ <- file.path(app_dir, ".Renviron")
  lib <- normalizePath(file.path(app_dir, "lib"), winslash = "/")
  writeLines(c(paste0("R_LIBS_USER=", lib), "TSS_TEST_GROWS=${TSS_TEST_GROWS}x", "TSS_TEST_SET=app"), environ)
  libraries <- apply_environ_libraries
  environment(libraries) <- baseenv()
  result <- callr::r(function(libraries, environ) {
    # As callr leaves them once the app's process has started
    Sys.setenv(TSS_TEST_GROWS = "a", R_LIBS_USER = "elsewhere")
    Sys.unsetenv("TSS_TEST_SET")
    applied <- libraries(environ)
    list(applied = applied, paths = .libPaths(), grows = Sys.getenv("TSS_TEST_GROWS"),
      set = Sys.getenv("TSS_TEST_SET", unset = NA), user = Sys.getenv("R_LIBS_USER"))
  }, args = list(libraries = libraries, environ = environ))
  expect_true(result$applied)
  # After any that R_LIBS names, as at R's startup
  expect_true(lib %in% result$paths)
  expect_equal(result$user, lib)
  # Other variables keep the values that R gave them at startup
  expect_equal(result$grows, "a")
  expect_true(is.na(result$set))

  # A file that sets no library variable changes nothing
  writeLines("TSS_TEST_SET=app", environ)
  unchanged <- callr::r(function(libraries, environ) {
    before <- .libPaths()
    list(applied = libraries(environ), same = identical(before, .libPaths()))
  }, args = list(libraries = libraries, environ = environ))
  expect_false(unchanged$applied)
  expect_true(unchanged$same)
  expect_false(apply_environ_libraries(nullfile()))
})

test_that("a line that R cannot read in an app's environment file leaves its library paths set", {
  lib <- tempfile("tss-environ-lib")
  environ <- tempfile("tss-environ")
  dir.create(lib)
  on.exit(unlink(c(lib, environ), recursive = TRUE), add = TRUE)
  old_paths <- .libPaths()
  old_env <- Sys.getenv(c("R_LIBS", "R_LIBS_USER"), unset = NA)
  on.exit({
    .libPaths(old_paths)
    for (name in names(old_env)) {
      if (is.na(old_env[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, structure(list(old_env[[name]]), names = name))
    }
  }, add = TRUE)
  # R reports such a line at the app's startup, and applies the others
  writeLines(c("a stray word", paste0("R_LIBS_USER=", normalizePath(lib, winslash = "/"))), environ)
  warned <- FALSE
  applied <- withCallingHandlers(apply_environ_libraries(environ), warning = function(w) {
    warned <<- TRUE
    invokeRestart("muffleWarning")
  })
  paths <- .libPaths()
  .libPaths(old_paths)

  expect_true(applied)
  expect_false(warned)
  expect_true(normalizePath(lib, winslash = "/") %in% normalizePath(paths, winslash = "/"))
})

test_that("closing an app's sessions from the process manager records each close once", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 1, resident = TRUE)))
  pm <- ProcessManager$new(config)
  for (id in c("s1", "s2")) {
    config$add_ws_connection(id, list(app_name = "app", ws = list(close = function() NULL)))
  }
  logged <- capture_test_log(pm$cleanup_app_connections("app"))
  expect_equal(sum(grepl("WebSocket connection closed for session s[12]", logged)), 2)
  # The sockets' own close handlers then find nothing to record
  cm <- ConnectionManager$new(config, pm)
  expect_length(grep("closed for session", capture_test_log(cm$remove_client_connection("s1"))), 0)

  config$add_ws_connection("s3", list(app_name = "app", ws = list(close = function() NULL)))
  logged <- capture_test_log(pm$stop_all_apps())
  expect_equal(sum(grepl("WebSocket connection closed for session s3", logged)), 1)
})
