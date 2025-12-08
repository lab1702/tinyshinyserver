# Test script for cleanup race condition fixes
# This tests that cleanup operations are safe from race conditions

test_cleanup_fixes <- function() {
  cat("Testing cleanup race condition fixes...\n\n")

  # Load required modules
  source("R/config.R")
  source("R/process_manager.R")

  # Create test config and process manager
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "test-app", path = "/tmp/test", port = 3001, resident = FALSE)
    ),
    log_dir = tempdir()
  )

  process_manager <- ProcessManager$new(config)

  # Test 1: Cleanup handles concurrent connection removal
  cat("Test 1: Cleanup with concurrent connection removal\n")

  # Add several connections
  for (i in 1:10) {
    session_id <- paste0("session-", i)
    config$add_ws_connection(session_id, list(
      ws = NULL,
      app_name = "test-app",
      client_ip = "127.0.0.1",
      user_agent = "test",
      last_activity = Sys.time() - 3600,  # 1 hour ago (stale)
      created_at = Sys.time() - 3600
    ))
  }

  count_before <- config$get_app_connection_count("test-app")
  cat(sprintf("  Connections before cleanup: %d\n", count_before))

  # Simulate concurrent removal (remove some connections manually)
  config$remove_ws_connection("session-1")
  config$remove_ws_connection("session-2")

  # Run cleanup (should handle already-removed connections gracefully)
  cleaned <- process_manager$cleanup_stale_connections()

  count_after <- config$get_app_connection_count("test-app")
  cat(sprintf("  Connections after cleanup: %d\n", count_after))
  cat(sprintf("  Cleanup reported cleaning: %d connections\n", cleaned))

  # Should have cleaned all remaining stale connections
  if (count_after == 0) {
    cat("  ✓ PASS: Cleanup handled concurrent removal\n\n")
  } else {
    cat("  ✗ FAIL: Expected all stale connections to be cleaned\n\n")
  }

  # Test 2: Idempotent cleanup (running twice doesn't cause issues)
  cat("Test 2: Idempotent cleanup\n")

  # Add stale connections again
  for (i in 1:5) {
    session_id <- paste0("session-", i)
    config$add_ws_connection(session_id, list(
      ws = NULL,
      app_name = "test-app",
      client_ip = "127.0.0.1",
      user_agent = "test",
      last_activity = Sys.time() - 3600,
      created_at = Sys.time() - 3600
    ))
  }

  # Run cleanup twice
  cleaned1 <- process_manager$cleanup_stale_connections()
  cleaned2 <- process_manager$cleanup_stale_connections()

  cat(sprintf("  First cleanup: %d connections\n", cleaned1))
  cat(sprintf("  Second cleanup: %d connections\n", cleaned2))

  if (cleaned1 == 5 && cleaned2 == 0) {
    cat("  ✓ PASS: Cleanup is idempotent\n\n")
  } else {
    cat("  ✗ FAIL: Expected second cleanup to find 0 connections\n\n")
  }

  # Test 3: Cleanup respects fresh connections
  cat("Test 3: Cleanup preserves fresh connections\n")

  # Add mix of stale and fresh connections
  for (i in 1:3) {
    session_id <- paste0("stale-", i)
    config$add_ws_connection(session_id, list(
      ws = NULL,
      app_name = "test-app",
      client_ip = "127.0.0.1",
      user_agent = "test",
      last_activity = Sys.time() - 3600,  # Stale
      created_at = Sys.time() - 3600
    ))
  }

  for (i in 1:3) {
    session_id <- paste0("fresh-", i)
    config$add_ws_connection(session_id, list(
      ws = NULL,
      app_name = "test-app",
      client_ip = "127.0.0.1",
      user_agent = "test",
      last_activity = Sys.time(),  # Fresh
      created_at = Sys.time()
    ))
  }

  count_before <- config$get_app_connection_count("test-app")
  cleaned <- process_manager$cleanup_stale_connections()
  count_after <- config$get_app_connection_count("test-app")

  cat(sprintf("  Before cleanup: %d connections\n", count_before))
  cat(sprintf("  Cleaned: %d stale connections\n", cleaned))
  cat(sprintf("  After cleanup: %d connections (expected: 3 fresh)\n", count_after))

  if (count_before == 6 && cleaned == 3 && count_after == 3) {
    cat("  ✓ PASS: Cleanup only removed stale connections\n\n")
  } else {
    cat("  ✗ FAIL: Expected 3 fresh connections to remain\n\n")
  }

  # Test 4: Cleanup with NULL connection info
  cat("Test 4: Cleanup handles NULL connection info gracefully\n")

  # Add a connection
  config$add_ws_connection("test-session", list(
    ws = NULL,
    app_name = "test-app",
    client_ip = "127.0.0.1",
    user_agent = "test",
    last_activity = Sys.time() - 3600,
    created_at = Sys.time() - 3600
  ))

  # Manually corrupt by setting to NULL (simulating concurrent removal)
  config$ws_connections[["test-session"]] <- NULL

  # Cleanup should handle this gracefully
  error_occurred <- FALSE
  tryCatch({
    process_manager$cleanup_stale_connections()
  }, error = function(e) {
    error_occurred <- TRUE
    cat(sprintf("  ERROR: %s\n", e$message))
  })

  if (!error_occurred) {
    cat("  ✓ PASS: Cleanup handled NULL connection info\n\n")
  } else {
    cat("  ✗ FAIL: Cleanup should not error on NULL connections\n\n")
  }

  # Test 5: Dead process cleanup
  cat("Test 5: Dead process cleanup is safe\n")

  # The process_manager doesn't have real processes, so this is a basic test
  # Just verify it doesn't crash
  error_occurred <- FALSE
  tryCatch({
    cleaned_processes <- process_manager$cleanup_dead_processes()
    cat(sprintf("  Cleaned %d dead processes\n", cleaned_processes))
  }, error = function(e) {
    error_occurred <- TRUE
    cat(sprintf("  ERROR: %s\n", e$message))
  })

  if (!error_occurred) {
    cat("  ✓ PASS: Dead process cleanup executed safely\n\n")
  } else {
    cat("  ✗ FAIL: Dead process cleanup should not error\n\n")
  }

  cat("All cleanup tests completed!\n")
}

# Run tests if this file is executed directly
if (!interactive()) {
  # Set up minimal logging
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_threshold(logger::ERROR)  # Only show errors
  }

  test_cleanup_fixes()
}
