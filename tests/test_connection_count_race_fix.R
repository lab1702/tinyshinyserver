# Test script for connection count race condition fixes
# This tests the idempotent operations and cache consistency validation

test_connection_count_fixes <- function() {
  cat("Testing connection count race condition fixes...\n\n")

  # Load the config module
  source("R/config.R")

  # Create a test config
  config <- ShinyServerConfig$new()

  # Test 1: Idempotent add - adding same connection twice
  cat("Test 1: Idempotent add_ws_connection\n")
  session_id <- "test-session-1"
  conn_info <- list(
    ws = NULL,
    app_name = "test-app",
    client_ip = "127.0.0.1",
    user_agent = "test",
    last_activity = Sys.time(),
    created_at = Sys.time()
  )

  # First add should increment count
  config$add_ws_connection(session_id, conn_info)
  count1 <- config$get_app_connection_count("test-app")
  cat(sprintf("  After first add: count = %d (expected: 1)\n", count1))

  # Second add with same session_id should NOT increment (update only)
  config$add_ws_connection(session_id, conn_info)
  count2 <- config$get_app_connection_count("test-app")
  cat(sprintf("  After second add (same session): count = %d (expected: 1)\n", count2))

  if (count1 == 1 && count2 == 1) {
    cat("  ✓ PASS: Add is idempotent\n\n")
  } else {
    cat("  ✗ FAIL: Expected count to remain 1\n\n")
  }

  # Test 2: Idempotent remove - removing same connection twice
  cat("Test 2: Idempotent remove_ws_connection\n")

  # First remove should decrement count
  result1 <- config$remove_ws_connection(session_id)
  count3 <- config$get_app_connection_count("test-app")
  cat(sprintf("  After first remove: count = %d, result = %s (expected: 0, TRUE)\n", count3, result1))

  # Second remove should be idempotent (no-op)
  result2 <- config$remove_ws_connection(session_id)
  count4 <- config$get_app_connection_count("test-app")
  cat(sprintf("  After second remove: count = %d, result = %s (expected: 0, FALSE)\n", count4, result2))

  if (count3 == 0 && count4 == 0 && result1 == TRUE && result2 == FALSE) {
    cat("  ✓ PASS: Remove is idempotent\n\n")
  } else {
    cat("  ✗ FAIL: Expected count to be 0 and second remove to return FALSE\n\n")
  }

  # Test 3: Multiple connections for same app
  cat("Test 3: Multiple connections for same app\n")
  for (i in 1:5) {
    session <- paste0("session-", i)
    config$add_ws_connection(session, list(
      ws = NULL,
      app_name = "test-app",
      client_ip = "127.0.0.1",
      user_agent = "test",
      last_activity = Sys.time(),
      created_at = Sys.time()
    ))
  }

  count5 <- config$get_app_connection_count("test-app")
  cat(sprintf("  After adding 5 connections: count = %d (expected: 5)\n", count5))

  # Validate consistency
  validation <- config$validate_connection_count_consistency(fix_errors = FALSE)
  cat(sprintf("  Consistency check: %s\n", ifelse(validation$consistent, "PASS", "FAIL")))

  if (count5 == 5 && validation$consistent) {
    cat("  ✓ PASS: Multiple connections tracked correctly\n\n")
  } else {
    cat("  ✗ FAIL\n\n")
  }

  # Test 4: Consistency validation and auto-fix
  cat("Test 4: Cache consistency validation and auto-fix\n")

  # Manually corrupt the cache
  assign("test-app", 999, envir = config$app_connection_counts)
  corrupted_count <- config$get_app_connection_count("test-app")
  cat(sprintf("  Manually corrupted count: %d (expected: 999)\n", corrupted_count))

  # Validate and fix
  validation2 <- config$validate_connection_count_consistency(fix_errors = TRUE)
  fixed_count <- config$get_app_connection_count("test-app")
  cat(sprintf("  After auto-fix: count = %d (expected: 5)\n", fixed_count))
  cat(sprintf("  Inconsistencies found: %d\n", length(validation2$inconsistencies)))

  if (!validation2$consistent && fixed_count == 5) {
    cat("  ✓ PASS: Auto-fix corrected the cache\n\n")
  } else {
    cat("  ✗ FAIL\n\n")
  }

  # Test 5: Edge case - negative count protection
  cat("Test 5: Negative count protection\n")

  # Remove all connections
  for (i in 1:5) {
    session <- paste0("session-", i)
    config$remove_ws_connection(session)
  }

  # Try to remove one more (should not go negative)
  config$remove_ws_connection("non-existent-session")
  final_count <- config$get_app_connection_count("test-app")
  cat(sprintf("  After removing non-existent session: count = %d (expected: 0)\n", final_count))

  if (final_count == 0) {
    cat("  ✓ PASS: Count cannot go negative\n\n")
  } else {
    cat("  ✗ FAIL\n\n")
  }

  cat("All tests completed!\n")
}

# Run tests if this file is executed directly
if (!interactive()) {
  # Set up minimal logging
  if (requireNamespace("logger", quietly = TRUE)) {
    logger::log_threshold(logger::WARN)
  }

  test_connection_count_fixes()
}
