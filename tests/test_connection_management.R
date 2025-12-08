# Test for connection management functions
# Tests for WebSocket and backend connection management in ShinyServerConfig

library(testthat)
library(devtools)
load_all(".")

# ============================================================================
# WebSocket Connection Management Tests
# ============================================================================

test_that("add_ws_connection adds new connection and increments count", {
  config <- ShinyServerConfig$new()

  conn_info <- list(
    ws = NULL,
    app_name = "app1",
    client_ip = "127.0.0.1",
    user_agent = "test",
    last_activity = Sys.time(),
    created_at = Sys.time()
  )

  result <- config$add_ws_connection("session1", conn_info)

  expect_true(result)
  expect_equal(config$get_app_connection_count("app1"), 1)

  # Verify connection is stored
  stored <- config$get_ws_connection("session1")
  expect_equal(stored$app_name, "app1")
  expect_equal(stored$client_ip, "127.0.0.1")
})

test_that("add_ws_connection is idempotent for same session", {
  config <- ShinyServerConfig$new()

  conn_info <- list(
    ws = NULL,
    app_name = "app1",
    client_ip = "127.0.0.1",
    user_agent = "test",
    last_activity = Sys.time(),
    created_at = Sys.time()
  )

  # Add first time
  config$add_ws_connection("session1", conn_info)
  expect_equal(config$get_app_connection_count("app1"), 1)

  # Add again with same session_id (should update, not increment)
  config$add_ws_connection("session1", conn_info)
  expect_equal(config$get_app_connection_count("app1"), 1)
})

test_that("add_ws_connection handles NULL inputs", {
  config <- ShinyServerConfig$new()

  conn_info <- list(app_name = "app1")

  # NULL session_id
  result <- config$add_ws_connection(NULL, conn_info)
  expect_false(result)

  # NULL connection_info
  result <- config$add_ws_connection("session1", NULL)
  expect_false(result)
})

test_that("add_ws_connection handles multiple apps", {
  config <- ShinyServerConfig$new()

  conn1 <- list(app_name = "app1", ws = NULL)
  conn2 <- list(app_name = "app2", ws = NULL)

  config$add_ws_connection("s1", conn1)
  config$add_ws_connection("s2", conn2)
  config$add_ws_connection("s3", conn1)

  expect_equal(config$get_app_connection_count("app1"), 2)
  expect_equal(config$get_app_connection_count("app2"), 1)
})

test_that("remove_ws_connection removes connection and decrements count", {
  config <- ShinyServerConfig$new()

  conn_info <- list(app_name = "app1", ws = NULL)

  config$add_ws_connection("session1", conn_info)
  expect_equal(config$get_app_connection_count("app1"), 1)

  result <- config$remove_ws_connection("session1")

  expect_true(result)
  expect_equal(config$get_app_connection_count("app1"), 0)
  expect_null(config$get_ws_connection("session1"))
})

test_that("remove_ws_connection is idempotent", {
  config <- ShinyServerConfig$new()

  conn_info <- list(app_name = "app1", ws = NULL)

  config$add_ws_connection("session1", conn_info)

  # First remove should succeed
  result1 <- config$remove_ws_connection("session1")
  expect_true(result1)
  expect_equal(config$get_app_connection_count("app1"), 0)

  # Second remove should return FALSE (idempotent)
  result2 <- config$remove_ws_connection("session1")
  expect_false(result2)
  expect_equal(config$get_app_connection_count("app1"), 0)
})

test_that("remove_ws_connection handles NULL session_id", {
  config <- ShinyServerConfig$new()

  result <- config$remove_ws_connection(NULL)
  expect_false(result)
})

test_that("remove_ws_connection never goes negative", {
  config <- ShinyServerConfig$new()

  # Manually corrupt the cache to test defensive programming
  assign("app1", -5, envir = config$app_connection_counts)

  # Add and remove a connection
  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$remove_ws_connection("s1")

  # Should be 0, not negative
  expect_equal(config$get_app_connection_count("app1"), 0)
})

test_that("get_ws_connection returns NULL for non-existent session", {
  config <- ShinyServerConfig$new()

  result <- config$get_ws_connection("nonexistent")
  expect_null(result)
})

test_that("get_all_ws_connections returns all connections", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app2", ws = NULL))

  all_conns <- config$get_all_ws_connections()

  expect_equal(length(all_conns), 2)
  expect_true("s1" %in% names(all_conns))
  expect_true("s2" %in% names(all_conns))
})

# ============================================================================
# Backend Connection Management Tests
# ============================================================================

test_that("add_backend_connection stores backend connection", {
  config <- ShinyServerConfig$new()

  conn_info <- list(
    app_name = "app1",
    port = 3001,
    connected_at = Sys.time()
  )

  config$add_backend_connection("backend1", conn_info)

  stored <- config$get_backend_connection("backend1")
  expect_equal(stored$app_name, "app1")
  expect_equal(stored$port, 3001)
})

test_that("remove_backend_connection removes backend connection", {
  config <- ShinyServerConfig$new()

  conn_info <- list(app_name = "app1", port = 3001)

  config$add_backend_connection("backend1", conn_info)
  expect_false(is.null(config$get_backend_connection("backend1")))

  config$remove_backend_connection("backend1")
  expect_null(config$get_backend_connection("backend1"))
})

test_that("get_all_backend_connections returns all backend connections", {
  config <- ShinyServerConfig$new()

  config$add_backend_connection("b1", list(app_name = "app1"))
  config$add_backend_connection("b2", list(app_name = "app2"))

  all_backends <- config$get_all_backend_connections()

  expect_equal(length(all_backends), 2)
  expect_true("b1" %in% names(all_backends))
  expect_true("b2" %in% names(all_backends))
})

# ============================================================================
# App Process Management Tests
# ============================================================================

test_that("add_app_process stores process", {
  config <- ShinyServerConfig$new()

  mock_process <- list(
    pid = 12345,
    get_pid = function() 12345,
    is_alive = function() TRUE
  )

  config$add_app_process("app1", mock_process)

  stored <- config$get_app_process("app1")
  expect_equal(stored$pid, 12345)
})

test_that("remove_app_process removes process", {
  config <- ShinyServerConfig$new()

  mock_process <- list(pid = 12345)

  config$add_app_process("app1", mock_process)
  expect_false(is.null(config$get_app_process("app1")))

  config$remove_app_process("app1")
  expect_null(config$get_app_process("app1"))
})

test_that("get_all_app_processes returns all processes", {
  config <- ShinyServerConfig$new()

  config$add_app_process("app1", list(pid = 1))
  config$add_app_process("app2", list(pid = 2))

  all_procs <- config$get_all_app_processes()

  expect_equal(length(all_procs), 2)
  expect_true("app1" %in% names(all_procs))
  expect_true("app2" %in% names(all_procs))
})

# ============================================================================
# Connection Count Cache Tests
# ============================================================================

test_that("get_app_connection_count returns 0 for new app", {
  config <- ShinyServerConfig$new()

  count <- config$get_app_connection_count("newapp")
  expect_equal(count, 0)
})

test_that("connection count cache tracks multiple apps independently", {
  config <- ShinyServerConfig$new()

  # Add connections for app1
  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app1", ws = NULL))

  # Add connections for app2
  config$add_ws_connection("s3", list(app_name = "app2", ws = NULL))

  expect_equal(config$get_app_connection_count("app1"), 2)
  expect_equal(config$get_app_connection_count("app2"), 1)
  expect_equal(config$get_app_connection_count("app3"), 0)
})

test_that("validate_connection_count_consistency detects inconsistencies", {
  config <- ShinyServerConfig$new()

  # Add connections normally
  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app1", ws = NULL))

  # Manually corrupt the cache
  assign("app1", 999, envir = config$app_connection_counts)

  # Validate and fix
  result <- config$validate_connection_count_consistency(fix_errors = TRUE)

  expect_false(result$consistent)
  expect_true("app1" %in% names(result$inconsistencies))

  # After fix, cache should be corrected
  expect_equal(config$get_app_connection_count("app1"), 2)
})

test_that("validate_connection_count_consistency passes for consistent state", {
  config <- ShinyServerConfig$new()

  config$add_ws_connection("s1", list(app_name = "app1", ws = NULL))
  config$add_ws_connection("s2", list(app_name = "app1", ws = NULL))

  result <- config$validate_connection_count_consistency(fix_errors = FALSE)

  expect_true(result$consistent)
  expect_equal(length(result$inconsistencies), 0)
})

cat("Connection management tests completed successfully!\n")
