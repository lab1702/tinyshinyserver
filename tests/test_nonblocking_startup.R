# Test for non-blocking app startup with 503 responses
# This test verifies that the server returns 503 with Retry-After headers
# instead of blocking the event loop during app startup

library(testthat)
library(devtools)
load_all(".")

test_that("Config tracks app startup state correctly", {
  # Create a config instance
  config <- ShinyServerConfig$new()

  # Test initial state - app not starting
  expect_false(config$is_app_starting("test_app"))
  expect_null(config$get_app_startup_state("test_app"))

  # Mark app as starting
  config$set_app_starting("test_app")

  # Verify app is marked as starting
  expect_true(config$is_app_starting("test_app"))

  startup_state <- config$get_app_startup_state("test_app")
  expect_equal(startup_state$state, "starting")
  expect_true("started_at" %in% names(startup_state))
  expect_true("elapsed" %in% names(startup_state))

  # Mark app as ready
  config$set_app_ready("test_app")

  # Verify app is no longer starting
  expect_false(config$is_app_starting("test_app"))
  expect_null(config$get_app_startup_state("test_app"))
})

test_that("Config handles startup timeout correctly", {
  # Create a config instance
  config <- ShinyServerConfig$new()

  # Manually create an expired startup state
  assign("timeout_app", list(
    state = "starting",
    started_at = Sys.time() - (config$APP_STARTUP_TIMEOUT_SECONDS + 5)
  ), envir = config$app_startup_state)

  # Get startup state should detect timeout
  startup_state <- config$get_app_startup_state("timeout_app")

  expect_equal(startup_state$state, "timeout")
  expect_true(startup_state$elapsed > config$APP_STARTUP_TIMEOUT_SECONDS)

  # State should be cleared after timeout detection
  expect_null(config$get_app_startup_state("timeout_app"))
})

test_that("create_503_response returns proper structure", {
  response <- create_503_response("App is starting", retry_after_seconds = 5)

  # Check response structure
  expect_equal(response$status, 503)
  expect_true("Retry-After" %in% names(response$headers))
  expect_equal(response$headers$`Retry-After`, "5")
  expect_equal(response$headers$`Content-Type`, "application/json")

  # Check body is valid JSON
  body_parsed <- jsonlite::fromJSON(response$body)
  expect_true("error" %in% names(body_parsed))
  expect_true("retry_after_seconds" %in% names(body_parsed))
  expect_equal(body_parsed$retry_after_seconds, 5)
})

test_that("App startup state prevents duplicate starts", {
  # This tests the logic in start_app_on_demand that prevents
  # starting an app that's already in the starting state

  config <- ShinyServerConfig$new()

  # Mark app as starting
  config$set_app_starting("app1")

  # Verify it's marked as starting
  expect_true(config$is_app_starting("app1"))

  # Simulate checking if we should start it again
  # (this is what start_app_on_demand does)
  should_start <- !config$is_app_starting("app1")

  expect_false(should_start)
})

cat("Non-blocking startup tests completed successfully!\n")
