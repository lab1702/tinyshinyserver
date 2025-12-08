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
