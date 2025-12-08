# Test port assignment logic
# Verifies that ports are checked for actual availability

library(devtools)
load_all(".")

test_port_assignment <- function() {
  cat("=== Port Assignment Tests ===\n\n")

  # Test 1: Basic port assignment
  cat("Test 1: Basic port assignment without conflicts...\n")
  config <- ShinyServerConfig$new()

  # Create minimal config with apps
  config$config <- list(
    proxy_port = 3838,
    management_port = 3839,
    starting_port = 4200,
    apps = list(
      list(name = "test1", path = "/tmp/test1", resident = TRUE),
      list(name = "test2", path = "/tmp/test2", resident = TRUE),
      list(name = "test3", path = "/tmp/test3", resident = TRUE)
    )
  )

  config$assign_app_ports()

  # Ports should be assigned
  for (app in config$config$apps) {
    if (is.null(app$port)) {
      stop("Port not assigned to app: ", app$name)
    }
    cat(sprintf("  App '%s' assigned to port %d\n", app$name, app$port))
  }
  cat("  PASS: All apps have ports assigned\n\n")

  # Test 2: Port assignment skips reserved ports
  cat("Test 2: Port assignment skips reserved ports...\n")
  config2 <- ShinyServerConfig$new()

  # Create minimal config with apps
  config2$config <- list(
    proxy_port = 3838,
    management_port = 3839,
    starting_port = 3838, # Start at same port as proxy
    apps = list(
      list(name = "app1", path = "/tmp/app1", resident = TRUE),
      list(name = "app2", path = "/tmp/app2", resident = TRUE)
    )
  )

  config2$assign_app_ports()

  # Verify apps didn't get assigned reserved ports
  for (app in config2$config$apps) {
    if (app$port == 3838 || app$port == 3839) {
      stop(sprintf("App '%s' assigned reserved port %d", app$name, app$port))
    }
  }
  cat("  PASS: Apps correctly skip reserved ports\n\n")

  # Test 3: Port assignment handles ports in use
  cat("Test 3: Port assignment skips ports in use by other processes...\n")

  # Create a test server to occupy a port
  test_port <- 4000
  test_server <- NULL

  tryCatch({
    # Start a simple HTTP server on port 4000
    test_server <- httpuv::startServer(
      host = "127.0.0.1",
      port = test_port,
      app = list(
        call = function(req) {
          list(status = 200, headers = list("Content-Type" = "text/plain"), body = "test")
        }
      )
    )

    # Give it a moment to start
    Sys.sleep(0.5)

    # Verify port is in use
    if (!is_port_available("127.0.0.1", test_port)) {
      stop("Test server didn't start - port 4000 is not in use")
    }

    cat(sprintf("  Test server started on port %d\n", test_port))

    # Now assign ports starting from 4000
    config3 <- ShinyServerConfig$new()
    config3$config <- list(
      proxy_port = 3838,
      management_port = 3839,
      starting_port = test_port, # Start at occupied port
      apps = list(
        list(name = "app1", path = "/tmp/app1", resident = TRUE),
        list(name = "app2", path = "/tmp/app2", resident = TRUE)
      )
    )

    config3$assign_app_ports()

    # Verify apps didn't get assigned the in-use port
    for (app in config3$config$apps) {
      if (app$port == test_port) {
        stop(sprintf("App '%s' assigned in-use port %d", app$name, app$port))
      }
      cat(sprintf("  App '%s' assigned port %d (skipped in-use port %d)\n",
                  app$name, app$port, test_port))
    }

    cat("  PASS: Port assignment correctly skips ports in use\n\n")

  }, finally = {
    # Clean up test server
    if (!is.null(test_server)) {
      httpuv::stopServer(test_server)
      cat("  Test server stopped\n\n")
    }
  })

  # Test 4: Duplicate detection still works
  cat("Test 4: Validation catches duplicate port assignments...\n")
  config4 <- ShinyServerConfig$new()
  config4$config <- list(
    proxy_port = 3838,
    management_port = 3839,
    starting_port = 4100,
    apps = list(
      list(name = "app1", path = "/tmp/app1", resident = TRUE, port = 5000),
      list(name = "app2", path = "/tmp/app2", resident = TRUE, port = 5000) # Duplicate!
    )
  )

  error_caught <- tryCatch({
    config4$validate_port_assignments()
    FALSE  # No error - return FALSE
  }, error = function(e) {
    if (grepl("Port conflict detected", e$message)) {
      cat("  Expected error caught:", e$message, "\n")
      TRUE  # Expected error - return TRUE
    } else {
      stop("Wrong error: ", e$message)
    }
  })

  if (!error_caught) {
    stop("Duplicate port validation failed - no error raised")
  }
  cat("  PASS: Duplicate ports are detected\n\n")

  cat("=== All Port Assignment Tests Passed ===\n")
}

# Run tests
test_port_assignment()
