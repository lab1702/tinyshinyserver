test_that("management mutation guard rejects simple browser requests before side effects", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempfile())
  dir.create(config$config$log_dir)
  on.exit(unlink(config$config$log_dir, recursive = TRUE), add = TRUE)
  restarts <- 0
  pm <- list(get_app_status = function(name) list(status = "running"),
    restart_app = function(name) { restarts <<- restarts + 1; list(success = TRUE) })
  for (origin in c("http://127.0.0.1:9999", "http://localhost:3839", "https://evil.example", "null")) {
    for (header in list(NULL, "", "wrong", c("management", "wrong"))) {
      for (path in c("/api/shutdown", "/api/reload", "/api/apps/app/restart")) {
        req <- list(PATH_INFO = path, REQUEST_METHOD = "POST", HTTP_ORIGIN = origin,
          HTTP_X_TINYSHINYSERVER_REQUEST = header, CONTENT_TYPE = "application/x-www-form-urlencoded")
        expect_equal(handle_management_request(req, config, pm, NULL)$status, 403)
      }
    }
  }
  expect_equal(restarts, 0)
  expect_false(config$shutdown_requested)
  expect_false(config$reload_requested)
  expect_equal(route_management_request("/api/shutdown", "POST", list(), config, pm, NULL)$status, 403)
})

test_that("management custom-header clients work and preflight grants no access", {
  config <- ShinyServerConfig$new()
  config$config <- list(log_dir = tempfile())
  dir.create(config$config$log_dir)
  on.exit(unlink(config$config$log_dir, recursive = TRUE), add = TRUE)
  restarts <- 0
  pm <- list(get_app_status = function(name) list(status = "running"),
    restart_app = function(name) { restarts <<- restarts + 1; list(success = TRUE) })
  # No Origin models curl; the HTTPS origin models the documented reverse proxy.
  for (origin in list(NULL, "http://localhost:3839", "https://manage.myapp.example.com")) {
    req <- list(PATH_INFO = "/api/apps/app/restart", REQUEST_METHOD = "POST",
      HTTP_ORIGIN = origin, HTTP_X_TINYSHINYSERVER_REQUEST = "management")
    expect_equal(handle_management_request(req, config, pm, NULL)$status, 200)
  }
  expect_equal(restarts, 3)
  req <- list(PATH_INFO = "/api/shutdown", REQUEST_METHOD = "OPTIONS",
    HTTP_ORIGIN = "http://evil.example", HTTP_ACCESS_CONTROL_REQUEST_METHOD = "POST",
    HTTP_ACCESS_CONTROL_REQUEST_HEADERS = "x-tinyshinyserver-request")
  response <- handle_management_request(req, config, pm, NULL)
  expect_false(any(tolower(names(response$headers)) == "access-control-allow-origin"))
  expect_false(config$shutdown_requested)
  req$REQUEST_METHOD <- "POST"
  req$HTTP_X_TINYSHINYSERVER_REQUEST <- "management"
  expect_equal(handle_management_request(req, config, pm, NULL)$status, 200)
  expect_true(config$shutdown_requested)
})
