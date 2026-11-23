# Test for HTTP routing and handler functions
# Tests routing logic and basic handler functions

# ============================================================================
# handle_health_check() tests
# ============================================================================

test_that("handle_health_check returns healthy status", {
  result <- handle_health_check()

  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  # Parse JSON body
  body <- jsonlite::fromJSON(result$body)
  expect_equal(body$status, "healthy")
})

# ============================================================================
# parse_proxy_path() tests
# ============================================================================

test_that("parse_proxy_path splits the app name from the path the app sees", {
  cases <- list(
    list("/proxy/myapp/websocket/", "myapp", "/websocket/"),
    list("/proxy/my-app/some/path", "my-app", "/some/path"),
    list("/proxy/my_app_123/", "my_app_123", "/"),
    list("/proxy/ValidApp123/doc.Rmd/__sockjs__/", "ValidApp123", "/doc.Rmd/__sockjs__/"),
    # The app name ends the path
    list("/proxy/myapp", "myapp", ""),
    # Later slashes belong to the app
    list("/proxy/myapp//x", "myapp", "//x")
  )
  for (case in cases) {
    route <- parse_proxy_path(case[[1]])
    expect_true(route$valid, info = case[[1]])
    expect_equal(route$app_name, case[[2]], info = case[[1]])
    expect_equal(route$backend_path, case[[3]], info = case[[1]])
  }
})

test_that("parse_proxy_path rejects paths that name no app", {
  for (path in list(NULL, NA_character_, 1, c("/proxy/a/", "/proxy/b/"), "", "/", "/health", "/proxy",
    "/proxy/", "/proxy//myapp", "/proxy//myapp/websocket/", "/other/proxy/myapp/")) {
    route <- parse_proxy_path(path)
    expect_false(route$valid, info = paste(format(path), collapse = " "))
    expect_match(route$error, "Invalid proxy path")
  }
})

test_that("parse_proxy_path rejects invalid app names", {
  for (path in c("/proxy/my app/ws", "/proxy/my@app/ws", "/proxy/invalid<script>/ws", "/proxy/../etc",
    paste0("/proxy/", strrep("a", 51), "/"))) {
    route <- parse_proxy_path(path)
    expect_false(route$valid, info = path)
    expect_match(route$error, "Invalid app name")
  }
})

# ============================================================================
# route_http_request() tests
# ============================================================================

# Mock template manager for testing
create_mock_template_manager <- function() {
  list(
    generate_landing_page = function(config) {
      return("<html><body>Landing Page</body></html>")
    },
    serve_static_file = function(file_path, if_none_match = NULL) {
      if (file_path == "style.css") {
        return(list(
          status = 200,
          headers = list("Content-Type" = "text/css"),
          body = "body { margin: 0; }"
        ))
      }
      return(list(
        status = 404,
        headers = list("Content-Type" = "text/plain"),
        body = "Not Found"
      ))
    }
  )
}

# Mock connection manager for testing
create_mock_connection_manager <- function() {
  list()
}

test_that("route_http_request routes to landing page", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  # Test root path
  result <- route_http_request("/", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "text/html")
  expect_match(result$body, "Landing Page")

  # Test empty path
  result <- route_http_request("", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_match(result$body, "Landing Page")
})

test_that("route_http_request routes to health check", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  result <- route_http_request("/health", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  body <- jsonlite::fromJSON(result$body)
  expect_equal(body$status, "healthy")
})

test_that("route_http_request routes to apps API", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = FALSE)
    )
  )
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  result <- route_http_request("/api/apps", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  body <- jsonlite::fromJSON(result$body)
  expect_true("app1" %in% names(body))
})

test_that("route_http_request routes to static files", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  result <- route_http_request("/templates/style.css", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "text/css")
  expect_match(result$body, "margin")
})

test_that("route_http_request returns 404 for unknown paths", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  result <- route_http_request("/unknown/path", "GET", NULL, list(), config, template_manager, connection_manager)
  expect_equal(result$status, 404)
  expect_match(result$body, "Not Found")
})

# ============================================================================
# handle_landing_page() tests
# ============================================================================

test_that("handle_landing_page returns HTML response", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()

  result <- handle_landing_page(config, template_manager)

  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "text/html")
  expect_match(result$body, "<html>")
  expect_match(result$body, "Landing Page")
})

test_that("handle_landing_page handles template errors", {
  config <- ShinyServerConfig$new()

  # Mock template manager that throws error
  bad_template_manager <- list(
    generate_landing_page = function(config) {
      stop("Template error")
    }
  )

  result <- handle_landing_page(config, bad_template_manager)

  expect_equal(result$status, 500)
  expect_match(result$body, "Internal Server Error")
})

# ============================================================================
# handle_apps_api() tests
# ============================================================================

test_that("handle_apps_api returns app status without process manager", {
  app_dir <- make_test_app_dir()
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path/to/app1", port = 3001, resident = TRUE),
      list(name = "app2", path = app_dir, port = 3002, resident = FALSE),
      list(name = "app3", path = "/path/to/app3", port = 3003, resident = FALSE)
    )
  )

  result <- handle_apps_api(config, process_manager = NULL)

  expect_equal(result$status, 200)
  expect_equal(result$headers$`Content-Type`, "application/json")

  body <- jsonlite::fromJSON(result$body)

  # Check app1
  expect_true("app1" %in% names(body))
  expect_equal(body$app1$name, "app1")
  expect_equal(body$app1$status, "stopped")
  expect_true(body$app1$resident)
  expect_equal(body$app1$connections, 0)
  expect_null(body$app1$port)
  expect_null(body$app1$path)

  # Check app2
  expect_true("app2" %in% names(body))
  expect_equal(body$app2$name, "app2")
  expect_equal(body$app2$status, "dormant")
  expect_false(body$app2$resident)
  expect_null(body$app2$port)

  # The landing page does not offer an on-demand app that could not start
  expect_equal(body$app3$status, "unavailable")
})

test_that("handle_apps_api sorts apps alphabetically", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "zebra", path = "/z", port = 3003, resident = FALSE),
      list(name = "apple", path = "/a", port = 3001, resident = FALSE),
      list(name = "mango", path = "/m", port = 3002, resident = FALSE)
    )
  )

  result <- handle_apps_api(config, process_manager = NULL)
  body <- jsonlite::fromJSON(result$body, simplifyVector = FALSE)

  # Get app names in order they appear in response
  app_names <- names(body)

  # Should be in alphabetical order
  expect_equal(app_names, c("apple", "mango", "zebra"))
})

test_that("handle_apps_api uses process manager when available", {
  config <- ShinyServerConfig$new()

  # Mock process manager
  mock_pm <- list(
    get_all_app_status = function() {
      return(list(
        app1 = list(
          name = "app1",
          status = "running",
          resident = TRUE,
          port = 3001,
          connections = 5,
          pid = 12345
        )
      ))
    }
  )

  result <- handle_apps_api(config, process_manager = mock_pm)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)

  expect_equal(body$app1$name, "app1")
  expect_equal(body$app1$status, "running")
  expect_equal(body$app1$connections, 5)
  # Internal details stay on the management API
  expect_null(body$app1$pid)
  expect_null(body$app1$port)
})

# ============================================================================
# handle_http_request() tests
# ============================================================================

test_that("handle_http_request validates request inputs", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  # Invalid method
  req <- list(
    PATH_INFO = "/",
    REQUEST_METHOD = "INVALID_METHOD",
    QUERY_STRING = ""
  )

  result <- handle_http_request(req, config, template_manager, connection_manager)

  expect_equal(result$status, 501)
})

test_that("handle_http_request routes valid requests", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  req <- list(
    PATH_INFO = "/health",
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_http_request(req, config, template_manager, connection_manager)

  expect_equal(result$status, 200)
  body <- jsonlite::fromJSON(result$body)
  expect_equal(body$status, "healthy")
})

test_that("an unexpected error is answered with HTTP 500 and no detail", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  req <- list(PATH_INFO = "/health", REQUEST_METHOD = "GET", QUERY_STRING = "")

  # httpuv would send the client the R error message
  local_mocked_bindings(route_http_request = function(...) stop("cannot open C:/srv/secret/file"))
  result <- handle_http_request(req, config, create_mock_template_manager(), create_mock_connection_manager())

  expect_equal(result$status, 500)
  expect_equal(result$headers$`Content-Type`, "application/json")
  expect_match(result$body, "Internal Server Error")
  expect_no_match(as.character(result$body), "secret")
})

test_that("handle_http_request rejects overly long paths", {
  config <- ShinyServerConfig$new()
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  long_path <- paste0("/", paste(rep("a", 1001), collapse = ""))

  req <- list(
    PATH_INFO = long_path,
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_http_request(req, config, template_manager, connection_manager)

  expect_equal(result$status, 400)
})

test_that("handle_http_request attaches managers to request", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "app1", path = "/path", port = 3001, resident = TRUE)
    )
  )
  template_manager <- create_mock_template_manager()
  connection_manager <- create_mock_connection_manager()

  mock_pm <- list(
    get_all_app_status = function() {
      list(app1 = list(name = "app1", status = "running"))
    }
  )

  req <- list(
    PATH_INFO = "/api/apps",
    REQUEST_METHOD = "GET",
    QUERY_STRING = ""
  )

  result <- handle_http_request(req, config, template_manager, connection_manager, mock_pm)

  expect_equal(result$status, 200)
})

# ============================================================================
# handle_static_file() tests
# ============================================================================

test_that("handle_static_file serves files via template manager", {
  template_manager <- create_mock_template_manager()

  result <- handle_static_file("/templates/style.css", template_manager)

  expect_equal(result$status, 200)
  expect_equal(result$headers[["Content-Type"]], "text/css")
})

test_that("handle_static_file returns 404 for missing files", {
  template_manager <- create_mock_template_manager()

  result <- handle_static_file("/templates/nonexistent.css", template_manager)

  expect_equal(result$status, 404)
})

test_that("handle_static_file strips /templates/ prefix", {
  # Track what file_path is passed to serve_static_file
  received_path <- NULL
  received_tag <- NULL
  template_manager <- list(
    serve_static_file = function(file_path, if_none_match = NULL) {
      received_path <<- file_path
      received_tag <<- if_none_match
      list(status = 200, headers = list("Content-Type" = "text/css"), body = "test")
    }
  )

  handle_static_file("/templates/styles/main.css", template_manager)

  expect_equal(received_path, "styles/main.css")
  expect_null(received_tag)

  # The browser's entity tag reaches the template manager from both servers
  req <- list(HTTP_IF_NONE_MATCH = "\"tag\"")
  route_http_request("/templates/styles/main.css", "GET", "", req, ShinyServerConfig$new(), template_manager, NULL)
  expect_equal(received_tag, "\"tag\"")
  received_tag <- NULL
  route_management_request("/templates/styles/main.css", "GET", req, ShinyServerConfig$new(), list(), template_manager)
  expect_equal(received_tag, "\"tag\"")
})

# ============================================================================
# handle_proxy_request() tests
# ============================================================================

test_that("handle_proxy_request rejects invalid app names", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  req <- list()

  # Path traversal attempt
  result <- handle_proxy_request("/proxy/../../../etc/passwd", "GET", NULL, req, config)

  expect_equal(result$status, 400)
  expect_match(result$body, "Invalid app name")
})

test_that("handle_proxy_request returns 404 for unknown apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  req <- list()

  result <- handle_proxy_request("/proxy/nonexistent/path", "GET", NULL, req, config)

  expect_equal(result$status, 404)
  expect_match(result$body, "App not found")
})

test_that("handle_proxy_request returns 400 for invalid path", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  req <- list()

  result <- handle_proxy_request("/proxy/", "GET", NULL, req, config)

  expect_equal(result$status, 400)
  expect_match(result$body, "Invalid proxy path")
})

test_that("handle_proxy_request starts non-resident app on demand", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "myapp", path = "/path", port = 3001, resident = FALSE)
    )
  )

  start_called <- FALSE
  mock_pm <- list(
    start_app_on_demand = function(app_name) {
      start_called <<- TRUE
      TRUE
    }
  )

  req <- list()

  local_mocked_bindings(wait_for_backend = function(url, wait_seconds) promises::promise_resolve(FALSE))
  result <- await_response(handle_proxy_request("/proxy/myapp/", "GET", NULL, req, config, mock_pm))

  # App start should have been called
  expect_true(start_called)
})

test_that("handle_proxy_request returns 502 when app fails to start", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "myapp", path = "/path", port = 3001, resident = FALSE)
    )
  )

  mock_pm <- list(
    start_app_on_demand = function(app_name) FALSE
  )

  req <- list()

  result <- handle_proxy_request("/proxy/myapp/", "GET", NULL, req, config, mock_pm)

  expect_equal(result$status, 502)
  expect_match(result$body, "Failed to start app")
})

test_that("a request for an app that failed moments ago is asked to retry after restart_delay", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "myapp", path = "/path", port = 3001, resident = FALSE)),
    restart_delay = 60)
  config$note_start_failure("myapp")
  pm <- ProcessManager$new(config)
  assign("start_app", function(app_config) stop("must wait for restart_delay"), envir = pm)
  tm <- create_template_manager()

  # A script receives JSON with Retry-After
  result <- handle_proxy_request("/proxy/myapp/", "GET", NULL, httpuv_request(list(accept = "*/*")), config, pm, NULL, tm)
  expect_equal(result$status, 503)
  expect_match(result$body, "restarting after a failure")
  retry <- as.numeric(result$headers[["Retry-After"]])
  expect_true(retry >= 59 && retry <= 60)

  # A browser receives the page that reloads itself, after the same delay
  browser <- httpuv_request(list(accept = "text/html,*/*;q=0.8"))
  page <- handle_proxy_request("/proxy/myapp/", "GET", NULL, browser, config, pm, NULL, tm)
  expect_equal(page$status, 503)
  expect_equal(page$headers[["Content-Type"]], "text/html")
  expect_match(page$body, paste0("var retryMs = ", page$headers[["Retry-After"]], " * 1000;"), fixed = TRUE)
})

test_that("an R Markdown app serves only its own document", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "reports", path = "/path", port = 3001, resident = TRUE)))
  config$set_app_document("reports", list(type = "rmd", file = "/path/report.Rmd"))
  # rmarkdown::run() would render any of these, running its code
  for (path in c("/_partial.Rmd", "/drafts/notes.Rmd", "/%5Fpartial.Rmd", "/notes.rmd", "/other.qmd",
                 "/%2e/report.Rmd")) {
    result <- handle_proxy_request(paste0("/proxy/reports", path), "GET", NULL, list(), config)
    expect_equal(result$status, 404, info = path)
  }
  forwarded <- character()
  local_mocked_bindings(forward_request = function(method, target_url, ...) {
    forwarded <<- c(forwarded, target_url)
    create_html_response("ok")
  })
  for (path in c("/", "/report.Rmd", "/REPORT.rmd", "/report_files/figure.png", "/shared/data.csv")) {
    expect_equal(handle_proxy_request(paste0("/proxy/reports", path), "GET", NULL, list(), config)$status, 200)
  }
  expect_length(forwarded, 5)
  # Other kinds of app serve such files as they are
  config$set_app_document("reports", list(type = "shiny"))
  expect_equal(handle_proxy_request("/proxy/reports/_partial.Rmd", "GET", NULL, list(), config)$status, 200)
})

test_that("request decisions read headers under the names the client sent", {
  # An underscore spelling replaces the HTTP_ entry, but not the header
  tm <- create_template_manager()
  first <- route_http_request("/templates/styles/main.css", "GET", "", httpuv_request(list(host = "localhost")),
    ShinyServerConfig$new(), tm, NULL)
  etag <- first$headers$ETag
  cached <- httpuv_request(list("if-none-match" = etag))
  expect_equal(route_http_request("/templates/styles/main.css", "GET", "", cached,
    ShinyServerConfig$new(), tm, NULL)$status, 304L)
  spoofed <- httpuv_request(list("if_none_match" = etag))
  expect_equal(spoofed$HTTP_IF_NONE_MATCH, etag)
  expect_equal(route_http_request("/templates/styles/main.css", "GET", "", spoofed,
    ShinyServerConfig$new(), tm, NULL)$status, 200)
  expect_equal(route_management_request("/templates/styles/main.css", "GET", spoofed,
    ShinyServerConfig$new(), list(), tm)$status, 200)

  # The proxy and management Host checks, and the WebSocket Origin check
  config <- ShinyServerConfig$new()
  config$config <- list(proxy_host = "127.0.0.1")
  expect_false(is_allowed_proxy_host(httpuv_request(list(host = "evil.example")), config))
  expect_true(is_allowed_proxy_host(httpuv_request(list(host = "localhost:3838")), config))
  expect_equal(handle_management_request(httpuv_request(list(host = "evil.example"),
    REQUEST_METHOD = "GET", PATH_INFO = "/"), config, list(), tm)$status, 403)
  expect_true(is_same_origin_websocket(httpuv_request(list(host = "localhost:3838",
    origin = "http://localhost:3838"))))
  expect_false(is_same_origin_websocket(httpuv_request(list(host = "localhost:3838",
    origin = "http://evil.example"))))
})

# ============================================================================
# forward_request() tests
# ============================================================================

test_that("forward_request returns 503 for starting apps", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  config$set_app_starting("testapp")
  local_mocked_bindings(wait_for_backend = function(url, wait_seconds) promises::promise_resolve(FALSE))
  result <- await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "testapp", config))
  expect_equal(result$status, 503)
  expect_match(result$body, "starting up")
  expect_true("Retry-After" %in% names(result$headers))
})

test_that("a browser opening an app that is not ready gets a page that reloads itself", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  config$set_app_starting("testapp")
  local_mocked_bindings(wait_for_backend = function(url, wait_seconds) promises::promise_resolve(FALSE))
  tm <- create_template_manager(base_url = "/shiny")
  url <- "http://127.0.0.1:3001/"
  browser <- list(HTTP_ACCEPT = "text/html,application/xhtml+xml,*/*;q=0.8")

  page <- await_response(forward_request("GET", url, browser, "testapp", config, tm))
  expect_equal(page$status, 503)
  expect_equal(page$headers[["Retry-After"]], "2")
  expect_equal(page$headers[["Content-Type"]], "text/html")
  expect_match(page$body, "<h1>testapp</h1>", fixed = TRUE)
  expect_match(page$body, "var retryMs = 2 * 1000;", fixed = TRUE)
  expect_match(page$body, "window.location.reload()", fixed = TRUE)
  expect_match(page$body, 'href="/shiny/templates/styles/main.css"', fixed = TRUE)

  # Scripts, other clients and other methods keep the JSON answer
  others <- list(
    list(method = "GET", req = list(HTTP_ACCEPT = "*/*"), tm = tm),
    list(method = "GET", req = list(), tm = tm),
    list(method = "GET", req = browser, tm = NULL),
    list(method = "POST", req = browser, tm = tm),
    list(method = "GET", req = browser, tm = list(
      generate_starting_page = function(...) stop("Template error")
    ))
  )
  for (case in others) {
    json <- await_response(forward_request(case$method, url, case$req, "testapp", config, case$tm))
    expect_equal(json$status, 503)
    expect_equal(json$headers[["Content-Type"]], "application/json")
    expect_match(json$body, "starting up")
  }

  # The one-off notice that startup is taking long must not end the reloading
  assign("testapp", list(
    state = "starting", started_at = Sys.time() - (config$APP_STARTUP_TIMEOUT_SECONDS + 5)
  ), envir = config$app_startup_state)
  late <- await_response(forward_request("GET", url, browser, "testapp", config, tm))
  expect_equal(late$status, 502)
  expect_equal(late$headers[["Content-Type"]], "text/html")
  expect_match(late$body, "window.location.reload()", fixed = TRUE)
})

test_that("handle_http_request passes the template manager to proxied requests", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "testapp", path = "/path", port = 3001, resident = TRUE)))
  seen <- NULL
  local_mocked_bindings(forward_request = function(method, target_url, req, app_name, config, template_manager = NULL) {
    seen <<- template_manager
    create_html_response("proxied")
  })
  req <- new.env()
  req$REQUEST_METHOD <- "GET"
  req$PATH_INFO <- "/proxy/testapp/page"
  req$QUERY_STRING <- ""
  req$HTTP_HOST <- "127.0.0.1:3838"
  tm <- create_mock_template_manager()
  result <- handle_http_request(req, config, tm, ConnectionManager$new(config))
  expect_equal(result$status, 200)
  expect_identical(seen, tm)
})

test_that("a startup that timed out is reported to one request while the app does not answer", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())
  assign("testapp", list(
    state = "starting", started_at = Sys.time() - (config$APP_STARTUP_TIMEOUT_SECONDS + 5)
  ), envir = config$app_startup_state)
  local_mocked_bindings(wait_for_backend = function(url, wait_seconds) promises::promise_resolve(FALSE))
  request <- function() {
    await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "testapp", config))
  }

  # A WebSocket handshake that asks first does not use the report up
  expect_false(config$is_app_starting("testapp"))
  result <- request()
  expect_equal(result$status, 502)
  expect_match(result$body, "timed out")

  expect_null(config$get_app_startup_state("testapp"))
  again <- request()
  expect_equal(again$status, 503)
  expect_match(again$body, "not ready")
})

test_that("an app that answers after its startup timed out is served without the report", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001)))
  config$add_app_process("app", test_backend_process())
  config$set_app_starting("app")
  config$set_app_start_timed_out("app")
  local_mocked_bindings(
    process_owns_port = function(process, port) TRUE,
    fetch_backend_async = function(url, handle, ...) promises::promise_resolve(list(
      status_code = 200L, headers = charToRaw("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n"),
      content = charToRaw("ready")
    ))
  )
  result <- await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "app", config))
  expect_equal(result$status, 200)
  expect_null(config$get_app_startup_state("app"))
})

test_that("the proxy's own pages and endpoints answer only GET and HEAD", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "testapp", path = "/path", port = 3001, resident = TRUE)))
  tm <- create_mock_template_manager()
  cm <- ConnectionManager$new(config)
  route <- function(path, method) route_http_request(path, method, NULL, list(), config, tm, cm)

  for (path in c("/", "/health", "/api/apps", "/templates/style.css")) {
    for (method in c("POST", "PUT", "PATCH", "DELETE", "OPTIONS")) {
      result <- route(path, method)
      expect_equal(result$status, 405, info = paste(method, path))
      expect_equal(result$headers[["Allow"]], "GET, HEAD", info = paste(method, path))
    }
    expect_equal(route(path, "GET")$status, 200, info = path)
    expect_equal(route(path, "HEAD")$status, 200, info = path)
  }
  # A path the proxy does not serve is still not found, whatever the method
  expect_equal(route("/unknown", "POST")$status, 404)
})

test_that("the proxy's own answers to HEAD keep their headers but send no body", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "testapp", path = "/path", port = 3001, resident = TRUE)))
  tm <- create_mock_template_manager()
  request <- function(path, method) {
    list(PATH_INFO = path, REQUEST_METHOD = method, QUERY_STRING = "", HTTP_HOST = "127.0.0.1:3838")
  }
  for (path in c("/", "/health", "/api/apps", "/templates/style.css", "/unknown", "/proxy/testapp")) {
    get <- handle_http_request(request(path, "GET"), config, tm, list())
    head <- handle_http_request(request(path, "HEAD"), config, tm, list())
    expect_equal(head$status, get$status, info = path)
    expect_equal(head$headers, get$headers, info = path)
    # httpuv sends a body element of NULL as no body, and compresses ""
    expect_true("body" %in% names(head), info = path)
    expect_null(head$body, info = path)
  }
})

test_that("an answer to HEAD that the proxy makes for a starting app has no body", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "testapp", path = "/path", port = 3001, resident = TRUE)))
  local_mocked_bindings(wait_for_backend = function(url, wait_seconds) promises::promise_resolve(FALSE))
  req <- list(PATH_INFO = "/proxy/testapp/", REQUEST_METHOD = "HEAD", QUERY_STRING = "", HTTP_HOST = "127.0.0.1:3838")
  result <- await_response(handle_http_request(req, config, NULL, ConnectionManager$new(config)))
  expect_equal(result$status, 503)
  expect_equal(result$headers[["Retry-After"]], "2")
  expect_null(result$body)
})

test_that("a connection that answered HEAD can carry the next request", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "testapp", path = "/path", port = 3001, resident = TRUE)))
  proxy <- start_test_http_server(function(req) handle_http_request(req, config, create_mock_template_manager(), list()))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  con <- socketConnection("127.0.0.1", proxy$port, blocking = FALSE, open = "r+b")
  on.exit(close(con), add = TRUE, after = FALSE)
  # Read until `until` arrives, and briefly after it for any bytes that follow
  exchange <- function(request, until) {
    writeBin(charToRaw(request), con)
    received <- raw()
    deadline <- Sys.time() + 5
    settle <- NULL
    while (Sys.time() < deadline && (is.null(settle) || Sys.time() < settle)) {
      later::run_now(0.01)
      received <- c(received, readBin(con, "raw", 65536))
      if (is.null(settle) && grepl(until, rawToChar(received), fixed = TRUE)) settle <- Sys.time() + 0.3
    }
    rawToChar(received)
  }

  head <- exchange("HEAD /health HTTP/1.1\r\nHost: 127.0.0.1\r\nAccept-Encoding: gzip\r\n\r\n", "\r\n\r\n")
  expect_match(head, "^HTTP/1.1 200")
  # Anything after the headers would be read as the start of the next answer
  expect_match(head, "\r\n\r\n$")
  get <- exchange("GET /health HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n", "healthy")
  expect_match(get, "^HTTP/1.1 200")
  expect_match(get, "healthy", fixed = TRUE)
})

test_that("forward_request returns 503 when backend is unavailable", {
  config <- ShinyServerConfig$new()
  local_mocked_bindings(wait_for_backend = function(url, wait_seconds) promises::promise_resolve(FALSE))
  result <- await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "testapp", config))
  expect_equal(result$status, 503)
  expect_match(result$body, "not ready")
})

# ============================================================================
# Integration tests for route_http_request with proxy
# ============================================================================

test_that("route_http_request routes proxy requests correctly", {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "testapp", path = "/path", port = 3001, resident = TRUE)
    )
  )
  template_manager <- create_mock_template_manager()
  connection_manager <- ConnectionManager$new(config)

  # Without a running app, this should return 503.
  local_mocked_bindings(
    wait_for_backend = function(url, wait_seconds) promises::promise_resolve(FALSE)
  )

  result <- await_response(route_http_request("/proxy/testapp/", "GET", NULL, list(), config, template_manager, connection_manager))

  expect_equal(result$status, 503)
})

test_that("forward_request honors each app's startup grace period", {
  for (case in list(
    list(timeout = NULL, elapsed = 3, polls = FALSE),
    list(timeout = 10, elapsed = 3, polls = TRUE),
    list(timeout = 0.5, elapsed = 1, polls = FALSE),
    list(timeout = 60, elapsed = 35, polls = TRUE)
  )) {
    config <- ShinyServerConfig$new()
    config$config <- list(apps = list(list(name = "app", port = 3001, appstart_timeout = case$timeout)))
    config$add_app_process("app", test_backend_process())
    assign("app", list(state = "starting", started_at = Sys.time() - case$elapsed),
      envir = config$app_startup_state)
    waits <- numeric()
    with_mocked_bindings(
      wait_for_backend = function(url, wait_seconds) {
        waits <<- c(waits, wait_seconds)
        promises::promise_resolve(TRUE)
      },
      process_owns_port = function(process, port) TRUE,
      fetch_backend_async = function(url, handle, ...) promises::promise_resolve(list(
        status_code = 200L, headers = charToRaw("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n"),
        content = charToRaw("ready")
      )),
      {
        result <- await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "app", config))
        if (case$polls) {
          expect_equal(result$status, 200)
          expect_null(config$get_app_startup_state("app"))
          expect_length(waits, 1)
          expect_equal(waits, case$timeout - case$elapsed, tolerance = 0.1)
        } else {
          expect_equal(result$status, 503)
          expect_match(result$body, "starting up")
          expect_length(waits, 0)
        }
      }
    )
  }
})

# ============================================================================
# Proxy routing safeguards
# ============================================================================

test_that("malformed proxy prefixes never start apps or form backend URLs", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  pm <- list(start_app_on_demand = function(...) stop("must not start"))
  local_mocked_bindings(forward_request = function(...) stop("must not forward"))
  for (path in c("/proxy//app/path", "/proxy///app/path", "//proxy/app/path")) {
    expect_equal(handle_proxy_request(path, "GET", "", list(), config, pm)$status, 400)
  }
})

test_that("root redirect does not launch a dormant app", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", resident = FALSE)))
  manager <- list(start_app_on_demand = function(...) stop("must not start"))
  response <- handle_proxy_request("/proxy/app", "GET", "x=1", list(), config, manager)
  expect_equal(response$status, 307)
  expect_equal(response$headers$Location, "/proxy/app/?x=1")
})

test_that("HTTP accounting releases rejected and synchronous responses", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 3001, resident = TRUE)))
  cm <- ConnectionManager$new(config)
  failure <- FALSE
  local_mocked_bindings(forward_request = function(...) {
    if (failure) return(promises::promise(function(resolve, reject) reject(simpleError("failure"))))
    create_503_response("starting")
  })
  expect_equal(handle_proxy_request("/proxy/app/", "GET", "", list(), config, connection_manager = cm)$status, 503)
  expect_equal(config$active_http_requests$app, 0)
  failure <- TRUE
  expect_error(await_response(handle_proxy_request("/proxy/app/", "GET", "", list(), config, connection_manager = cm)), "failure")
  expect_equal(config$active_http_requests$app, 0)
})

test_that("a loopback proxy rejects rebound Host names", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = 1, resident = FALSE)))
  pm <- list(start_app_on_demand = function(...) stop("must not start"))
  closed <- 0
  for (host in c("attacker.example:3838", "attacker.example", "127.0.0.1.attacker.example")) {
    req <- list(PATH_INFO = "/proxy/app/", REQUEST_METHOD = "GET", HTTP_HOST = host)
    expect_equal(handle_http_request(req, config, NULL, NULL, pm)$status, 403)
    ws <- list(request = list(PATH_INFO = "/proxy/app/websocket/", HTTP_HOST = host,
      HTTP_ORIGIN = paste0("http://", host)),
      close = function() closed <<- closed + 1,
      onMessage = function(...) stop("Rebound socket must not register callbacks"))
    handle_websocket_connection(ws, config, ConnectionManager$new(config), pm)
  }
  expect_equal(closed, 3)
  expect_length(config$get_all_ws_connections(), 0)
  req <- list(PATH_INFO = "/health", REQUEST_METHOD = "GET", HTTP_HOST = "localhost:3838")
  expect_equal(handle_http_request(req, config, NULL, NULL, pm)$status, 200)
  # A proxy deliberately bound to all interfaces serves any host name
  config$config$proxy_host <- "0.0.0.0"
  req <- list(PATH_INFO = "/health", REQUEST_METHOD = "GET", HTTP_HOST = "server.internal:3838")
  expect_equal(handle_http_request(req, config, NULL, NULL, pm)$status, 200)
})

test_that("an R Markdown app refuses other documents and R scripts however the path is spelled", {
  document <- list(type = "rmd", file = file.path(tempdir(), "report.Rmd"))
  for (path in c("/_partial.Rmd", "/helper.R", "/drafts/notes.Rmd/", "/%5Fpartial.Rmd", "/./report.Rmd")) {
    expect_true(is_other_document_path(document, path), info = path)
  }
  for (path in c("/", "", "/report.Rmd", "/REPORT.rmd", "/report_files/figure.png", "/data.csv", "/websocket/")) {
    expect_false(is_other_document_path(document, path), info = path)
  }
  # rmarkdown::run() serves Quarto documents too, and renders other files
  # for them in the same way unless they are prerendered
  quarto <- list(type = "qmd", file = file.path(tempdir(), "dash.qmd"))
  expect_true(is_other_document_path(quarto, "/helper.R/websocket/", websocket = TRUE))
  expect_true(is_other_document_path(quarto, "/_other.qmd"))
  expect_false(is_other_document_path(quarto, "/websocket/", websocket = TRUE))
  expect_false(is_other_document_path(quarto, "/dash_files/libs/app.js"))
  # Shiny apps serve such files as they are
  expect_false(is_other_document_path(list(type = "shiny"), "/helper.R"))
  expect_false(is_other_document_path(NULL, "/helper.R"))
})

test_that("a document's name in another letter case names it only where no other file has that name", {
  app_dir <- tempfile("tss-rmd-case")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  writeLines("", file.path(app_dir, "report.Rmd"))
  document <- list(type = "rmd", file = file.path(app_dir, "report.Rmd"))
  expect_true(is_document_name("/report.Rmd", document$file))
  expect_true(is_document_name("/REPORT.rmd", document$file))
  expect_false(is_document_name("/other.Rmd", document$file))
  # On a file system that tells letter case apart, this is another document
  writeLines("", file.path(app_dir, "Report.Rmd"))
  if (length(list.files(app_dir)) == 2) {
    expect_false(is_document_name("/Report.Rmd", document$file))
    expect_true(is_other_document_path(document, "/Report.Rmd/websocket/", websocket = TRUE))
  }
})

test_that("a request for another document does not start a dormant R Markdown app", {
  app_dir <- tempfile("tss-rmd")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)
  writeLines(c("---", "runtime: shiny", "---"), file.path(app_dir, "report.Rmd"))
  writeLines("", file.path(app_dir, "_partial.Rmd"))
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "reports", path = app_dir, port = 3001, resident = FALSE)))
  starts <- 0
  pm <- list(start_app_on_demand = function(app_name) {
    starts <<- starts + 1
    FALSE
  })

  # Nothing would stop an app started for a request that is then refused
  result <- handle_proxy_request("/proxy/reports/_partial.Rmd", "GET", NULL, list(), config, pm)
  expect_equal(result$status, 404)
  expect_equal(starts, 0)

  # The app's own page starts it
  handle_proxy_request("/proxy/reports/", "GET", NULL, list(), config, pm)
  expect_equal(starts, 1)
})

test_that("a document refused after a start leaves the started app to its idle check", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "reports", path = tempdir(), port = 3001, resident = FALSE)))
  # The directory held a Shiny app, and holds a document by the time it starts
  config$set_app_document("reports", list(type = "shiny"))
  pm <- list(start_app_on_demand = function(app_name) {
    config$set_app_document(app_name, list(type = "rmd", file = file.path(tempdir(), "report.Rmd")))
    TRUE
  })
  checks <- character()
  cm <- list(schedule_session_check = function(app_name, ...) checks <<- c(checks, app_name))
  result <- handle_proxy_request("/proxy/reports/_partial.Rmd", "GET", NULL, list(), config, pm, cm)
  expect_equal(result$status, 404)
  expect_equal(checks, "reports")
})

test_that("the public apps API names the current base_path for an open landing page", {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", path = tempdir(), port = 3001, resident = TRUE)),
    base_path = "/shiny")
  pm <- list(get_all_app_status = function() list(app = list(name = "app", status = "running")))
  expect_equal(handle_apps_api(config, pm)$headers[["X-TinyShinyServer-Base-Path"]], "/shiny")
  config$config$base_path <- ""
  expect_equal(handle_apps_api(config, pm)$headers[["X-TinyShinyServer-Base-Path"]], "")
})

test_that("header values that are not UTF-8 are ignored or refused, not errors", {
  req <- httpuv_request(list(connection = "keep-alive, X-\xe9, X-Drop", "x-drop" = "1", "x-name" = "caf\xe9"))
  headers <- forwarded_request_headers(req)
  expect_false("X-DROP" %in% names(headers))
  expect_equal(headers[["X-NAME"]], "caf\xe9")
  expect_false(is_loopback_host_header("localhost\xe9"))
  expect_false(etag_matches("\"a\xe9\"", "\"a\""))
  expect_false(is_same_origin_websocket(httpuv_request(list(origin = "http://h\xe9", host = "h"))))
  expect_true(is_same_origin_websocket(httpuv_request(list(origin = "http://h", host = "h",
    "x-forwarded-proto" = "\xe9"))))
})
