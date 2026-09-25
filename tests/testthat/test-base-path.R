# Tests for serving the proxy under a public URL prefix (base_path)

base_path_config <- function(base_path = "/shiny", port = 3001) {
  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(list(name = "app", port = port, resident = TRUE, appstart_timeout = 2)),
    base_path = base_path
  )
  config
}

base_path_request <- function(path, query = "") {
  list(PATH_INFO = path, REQUEST_METHOD = "GET", QUERY_STRING = query, HTTP_HOST = "127.0.0.1:3838")
}

test_that("validate_base_path normalizes accepted prefixes", {
  for (case in list(c("", ""), c("/", ""), c("/shiny", "/shiny"), c("/shiny/", "/shiny"),
    c("/team-a/apps_v2.0~x", "/team-a/apps_v2.0~x"))) {
    result <- validate_base_path(case[1])
    expect_true(result$valid, info = case[1])
    expect_identical(result$sanitized, case[2])
  }
})

test_that("validate_base_path rejects unsafe or ambiguous prefixes", {
  for (value in list(NULL, NA_character_, 1, c("/a", "/b"), "shiny", "//", "/a//b", "/a/../b",
    "/./a", "/a b", "/a\"b", "/a<b", "/a?b", "/a%20b", paste0("/", strrep("a", 100)))) {
    expect_false(validate_base_path(value)$valid, info = paste(format(value), collapse = " "))
  }
  for (reserved in c("/proxy", "/api/x", "/templates", "/health", "/Proxy")) {
    result <- validate_base_path(reserved)
    expect_false(result$valid, info = reserved)
    expect_match(result$error, "already uses")
  }
})

test_that("configuration defaults and normalizes base_path", {
  config <- ShinyServerConfig$new()
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  settings <- list(apps = list(list(name = "app", path = "/tmp")), log_dir = tempdir(), starting_port = 3001)
  writeLines(jsonlite::toJSON(settings, auto_unbox = TRUE), path)
  expect_identical(config$read_config(path)$base_path, "")

  settings$base_path <- "/shiny/"
  writeLines(jsonlite::toJSON(settings, auto_unbox = TRUE), path)
  expect_identical(config$read_config(path)$base_path, "/shiny")

  settings$base_path <- "/api"
  writeLines(jsonlite::toJSON(settings, auto_unbox = TRUE), path)
  expect_error(config$read_config(path), "base_path may not start with /api")
})

test_that("strip_base_path removes only a whole leading prefix", {
  expect_identical(strip_base_path("/shiny/proxy/app/", "/shiny"), "/proxy/app/")
  expect_identical(strip_base_path("/shiny/", "/shiny"), "/")
  expect_identical(strip_base_path("/proxy/app/", "/shiny"), "/proxy/app/")
  expect_identical(strip_base_path("/shinyapp/x", "/shiny"), "/shinyapp/x")
  expect_identical(strip_base_path("/shiny/proxy/app/", ""), "/shiny/proxy/app/")
  expect_null(strip_base_path(NULL, "/shiny"))
})

test_that("requests are routed with or without the prefix", {
  config <- base_path_config()
  for (path in c("/shiny/health", "/health")) {
    result <- handle_http_request(base_path_request(path), config, list(), list())
    expect_equal(result$status, 200, info = path)
  }
  expect_equal(handle_http_request(base_path_request("/shinyx/health"), config, list(), list())$status, 404)
})

test_that("the bare prefix redirects to its trailing-slash form", {
  config <- base_path_config()
  result <- handle_http_request(base_path_request("/shiny", "?a=1"), config, list(), list())
  expect_equal(result$status, 308)
  expect_equal(result$headers$Location, "/shiny/?a=1")
})

test_that("app mount redirects include the prefix", {
  config <- base_path_config()
  for (path in c("/shiny/proxy/app", "/proxy/app")) {
    result <- handle_http_request(base_path_request(path, "x=1"), config, list(), list())
    expect_equal(result$status, 308, info = path)
    expect_equal(result$headers$Location, "/shiny/proxy/app/?x=1", info = path)
  }
})

test_that("backend redirects and cookie paths are rewritten under the prefix", {
  headers <- charToRaw(paste0(
    "HTTP/1.1 302 Found\r\nLocation: http://127.0.0.1:3001/login?next=%2F\r\n",
    "Set-Cookie: a=1; Path=/; HttpOnly\r\nSet-Cookie: b=2\r\n\r\n"
  ))
  result <- proxy_response_headers(headers, "http://127.0.0.1:3001/folder/page", "app", "GET", "/shiny")
  expect_equal(result$location, "/shiny/proxy/app/login?next=%2F")
  expect_equal(unname(unlist(result[names(result) == "set-cookie"])),
    c("a=1; Path=/shiny/proxy/app/; HttpOnly", "b=2; Path=/shiny/proxy/app/folder"))
})

test_that("proxied HTTP responses carry the prefix whichever way the request arrives", {
  backend <- start_test_http_server(function(req) {
    list(status = 302L, headers = list(Location = "/next", "Set-Cookie" = "sid=1; Path=/"), body = "")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- base_path_config(port = backend$port)
  config$add_app_process("app", test_backend_process())
  cm <- ConnectionManager$new(config)

  for (path in c("/shiny/proxy/app/page", "/proxy/app/page")) {
    req <- base_path_request(path)
    result <- await_response(handle_http_request(req, config, list(), cm))
    expect_equal(result$status, 302, info = path)
    expect_equal(result$headers$location, "/shiny/proxy/app/next", info = path)
    expect_equal(result$headers$`set-cookie`, "sid=1; Path=/shiny/proxy/app/", info = path)
  }
})

test_that("WebSocket sessions are routed with or without the prefix", {
  backend <- start_test_http_server(function(req) handle_health_check(), on_ws = function(ws) {
    ws$onMessage(function(binary, message) ws$send(ws$request$PATH_INFO))
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- base_path_config(port = backend$port)
  config$add_app_process("app", test_backend_process())
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) handle_health_check(),
    on_ws = function(ws) handle_websocket_connection(ws, config, cm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  clients <- list()
  on.exit(lapply(clients, function(client) client$close()), add = TRUE, after = FALSE)

  for (path in c("/shiny/proxy/app/websocket/", "/proxy/app/websocket/")) {
    client <- websocket::WebSocket$new(paste0(sub("^http", "ws", proxy$url), path))
    clients[[length(clients) + 1L]] <- client
    response <- promises::promise(function(resolve, reject) {
      client$onOpen(function(event) client$send("init"))
      client$onMessage(function(event) resolve(event$data))
      client$onError(function(event) reject(simpleError(event$message)))
    })
    expect_identical(await_response(response), "/websocket/", info = path)
  }
})

test_that("the landing page links and fetches under the prefix", {
  tm <- create_template_manager(base_url = "/shiny")
  html <- tm$generate_landing_page(base_path_config())
  expect_match(html, 'href="/shiny/templates/styles/main.css"', fixed = TRUE)
  expect_match(html, 'const baseUrl = "/shiny";', fixed = TRUE)
  expect_match(html, 'baseUrl + "/proxy/"', fixed = TRUE)
  expect_match(html, 'fetch(baseUrl + "/api/apps"', fixed = TRUE)
})

test_that("the management page uses only relative URLs", {
  html <- create_template_manager(base_url = "/shiny")$generate_management_page()
  expect_false(grepl("/shiny", html, fixed = TRUE))
  expect_false(grepl('(src|href)="/', html))
  expect_false(grepl('fetch\\("/', html))
  expect_match(html, 'href="templates/styles/main.css"', fixed = TRUE)
  expect_match(html, 'fetch("api/status")', fixed = TRUE)
})
