start_test_http_server <- function(call, on_ws = NULL) {
  for (port in sample(20000:50000, 3)) {
    server <- tryCatch(httpuv::startServer("127.0.0.1", port, list(call = call, onWSOpen = on_ws), quiet = TRUE),
      error = function(e) NULL)
    if (!is.null(server)) return(list(server = server, port = port, url = paste0("http://127.0.0.1:", port)))
  }
  skip("Loopback sockets unavailable")
}

proxy_test_config <- function(port, timeout = 2) {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = port, resident = TRUE, appstart_timeout = timeout)))
  config
}

test_that("proxy cookies belong only to the current browser request", {
  backend <- start_test_http_server(function(req) {
    headers <- list("Content-Type" = "text/plain")
    if (req$PATH_INFO == "/set") headers$`Set-Cookie` <- "session=alice; Path=/"
    list(status = 200L, headers = headers, body = req$HTTP_COOKIE %||% "NONE")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  request <- function(path, cookie = NULL, method = "GET") {
    await_response(forward_request(method, paste0(backend$url, path), list(HTTP_COOKIE = cookie), "app", config))
  }
  expect_equal(request("/set")$status, 200)
  expect_equal(request("/echo")$body, "NONE")
  expect_equal(request("/echo", "session=bob")$body, "session=bob")
  expect_equal(request("/echo", "session=carol", "POST")$body, "session=carol")
  expect_equal(request("/echo", method = "POST")$body, "NONE")
  responses <- await_response(promises::promise_all(
    alice = forward_request("GET", paste0(backend$url, "/echo"), list(HTTP_COOKIE = "session=alice"), "app", config),
    bob = forward_request("GET", paste0(backend$url, "/echo"), list(HTTP_COOKIE = "session=bob"), "app", config),
    guest = forward_request("GET", paste0(backend$url, "/echo"), list(), "app", config)
  ))
  expect_equal(responses$alice$body, "session=alice")
  expect_equal(responses$bob$body, "session=bob")
  expect_equal(responses$guest$body, "NONE")
})

test_that("async proxy preserves methods, binary uploads, content types and HEAD", {
  observed <- list()
  backend <- start_test_http_server(function(req) {
    observed[[length(observed) + 1]] <<- list(
      method = req$REQUEST_METHOD, type = req$CONTENT_TYPE %||% req$HTTP_CONTENT_TYPE,
      marker = req$HTTP_X_TEST, body = req$rook.input$read()
    )
    list(status = 201L, headers = list("Content-Type" = "application/octet-stream"), body = as.raw(c(0, 128, 255)))
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  payload <- as.raw(c(0, 1, 128, 255))
  for (method in c("POST", "PUT", "DELETE")) {
    response <- await_response(forward_request(method, backend$url,
      list(HTTP_X_TEST = "legitimate", HTTP_CONTENT_TYPE = "application/custom",
        rook.input = list(read = function() payload)), "app", config))
    expect_equal(response$status, 201)
    expect_identical(response$body, as.raw(c(0, 128, 255)))
    last <- tail(observed, 1)[[1]]
    expect_equal(last$method, method)
    expect_equal(last$type, "application/custom")
    expect_equal(last$marker, "legitimate")
    expect_identical(last$body, payload)
  }
  response <- await_response(forward_request("HEAD", backend$url, list(), "app", config))
  expect_equal(response$status, 201)
  expect_length(response$body, 0)
  expect_equal(tail(observed, 1)[[1]]$method, "HEAD")
  response <- await_response(forward_request("OPTIONS", backend$url, list(), "app", config))
  expect_equal(response$status, 201)
  expect_equal(tail(observed, 1)[[1]]$method, "OPTIONS")
})

test_that("slow backend responses do not block unrelated HTTP requests", {
  backend <- start_test_http_server(function(req) promises::promise(function(resolve, reject) {
    later::later(function() resolve(create_html_response("slow result")), 0.3)
  }))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  proxy <- start_test_http_server(function(req) {
    if (req$PATH_INFO == "/health") return(handle_health_check())
    forward_request("GET", backend$url, req, "app", config)
  }, on_ws = function(ws) {
    ws$onMessage(function(binary, message) ws$send(message))
  })
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  completed <- character()
  slow <- promises::then(fetch_backend_async(paste0(proxy$url, "/slow"), curl::new_handle()), function(x) {
    completed <<- c(completed, "slow"); x
  })
  health <- promises::promise(function(resolve, reject) {
    later::later(function() resolve(fetch_backend_async(paste0(proxy$url, "/health"), curl::new_handle())), 0.05)
  })
  health <- promises::then(health, function(x) {completed <<- c(completed, "health"); x})
  ws <- NULL
  on.exit(if (!is.null(ws)) ws$close(), add = TRUE)
  websocket_response <- promises::promise(function(resolve, reject) {
    later::later(function() {
      ws <<- websocket::WebSocket$new(sub("^http", "ws", proxy$url))
      ws$onOpen(function(event) ws$send("still responsive"))
      ws$onMessage(function(event) {
        completed <<- c(completed, "websocket")
        resolve(event$data)
      })
      ws$onError(function(event) reject(simpleError(event$message)))
    }, 0.05)
  })
  result <- await_response(promises::promise_all(slow = slow, health = health, ws = websocket_response))
  expect_equal(tail(completed, 1), "slow")
  expect_setequal(head(completed, 2), c("health", "websocket"))
  expect_equal(result$ws, "still responsive")
  expect_equal(rawToChar(result$slow$content), "slow result")
  expect_equal(result$health$status_code, 200)
})

test_that("startup polling yields and sends a queued POST only once", {
  posts <- 0
  backend <- start_test_http_server(function(req) {
    posts <<- posts + 1
    create_html_response("started")
  })
  httpuv::stopServer(backend$server)
  restarted <- NULL
  on.exit(if (!is.null(restarted)) httpuv::stopServer(restarted), add = TRUE)
  config <- proxy_test_config(backend$port, timeout = 1)
  config$set_app_starting("app")
  later::later(function() {
    restarted <<- httpuv::startServer("127.0.0.1", backend$port, list(call = function(req) {
      posts <<- posts + 1
      create_html_response("started")
    }))
  }, 0.2)
  unrelated_ran <- FALSE
  later::later(function() {unrelated_ran <<- TRUE}, 0.05)
  result <- forward_request("POST", backend$url, list(rook.input = list(read = function() charToRaw("payload"))), "app", config)
  expect_true(promises::is.promise(result))
  expect_false(unrelated_ran)
  response <- await_response(result)
  expect_true(unrelated_ran)
  expect_equal(response$status, 200)
  expect_equal(response$body, "started")
  expect_equal(posts, 1)
  expect_null(config$get_app_startup_state("app"))
})

test_that("startup grace expiry returns 503 without blocking timers", {
  backend <- start_test_http_server(function(req) create_html_response("unused"))
  httpuv::stopServer(backend$server)
  config <- proxy_test_config(backend$port, timeout = 0.2)
  config$set_app_starting("app")
  ran <- FALSE
  later::later(function() {ran <<- TRUE}, 0.05)
  result <- await_response(forward_request("GET", backend$url, list(), "app", config))
  expect_true(ran)
  expect_equal(result$status, 503)
  expect_match(result$body, "starting up")
})

test_that("async transfer failures become 502 responses", {
  config <- proxy_test_config(3001)
  local_mocked_bindings(
    wait_for_backend = function(url, wait_seconds) promises::promise_resolve(TRUE),
    fetch_backend_async = function(url, handle) promises::promise_reject(simpleError("transfer failed"))
  )
  result <- await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "app", config))
  expect_equal(result$status, 502)
  expect_match(result$body, "transfer failed")
})

test_that("process manager readiness probes are asynchronous", {
  backend <- start_test_http_server(function(req) create_html_response("ready"))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  process <- list(is_alive = function() TRUE)
  config$add_app_process("app", process)
  config$set_app_starting("app")
  local_mocked_bindings(is_port_in_use = function(...) stop("Blocking probe must not be used"))
  result <- ProcessManager$new(config)$check_app_ready("app", backend$port, process)
  expect_true(promises::is.promise(result))
  expect_true(await_response(result))
  expect_null(config$get_app_startup_state("app"))
})
