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

test_that("proxy preserves binary WebSocket frames before and after backend readiness", {
  observed <- list()
  backend <- start_test_http_server(function(req) handle_health_check(), on_ws = function(ws) {
    ws$onMessage(function(binary, message) {
      observed[[length(observed) + 1L]] <<- list(binary = binary, message = message)
      ws$send(message)
    })
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) handle_health_check(),
    on_ws = function(ws) handle_websocket_connection(ws, config, cm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  client <- websocket::WebSocket$new(paste0(sub("^http", "ws", proxy$url), "/proxy/app/websocket/"))
  on.exit(client$close(), add = TRUE)
  payloads <- list(as.raw(c(0, 1, 127, 128, 255)), raw(), "text after binary")
  received <- list()
  result <- promises::promise(function(resolve, reject) {
    client$onOpen(function(event) client$send(payloads[[1]]))
    client$onMessage(function(event) {
      received[[length(received) + 1L]] <<- event$data
      if (length(received) == length(payloads)) resolve(received)
      else client$send(payloads[[length(received) + 1L]])
    })
    client$onClose(function(event) reject(simpleError("Proxy closed before all frames arrived")))
    client$onError(function(event) reject(simpleError(event$message)))
  })
  expect_identical(await_response(result), payloads)
  expect_identical(lapply(observed, `[[`, "message"), payloads)
  expect_identical(vapply(observed, `[[`, logical(1), "binary"), c(TRUE, TRUE, FALSE))
})

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

test_that("proxy preserves download, cookie, cache and redirect headers over HTTP", {
  backend <- start_test_http_server(function(req) {
    if (req$PATH_INFO == "/redirect") return(list(status = 302L,
      headers = list("Location" = "/download"), body = "redirect"))
    if (req$PATH_INFO == "/absolute") return(list(status = 302L,
      headers = list("Location" = paste0("http://127.0.0.1:", req$SERVER_PORT, "/download")), body = "redirect"))
    list(status = 200L, headers = structure(list("text/csv", "attachment; filename=report.csv",
      "private, no-store", "a=1; Path=/; HttpOnly", "b=2; Path=/; SameSite=Lax"),
      names = c("Content-Type", "Content-Disposition", "Cache-Control", "Set-Cookie", "Set-Cookie")),
      body = "a,b\n1,2")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  proxy <- start_test_http_server(function(req) {
    handle_proxy_request(req$PATH_INFO, req$REQUEST_METHOD, req$QUERY_STRING, req, config)
  })
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  result <- await_response(fetch_backend_async(paste0(proxy$url, "/proxy/app/download"), curl::new_handle()))
  headers <- curl::parse_headers_list(result$headers)
  expect_equal(result$status_code, 200)
  expect_equal(headers$`content-disposition`, "attachment; filename=report.csv")
  expect_equal(headers$`cache-control`, "private, no-store")
  expect_equal(unname(unlist(headers[names(headers) == "set-cookie"])),
    c("a=1; Path=/proxy/app/; HttpOnly", "b=2; Path=/proxy/app/; SameSite=Lax"))
  expect_equal(rawToChar(result$content), "a,b\n1,2")
  for (path in c("/redirect", "/absolute")) {
    response <- await_response(forward_request("GET", paste0(backend$url, path), list(), "app", config))
    expect_equal(response$status, 302)
    expect_equal(response$headers$location, "/proxy/app/download")
  }
})

test_that("encoded backend bodies retain matching content encoding", {
  file <- tempfile()
  on.exit(unlink(file), add = TRUE)
  stream <- gzfile(file, "wb")
  writeBin(charToRaw("compressed response"), stream)
  close(stream)
  compressed <- readBin(file, "raw", file.info(file)$size)
  backend <- start_test_http_server(function(req) list(status = 200L,
    headers = list("Content-Type" = "text/plain", "Content-Encoding" = "gzip"), body = compressed))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  response <- await_response(forward_request("GET", backend$url, list(), "app", proxy_test_config(backend$port)))
  expect_equal(response$status, 200)
  expect_equal(response$headers$`content-encoding`, "gzip")
  expect_identical(response$body, compressed)
  expect_equal(memDecompress(response$body, type = "gzip", asChar = TRUE), "compressed response")
})

test_that("query-string colons do not change the backend readiness port", {
  backend <- start_test_http_server(function(req) create_html_response(req$QUERY_STRING))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  result <- await_response(handle_proxy_request("/proxy/app/", "GET", "time=12:34", list(), config))
  expect_equal(result$status, 200)
  expect_equal(result$body, "?time=12:34")
})

test_that("a real backend WebSocket close reaches the browser", {
  received <- FALSE
  backend <- start_test_http_server(function(req) create_html_response("backend"), on_ws = function(ws) {
    ws$onMessage(function(binary, message) {
      received <<- identical(message, "init")
      ws$close()
    })
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) create_html_response("proxy"), on_ws = function(ws) {
    handle_websocket_connection(ws, config, cm)
  })
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  client <- websocket::WebSocket$new(paste0(sub("^http", "ws", proxy$url), "/proxy/app/websocket"))
  on.exit(client$close(), add = TRUE)
  result <- promises::promise(function(resolve, reject) {
    client$onOpen(function(event) client$send("init"))
    client$onClose(function(event) resolve(TRUE))
    client$onError(function(event) reject(simpleError(event$message)))
  })
  expect_true(await_response(result))
  expect_true(received)
  expect_equal(config$get_app_connection_count("app"), 0)
  expect_length(config$get_all_ws_connections(), 0)
  expect_length(config$get_all_backend_connections(), 0)
})

test_that("HTTP routing preserves query delimiters and directory paths", {
  backend <- start_test_http_server(function(req) {
    if (req$PATH_INFO == "/dir") {
      return(list(status = 301L, headers = list(Location = "/dir/"), body = "redirect"))
    }
    list(status = 200L, headers = list("Content-Type" = "application/json"),
      body = jsonlite::toJSON(list(path = req$PATH_INFO, query = req$QUERY_STRING), auto_unbox = TRUE))
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  proxy <- start_test_http_server(function(req) handle_http_request(req, config, NULL, NULL))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  for (path in c("/?foo=bar&encoded=a%3Fb", "/dir/", "/dir//file?x=1")) {
    response <- await_response(fetch_backend_async(paste0(proxy$url, "/proxy/app", path),
      curl::new_handle(followlocation = FALSE)))
    expect_equal(response$status_code, 200)
    body <- jsonlite::fromJSON(rawToChar(response$content))
    expect_equal(paste0(body$path, body$query), path)
  }
})

test_that("WebSocket proxy forwards each browser's own authentication headers", {
  backend <- start_test_http_server(function(req) create_html_response("ok"), on_ws = function(ws) {
    ws$onMessage(function(binary, message) {
      ws$send(jsonlite::toJSON(list(cookie = ws$request$HTTP_COOKIE %||% "NONE",
        auth = ws$request$HTTP_AUTHORIZATION %||% "NONE"), auto_unbox = TRUE))
    })
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) create_html_response("ok"),
    on_ws = function(ws) handle_websocket_connection(ws, config, cm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  for (user in c("alice", "bob", "NONE")) {
    headers <- if (user == "NONE") list() else list(Cookie = paste0("session=", user), Authorization = paste("Bearer", user))
    ws <- websocket::WebSocket$new(paste0(sub("http:", "ws:", proxy$url), "/proxy/app/websocket/"), headers = headers)
    on.exit(ws$close(), add = TRUE)
    response <- promises::promise(function(resolve, reject) {
      ws$onOpen(function(event) ws$send("init"))
      ws$onMessage(function(event) resolve(jsonlite::fromJSON(event$data)))
      ws$onError(function(event) reject(simpleError(event$message)))
    })
    actual <- await_response(response)
    expect_equal(actual$cookie, headers$Cookie %||% "NONE")
    expect_equal(actual$auth, headers$Authorization %||% "NONE")
    ws$close()
  }
})

test_that("a refused backend WebSocket disconnects the browser and clears tracking", {
  backend <- start_test_http_server(function(req) create_html_response("ok"))
  config <- proxy_test_config(backend$port)
  httpuv::stopServer(backend$server)
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) create_html_response("ok"),
    on_ws = function(ws) handle_websocket_connection(ws, config, cm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  ws <- websocket::WebSocket$new(paste0(sub("http:", "ws:", proxy$url), "/proxy/app/websocket/"))
  on.exit(ws$close(), add = TRUE)
  closed <- promises::promise(function(resolve, reject) {
    ws$onOpen(function(event) ws$send("init"))
    ws$onClose(function(event) resolve(TRUE))
    ws$onError(function(event) reject(simpleError(event$message)))
  })
  expect_true(await_response(closed))
  expect_length(config$get_all_ws_connections(), 0)
  expect_length(config$get_all_backend_connections(), 0)
  expect_equal(config$get_app_connection_count("app"), 0)
})

test_that("bare app URLs redirect with query and method preserved", {
  methods <- character()
  backend <- start_test_http_server(function(req) {
    methods <<- c(methods, req$REQUEST_METHOD)
    list(status = 200L, headers = list("Content-Type" = "text/plain"),
      body = paste(req$PATH_INFO, req$QUERY_STRING, rawToChar(req$rook.input$read())))
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  proxy <- start_test_http_server(function(req) handle_http_request(req, config, NULL, NULL))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  url <- paste0(proxy$url, "/proxy/app?x=a%2Fb&y=2")
  response <- await_response(fetch_backend_async(url, curl::new_handle(followlocation = FALSE)))
  expect_equal(response$status_code, 308)
  expect_equal(curl::parse_headers_list(response$headers)$location, "/proxy/app/?x=a%2Fb&y=2")
  expect_length(methods, 0)
  response <- await_response(fetch_backend_async(url, curl::new_handle(followlocation = TRUE, postfields = "payload")))
  expect_equal(response$status_code, 200)
  expect_equal(methods, "POST")
  expect_equal(rawToChar(response$content), "/ ?x=a%2Fb&y=2 payload")
})

test_that("idle shutdown waits for every concurrent HTTP response", {
  completions <- list()
  backend <- start_test_http_server(function(req) promises::promise(function(resolve, reject) {
    completions[[req$PATH_INFO]] <<- resolve
  }))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  config$config$apps[[1]]$resident <- FALSE
  alive <- TRUE
  kills <- 0
  config$add_app_process("app", list(is_alive = function() alive, kill_tree = function() {
    kills <<- kills + 1
    alive <<- FALSE
    httpuv::stopServer(backend$server)
  }))
  pm <- ProcessManager$new(config)
  cm <- ConnectionManager$new(config, pm)
  proxy <- start_test_http_server(function(req) handle_http_request(req, config, NULL, cm, pm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  cm$add_client_connection("s", list(close = function() NULL), "app", "127.0.0.1", "test")
  first <- fetch_backend_async(paste0(proxy$url, "/proxy/app/first"), curl::new_handle())
  second <- fetch_backend_async(paste0(proxy$url, "/proxy/app/second"), curl::new_handle())
  deadline <- Sys.time() + 5
  while (length(completions) < 2 && Sys.time() < deadline) later::run_now(.01)
  expect_length(completions, 2)
  cm$remove_client_connection("s")
  expect_equal(kills, 0)
  completions[["/second"]](create_html_response("second"))
  expect_equal(await_response(second)$status_code, 200)
  expect_equal(kills, 0)
  expect_equal(config$active_http_requests$app, 1)
  completions[["/first"]](create_html_response("first"))
  response <- await_response(first)
  expect_equal(response$status_code, 200)
  expect_match(rawToChar(response$content), "first")
  expect_equal(kills, 1)
  expect_equal(config$active_http_requests$app, 0)
})
