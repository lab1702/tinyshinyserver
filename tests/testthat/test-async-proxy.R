proxy_test_config <- function(port, timeout = 2) {
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app", port = port, resident = TRUE, appstart_timeout = timeout)))
  config$add_app_process("app", test_backend_process())
  config
}

test_that("WebSocket proxy preserves document paths and query strings", {
  backend <- start_test_http_server(function(req) handle_health_check(), on_ws = function(ws) {
    ws$onMessage(function(binary, message) {
      ws$send(paste0(ws$request$PATH_INFO, ws$request$QUERY_STRING))
    })
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) handle_health_check(),
    on_ws = function(ws) handle_websocket_connection(ws, config, cm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  clients <- list()
  on.exit(lapply(clients, function(client) client$close()), add = TRUE, after = FALSE)
  for (suffix in c("/websocket/", "/second.Rmd/websocket/?token=a%2Fb&n=1", "/folder/doc.Rmd/websocket?x=1")) {
    client <- websocket::WebSocket$new(paste0(sub("^http", "ws", proxy$url), "/proxy/app", suffix))
    clients[[length(clients) + 1L]] <- client
    response <- promises::promise(function(resolve, reject) {
      client$onOpen(function(event) client$send("init"))
      client$onMessage(function(event) resolve(event$data))
      client$onError(function(event) reject(simpleError(event$message)))
    })
    expect_identical(await_response(response), suffix)
  }
})

test_that("the INFO log names an app's socket without its query string", {
  backend <- start_test_http_server(function(req) handle_health_check(), on_ws = function(ws) {
    ws$send(ws$request$QUERY_STRING)
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) handle_health_check(),
    on_ws = function(ws) handle_websocket_connection(ws, config, cm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)

  logged <- character()
  logger::log_threshold("INFO", namespace = "tinyshinyserver")
  logger::log_appender(function(lines) logged <<- c(logged, lines), namespace = "tinyshinyserver")
  on.exit(logger::log_appender(logger::appender_console, namespace = "tinyshinyserver"), add = TRUE)

  client <- websocket::WebSocket$new(paste0(sub("^http", "ws", proxy$url), "/proxy/app/websocket/?token=secret"))
  on.exit(client$close(), add = TRUE, after = FALSE)
  response <- promises::promise(function(resolve, reject) {
    client$onMessage(function(event) resolve(event$data))
    client$onError(function(event) reject(simpleError(event$message)))
  })
  # The app still receives the query string
  expect_identical(await_response(response), "?token=secret")

  expect_true(any(grepl(paste0("Connecting to backend: ws://127.0.0.1:", backend$port, "/websocket/ for app app"),
    logged, fixed = TRUE)))
  expect_false(any(grepl("secret", logged, fixed = TRUE)))
})

test_that("an app that speaks first reaches a browser that never sends", {
  backend <- start_test_http_server(function(req) handle_health_check(), on_ws = function(ws) {
    ws$send(paste0("hello from ", ws$request$PATH_INFO))
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) handle_health_check(),
    on_ws = function(ws) handle_websocket_connection(ws, config, cm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  # Shiny's autoreload socket, for one, only listens
  client <- websocket::WebSocket$new(paste0(sub("^http", "ws", proxy$url), "/proxy/app/autoreload/"))
  on.exit(client$close(), add = TRUE, after = FALSE)
  opened <- Sys.time()
  response <- promises::promise(function(resolve, reject) {
    client$onMessage(function(event) resolve(event$data))
    client$onClose(function(event) reject(simpleError("Proxy closed the listening session")))
    client$onError(function(event) reject(simpleError(event$message)))
  })
  expect_identical(await_response(response), "hello from /autoreload/")

  # The app's message counts as the session's activity without rewriting its records
  session_id <- names(config$get_all_ws_connections())
  expect_length(session_id, 1)
  expect_gte(as.numeric(config$get_session_activity(session_id)), as.numeric(opened))
  expect_lte(as.numeric(config$get_ws_connection(session_id)$last_activity),
    as.numeric(config$get_session_activity(session_id)))
})

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
  on.exit(client$close(), add = TRUE, after = FALSE)
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
  expect_equal(rawToChar(request("/echo")$body), "NONE")
  expect_equal(rawToChar(request("/echo", "session=bob")$body), "session=bob")
  expect_equal(rawToChar(request("/echo", "session=carol", "POST")$body), "session=carol")
  expect_equal(rawToChar(request("/echo", method = "POST")$body), "NONE")
  responses <- await_response(promises::promise_all(
    alice = forward_request("GET", paste0(backend$url, "/echo"), list(HTTP_COOKIE = "session=alice"), "app", config),
    bob = forward_request("GET", paste0(backend$url, "/echo"), list(HTTP_COOKIE = "session=bob"), "app", config),
    guest = forward_request("GET", paste0(backend$url, "/echo"), list(), "app", config)
  ))
  expect_equal(rawToChar(responses$alice$body), "session=alice")
  expect_equal(rawToChar(responses$bob$body), "session=bob")
  expect_equal(rawToChar(responses$guest$body), "NONE")
})

test_that("a forwarded body keeps the client's Content-Type, or lack of one, and an empty one its length", {
  observed <- NULL
  backend <- start_test_http_server(function(req) {
    observed <<- list(type = req$HTTP_CONTENT_TYPE, length = req$HTTP_CONTENT_LENGTH,
      body = req$rook.input$read())
    create_html_response("ok")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  send <- function(method, body, ...) {
    req <- list(rook.input = list(read = function() body), ...)
    expect_equal(await_response(forward_request(method, backend$url, req, "app", config))$status, 200)
    observed
  }

  # libcurl labels a body that has no type as a URL-encoded form
  untyped <- send("POST", charToRaw("abc"))
  expect_null(untyped$type)
  expect_equal(untyped$length, "3")
  expect_identical(untyped$body, charToRaw("abc"))

  # A declared empty body arrives as one, with its type
  empty <- send("POST", raw(), CONTENT_LENGTH = "0", CONTENT_TYPE = "application/json")
  expect_equal(empty$type, "application/json")
  expect_equal(empty$length, "0")
  expect_null(send("PUT", raw(), CONTENT_LENGTH = "0")$type)

  # A request without a body gains neither header
  bodiless <- send("DELETE", raw())
  expect_null(bodiless$type)
  expect_null(bodiless$length)
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
  for (method in c("POST", "PUT", "PATCH", "DELETE")) {
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
  expect_null(response$body)
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
  # Close the client before stopping the server that owns its peer.
  on.exit(if (!is.null(ws)) ws$close(), add = TRUE, after = FALSE)
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
  expect_equal(rawToChar(response$body), "started")
  expect_equal(posts, 1)
  expect_null(config$get_app_startup_state("app"))
})

test_that("proxied text responses pass through as the app's bytes", {
  csv <- enc2utf8("name,city
José,Zürich
")
  backend <- start_test_http_server(function(req) {
    list(status = 200L, headers = list("Content-Type" = "text/csv; charset=utf-8"), body = charToRaw(csv))
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  response <- await_response(forward_request("GET", backend$url, list(), "app", proxy_test_config(backend$port)))
  expect_equal(response$status, 200)
  expect_identical(response$body, charToRaw(csv))
  expect_identical(response$headers[["content-type"]], "text/csv; charset=utf-8")
})

test_that("a large app response is written to a file instead of held in memory", {
  size <- RESPONSE_SPOOL_BYTES + 4096
  payload <- as.raw(seq_len(size) %% 251)
  backend <- start_test_http_server(function(req) {
    list(status = 200L, headers = list("Content-Type" = "application/octet-stream"),
      body = if (req$PATH_INFO == "/large") payload else payload[seq_len(RESPONSE_SPOOL_BYTES)])
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)

  # Up to the limit the body stays in memory
  response <- await_response(forward_request("GET", paste0(backend$url, "/small"), list(), "app", config), timeout = 30)
  expect_identical(response$body, payload[seq_len(RESPONSE_SPOOL_BYTES)])

  # Above it, httpuv sends the file and then deletes it
  response <- await_response(forward_request("GET", paste0(backend$url, "/large"), list(), "app", config), timeout = 30)
  expect_equal(response$status, 200)
  expect_named(response$body, c("file", "owned"))
  on.exit(unlink(response$body$file), add = TRUE)
  expect_true(response$body$owned)
  expect_identical(readBin(response$body$file, "raw", size + 1), payload)

  # A response that cannot have a body leaves no file behind
  files <- function() list.files(tempdir(), pattern = "^tss-response-")
  before <- files()
  response <- await_response(forward_request("HEAD", paste0(backend$url, "/large"), list(), "app", config), timeout = 30)
  expect_null(response$body)
  expect_setequal(files(), before)
})

test_that("a collected response body is a raw vector or a file, and a failed one leaves nothing", {
  body <- response_body_collector(limit = 10)
  body$write(as.raw(1:4))
  body$write(raw())
  body$write(as.raw(5:10), final = TRUE)
  expect_identical(body$finish(), as.raw(1:10))
  expect_identical(response_body_collector()$finish(), raw())

  body <- response_body_collector(limit = 10)
  for (chunk in list(as.raw(1:6), as.raw(7:12), as.raw(13:20))) body$write(chunk)
  spooled <- body$finish()
  on.exit(unlink(spooled$file), add = TRUE)
  expect_identical(readBin(spooled$file, "raw", 100), as.raw(1:20))
  expect_true(spooled$owned)

  # A transfer that fails part-way removes what it had written
  body <- response_body_collector(limit = 10)
  body$write(as.raw(1:20))
  path <- environment(body$write)$path
  expect_true(file.exists(path))
  body$discard()
  expect_false(file.exists(path))

  # A finished body belongs to the response and outlives a late discard()
  body <- response_body_collector(limit = 10)
  body$write(as.raw(1:20))
  kept <- body$finish()
  body$discard()
  expect_identical(readBin(kept$file, "raw", 100), as.raw(1:20))
  discard_response_body(kept)
  expect_false(file.exists(kept$file))

  discard_response_body(as.raw(1:3))
  discard_response_body(spooled)
  expect_false(file.exists(spooled$file))
})

test_that("a failed transfer discards a partly received large response", {
  backend <- start_test_http_server(function(req) {
    list(status = 200L, headers = list("Content-Type" = "application/octet-stream"), body = raw(4e6))
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)

  # Write to a file almost at once, and note where
  collector <- response_body_collector
  paths <- character()
  local_mocked_bindings(response_body_collector = function(...) {
    body <- collector(limit = 1024)
    write <- body$write
    body$write <- function(data, final = FALSE) {
      write(data, final)
      paths <<- union(paths, environment(write)$path)
    }
    body
  })

  # A transfer too slow to finish before its time limit fails part-way
  handle <- new_backend_handle(max_recv_speed_large = 200000, timeout_ms = 1000, accept_encoding = "identity")
  expect_error(await_response(fetch_backend_async(backend$url, handle), timeout = 30))
  expect_length(paths, 1)
  expect_false(file.exists(paths))
})

test_that("responses that cannot have a body are sent without one", {
  backend <- start_test_http_server(function(req) {
    list(status = as.integer(sub("^/", "", req$PATH_INFO)), headers = list(ETag = "\"v1\""), body = NULL)
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  for (status in c(204L, 304L)) {
    response <- await_response(forward_request("GET", paste0(backend$url, "/", status), list(), "app", config))
    expect_equal(response$status, status)
    expect_equal(response$headers$etag, "\"v1\"")
    # An empty body would be compressed and sent after a response that has none
    expect_true("body" %in% names(response))
    expect_null(response$body)
  }
})

test_that("proxied responses carry no second Date and no invented Content-Type", {
  # httpuv writes each response's Date itself
  headers <- charToRaw("HTTP/1.1 304 Not Modified\r\nDate: Fri, 20 Nov 2026 19:28:30 GMT\r\nETag: abc\r\n\r\n")
  expect_equal(names(proxy_response_headers(headers, "http://127.0.0.1:3001/file.txt", "app", "GET")), "etag")

  backend <- start_test_http_server(function(req) {
    list(status = 200L, headers = list("X-App" = "untyped"), body = charToRaw("bytes"))
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  response <- await_response(forward_request("GET", backend$url, list(), "app", proxy_test_config(backend$port)))
  expect_equal(response$status, 200)
  expect_identical(response$body, charToRaw("bytes"))
  expect_equal(response$headers[["x-app"]], "untyped")
  expect_false(any(c("date", "content-type") %in% names(response$headers)))
})

test_that("proxied requests close their backend connections when they finish", {
  skip_if_not(ps::ps_is_supported())
  backend <- start_test_http_server(function(req) create_html_response("ok"))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  for (i in 1:5) {
    response <- await_response(forward_request("GET", backend$url, list(), "app", config))
    expect_equal(response$status, 200)
  }
  # Count this session's client ends of connections to the backend without
  # running garbage collection, which would also close abandoned connections.
  connections <- ps::ps_connections(ps::ps_handle())
  open <- !is.na(connections$rport) & connections$rport == backend$port &
    connections$state == "CONN_ESTABLISHED"
  expect_equal(sum(open), 0)
})

test_that("readiness probes close their connections without garbage collection", {
  # libcurl 8 closes a finished connect-only connection itself, so the
  # connection count above can only catch a missing option on older libcurl.
  options <- NULL
  new_handle <- curl::new_handle
  local_mocked_bindings(new_handle = function(...) {
    options <<- list(...)
    new_handle()
  }, .package = "curl")
  local_mocked_bindings(fetch_backend_async = function(url, handle) promises::promise_resolve(NULL))
  expect_true(await_response(wait_for_backend("http://127.0.0.1:1/")))
  expect_true(options$connect_only)
  expect_true(options$forbid_reuse)
  expect_identical(options$noproxy, "*")
})

test_that("backend requests ignore proxy environment variables", {
  backend <- start_test_http_server(function(req) create_html_response("direct"))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  variables <- c("http_proxy", "ALL_PROXY", "all_proxy", "no_proxy", "NO_PROXY")
  old <- Sys.getenv(variables, unset = NA)
  on.exit(for (name in variables) {
    if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, as.list(old[name]))
  }, add = TRUE)
  # Nothing listens on the discard port, so a request sent to this proxy fails
  Sys.unsetenv(c("no_proxy", "NO_PROXY"))
  Sys.setenv(http_proxy = "http://127.0.0.1:9", ALL_PROXY = "http://127.0.0.1:9")

  expect_true(await_response(wait_for_backend(backend$url)))
  config <- proxy_test_config(backend$port)
  response <- await_response(forward_request("GET", backend$url, list(), "app", config))
  expect_equal(response$status, 200)
  expect_identical(rawToChar(response$body), "direct")
})

test_that("an app receives dot segments as the client sent them", {
  seen <- character()
  backend <- start_test_http_server(function(req) {
    seen <<- c(seen, req$PATH_INFO)
    create_html_response("ok")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  # libcurl would send these as /a/b, /c and /a/b
  paths <- c("/a/./b", "/a/%2e%2e/c", "/a/%2E/b")
  for (path in paths) {
    response <- await_response(forward_request("GET", paste0(backend$url, path), list(), "app", config))
    expect_equal(response$status, 200)
  }
  expect_identical(seen, paths)
})

test_that("requests to a running app are forwarded without a readiness probe", {
  seen <- character()
  backend <- start_test_http_server(function(req) {
    seen <<- c(seen, paste0(req$PATH_INFO, req$QUERY_STRING))
    create_html_response("ready")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  local_mocked_bindings(wait_for_backend = function(...) stop("A running app must not be probed"))
  # A literal "%" in the query reaches the app unchanged
  url <- paste0(backend$url, "/page?discount=50%&x=1")
  response <- await_response(forward_request("GET", url, list(), "app", config))
  expect_equal(response$status, 200)
  expect_identical(seen, "/page?discount=50%&x=1")
})

test_that("a refused connection to a verified app returns 503 and rechecks its port", {
  config <- proxy_test_config(3001)
  local_mocked_bindings(process_owns_port = function(process, port) TRUE)
  expect_true(config$app_backend_verified("app"))
  local_mocked_bindings(
    wait_for_backend = function(...) stop("A running app must not be probed"),
    fetch_backend_async = function(url, handle) promises::promise_reject(structure(
      class = c("backend_connect_error", "simpleError", "error", "condition"),
      list(message = "Could not connect to 127.0.0.1 port 3001", call = NULL)
    ))
  )
  result <- await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "app", config))
  expect_equal(result$status, 503)
  expect_match(result$body, "not ready")
  expect_no_match(result$body, "3001")
  expect_false(exists("app", envir = config$verified_backends, inherits = FALSE))
})

test_that("only transfers that never connect are marked as connection failures", {
  backend <- start_test_http_server(function(req) create_html_response("unused"))
  httpuv::stopServer(backend$server)
  deadline <- Sys.time() + 5
  while (is_port_in_use("127.0.0.1", backend$port) && Sys.time() < deadline) Sys.sleep(0.05)
  # Windows retries a refused loopback connection for about two seconds
  refused <- tryCatch(
    await_response(fetch_backend_async(backend$url, curl::new_handle(connecttimeout_ms = 500))),
    error = function(e) e
  )
  expect_s3_class(refused, "backend_connect_error")
  expect_s3_class(refused, "error")

  release <- NULL
  stalled <- start_test_http_server(function(req) promises::promise(function(resolve, reject) {
    release <<- resolve
  }))
  on.exit(httpuv::stopServer(stalled$server), add = TRUE)
  timed_out <- tryCatch(
    await_response(fetch_backend_async(stalled$url, curl::new_handle(timeout_ms = 300))),
    error = function(e) e
  )
  release(create_html_response("late"))
  expect_s3_class(timed_out, "error")
  expect_false(inherits(timed_out, "backend_connect_error"))
})

test_that("an upload the app refuses from its headers returns the app's response", {
  # Like Shiny above shiny.maxRequestSize: answer from the headers, then close
  expects <- list()
  stored <- NULL
  server <- NULL
  for (port in sample(20000:50000, 3)) {
    server <- tryCatch(httpuv::startServer("127.0.0.1", port, list(
      onHeaders = function(req) {
        expects[[length(expects) + 1L]] <<- req$HTTP_EXPECT
        reject_large_request_body(req, 1024^2)
      },
      call = function(req) {
        stored <<- length(req$rook.input$read())
        create_html_response("stored")
      }
    ), quiet = TRUE), error = function(e) NULL)
    if (!is.null(server)) break
  }
  if (is.null(server)) skip("Loopback sockets unavailable")
  on.exit(httpuv::stopServer(server), add = TRUE)
  config <- proxy_test_config(port)
  url <- paste0("http://127.0.0.1:", port, "/upload")
  upload <- function(size) {
    body <- raw(size)
    # curl sends this header with large uploads; the proxy must not pass it on
    req <- list(HTTP_EXPECT = "100-continue", CONTENT_TYPE = "application/octet-stream",
      rook.input = list(read = function() body))
    await_response(forward_request("POST", url, req, "app", config))
  }

  # Sending the body anyway resets the connection and can lose the answer
  for (attempt in 1:5) {
    expect_equal(upload(8 * 1024^2)$status, 413)
  }
  expect_null(stored)

  # A large body the app accepts is sent after the app agrees to it
  expect_equal(upload(512 * 1024)$status, 200)
  expect_equal(stored, 512 * 1024)
  expect_identical(expects[[length(expects)]], "100-continue")

  # A small body goes with the request, without the client's Expect header
  expects <- list()
  expect_equal(upload(1024)$status, 200)
  expect_equal(stored, 1024)
  expect_length(expects, 0)
})

test_that("transfers wait on their sockets instead of polling on a timer", {
  received <- NULL
  backend <- start_test_http_server(function(req) {
    received <<- length(req$rook.input$read())
    create_html_response("stored")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  # A timer poll sends one upload buffer, 64 KB by default, on each tick
  timer_polls <- 0
  later_real <- later::later
  local_mocked_bindings(later = function(func, delay = 0, ...) {
    if (delay > 0 && delay <= 0.01) timer_polls <<- timer_polls + 1
    later_real(func, delay, ...)
  }, .package = "later")
  body <- raw(4 * 1024^2)
  handle <- curl::new_handle(customrequest = "POST", postfields = body)
  response <- await_response(fetch_backend_async(backend$url, handle))
  expect_equal(response$status_code, 200)
  expect_equal(received, length(body))
  expect_lt(timer_polls, 5)
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
    fetch_backend_async = function(url, handle) promises::promise_reject(simpleError("transfer failed")),
    process_owns_port = function(process, port) TRUE
  )
  result <- await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "app", config))
  expect_equal(result$status, 502)
  expect_match(result$body, "Bad Gateway")
  expect_no_match(result$body, "transfer failed")
})

test_that("process manager readiness probes are asynchronous", {
  backend <- start_test_http_server(function(req) create_html_response("ready"))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  process <- test_backend_process()
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
  expect_equal(rawToChar(result$body), "?time=12:34")
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
  on.exit(client$close(), add = TRUE, after = FALSE)
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
        auth = ws$request$HTTP_AUTHORIZATION %||% "NONE",
        user = ws$request$HTTP_X_FORWARDED_USER %||% "NONE"), auto_unbox = TRUE))
    })
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) create_html_response("ok"),
    on_ws = function(ws) handle_websocket_connection(ws, config, cm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  for (user in c("alice", "bob", "NONE")) {
    # X-Forwarded-User stands for the identity header of an authenticating
    # reverse proxy. A client cannot replace it by sending the same name with
    # underscores, which the app's server would read in its place.
    headers <- if (user == "NONE") list() else list(Cookie = paste0("session=", user),
      Authorization = paste("Bearer", user), "X-Forwarded-User" = user, "X_Forwarded_User" = "admin")
    ws <- websocket::WebSocket$new(paste0(sub("http:", "ws:", proxy$url), "/proxy/app/websocket/"), headers = headers)
    on.exit(ws$close(), add = TRUE, after = FALSE)
    response <- promises::promise(function(resolve, reject) {
      ws$onOpen(function(event) ws$send("init"))
      ws$onMessage(function(event) resolve(jsonlite::fromJSON(event$data)))
      ws$onError(function(event) reject(simpleError(event$message)))
    })
    actual <- await_response(response)
    expect_equal(actual$cookie, headers$Cookie %||% "NONE")
    expect_equal(actual$auth, headers$Authorization %||% "NONE")
    expect_equal(actual$user, user)
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
  on.exit(ws$close(), add = TRUE, after = FALSE)
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
  config$HTTP_SESSION_GRACE_SECONDS <- 0.2
  config$config$apps[[1]]$resident <- FALSE
  alive <- TRUE
  kills <- 0
  config$add_app_process("app", list(is_alive = function() alive, get_pid = function() Sys.getpid(), kill_tree = function() {
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
  expect_equal(kills, 0)
  expect_equal(config$active_http_requests$app, 0)
  deadline <- Sys.time() + 2
  while (kills == 0 && Sys.time() < deadline) later::run_now(.01)
  expect_equal(kills, 1)
})

test_that("the proxy never forwards to another program holding an app's port", {
  skip_on_cran() # Starts an R process
  skip_if_not(ps::ps_is_supported())
  hits <- 0
  backend <- start_test_http_server(function(req) {
    hits <<- hits + 1
    create_html_response("foreign")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  # A live app process that failed to bind: this R session holds its port.
  app <- callr::r_bg(function() Sys.sleep(60))
  on.exit(app$kill(), add = TRUE)
  config <- proxy_test_config(backend$port)
  config$add_app_process("app", app)
  # Retire the process, so that readiness checks still watching it stop
  on.exit(config$remove_app_process("app"), add = TRUE, after = FALSE)
  # Checks run on the event loop, so they must not walk the process table.
  local_mocked_bindings(ps_children = function(...) stop("Process table walked"), .package = "ps")
  expect_true(process_owns_port(test_backend_process(), backend$port))
  expect_false(process_owns_port(app, backend$port))

  config$set_app_starting("app")
  pm <- ProcessManager$new(config)
  expect_false(await_response(pm$check_app_ready("app", backend$port, app, max_attempts = 1)))
  request <- function() {
    await_response(forward_request("GET", backend$url, list(HTTP_COOKIE = "session=secret"), "app", config))
  }
  # The readiness checks gave up, which the first request reports
  expect_equal(request()$status, 502)
  expect_equal(request()$status, 503)

  closed <- FALSE
  client <- list(request = list(PATH_INFO = "/proxy/app/websocket/", HTTP_COOKIE = "session=secret"),
    close = function() closed <<- TRUE)
  cm <- ConnectionManager$new(config)
  cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
  expect_null(cm$create_backend_connection("app", "s", client))
  expect_true(closed)
  expect_null(config$get_backend_connection("s"))
  expect_equal(hits, 0)
})

test_that("a launched Shiny app process owns its listening port", {
  skip_on_cran()
  skip_if_not(ps::ps_is_supported())
  app_dir <- tempfile("tss-app")
  log_dir <- tempfile("tss-logs")
  dir.create(app_dir)
  dir.create(log_dir)
  on.exit(unlink(c(app_dir, log_dir), recursive = TRUE), add = TRUE)
  writeLines("shiny::shinyApp(shiny::fluidPage('ok'), function(input, output) NULL)",
    file.path(app_dir, "app.R"))
  config <- ShinyServerConfig$new()
  on.exit(stop_test_app_processes(config), add = TRUE, after = FALSE)
  config$config <- list(apps = list(list(name = "app", path = app_dir, port = httpuv::randomPort(),
    resident = TRUE, appstart_timeout = 60)), log_dir = log_dir)
  ProcessManager$new(config)$start_app(config$config$apps[[1]])
  deadline <- Sys.time() + 60
  while (config$is_app_starting("app") && Sys.time() < deadline) later::run_now(0.1)
  expect_false(config$is_app_starting("app"))
  expect_true(config$app_backend_verified("app"))
})

test_that("app WebSocket messages larger than 32 MiB reach the browser", {
  size <- 33 * 1024 * 1024
  backend <- start_test_http_server(function(req) create_html_response("ok"), on_ws = function(ws) {
    ws$onMessage(function(binary, message) ws$send(raw(size)))
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  cm <- ConnectionManager$new(config)
  proxy <- start_test_http_server(function(req) create_html_response("ok"),
    on_ws = function(ws) handle_websocket_connection(ws, config, cm))
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  client <- websocket::WebSocket$new(paste0(sub("^http", "ws", proxy$url), "/proxy/app/websocket/"),
    maxMessageSize = .Machine$integer.max)
  on.exit(client$close(), add = TRUE, after = FALSE)
  result <- promises::promise(function(resolve, reject) {
    client$onOpen(function(event) client$send("init"))
    client$onMessage(function(event) resolve(length(event$data)))
    client$onClose(function(event) reject(simpleError("Proxy closed the large message session")))
    client$onError(function(event) reject(simpleError(event$message)))
  })
  expect_equal(await_response(result, timeout = 30), size)
})

test_that("request bodies are limited from their headers alone", {
  expect_null(reject_large_request_body(list(), 10))
  expect_null(reject_large_request_body(list(CONTENT_LENGTH = "10"), 10))
  for (req in list(list(CONTENT_LENGTH = "11"), list(CONTENT_LENGTH = "x"),
    list(HTTP_TRANSFER_ENCODING = "chunked"),
    # A chunked body is rejected whatever length the request also declares
    list(CONTENT_LENGTH = "5", HTTP_TRANSFER_ENCODING = "chunked"))) {
    expect_equal(reject_large_request_body(req, 10)$status, 413)
  }
})

test_that("a header spelled with an underscore cannot understate a request body", {
  understated <- httpuv_request(c("content-length" = "5000", "content_length" = "5"))
  # What httpuv does, and why the limit reads the headers under the names sent
  expect_equal(understated$CONTENT_LENGTH, "5")
  expect_equal(reject_large_request_body(understated, 1024)$status, 413)

  chunked <- httpuv_request(c("transfer-encoding" = "chunked", "content_length" = "5"))
  expect_equal(reject_large_request_body(chunked, 1024)$status, 413)

  expect_null(reject_large_request_body(httpuv_request(c("content-length" = "1024")), 1024))
  # The misspelled header alone declares no body
  expect_null(reject_large_request_body(httpuv_request(c("content_length" = "5000")), 1024))
})

test_that("headers spelled with an underscore are not passed on to an app", {
  req <- httpuv_request(c("x-forwarded-user" = "alice", "x_forwarded_user" = "admin",
    "cookie" = "session=a", "my_header" = "x", "host" = "localhost", "content-length" = "3"))
  expect_equal(req$HTTP_X_FORWARDED_USER, "admin")
  headers <- forwarded_request_headers(req)
  expect_setequal(names(headers), c("X-FORWARDED-USER", "COOKIE"))
  expect_equal(headers[["X-FORWARDED-USER"]], "alice")
  expect_equal(headers[["COOKIE"]], "session=a")
  expect_setequal(names(forwarded_request_headers(req, skip = "COOKIE")), "X-FORWARDED-USER")

  # A request object without HEADERS is read from its HTTP_ entries, as before
  plain <- forwarded_request_headers(list(HTTP_X_FORWARDED_USER = "alice", HTTP_HOST = "localhost",
    HTTP_KEEP_ALIVE = "1", PATH_INFO = "/"))
  expect_equal(plain, list("X-FORWARDED-USER" = "alice"))
})

test_that("an oversized request body is rejected before the app is called", {
  called <- FALSE
  server <- NULL
  for (port in sample(20000:50000, 3)) {
    server <- tryCatch(httpuv::startServer("127.0.0.1", port, list(
      onHeaders = function(req) reject_large_request_body(req, 1024),
      call = function(req) { called <<- TRUE; create_html_response("ok") }
    ), quiet = TRUE), error = function(e) NULL)
    if (!is.null(server)) break
  }
  if (is.null(server)) skip("Loopback sockets unavailable")
  on.exit(httpuv::stopServer(server), add = TRUE)
  post <- function(size) {
    handle <- curl::new_handle(customrequest = "POST", postfields = raw(size))
    await_response(fetch_backend_async(paste0("http://127.0.0.1:", port, "/"), handle))
  }
  expect_equal(post(2048)$status_code, 413)
  expect_false(called)
  # Nor when a second header, which httpuv reads as Content-Length, states less
  understated <- curl::new_handle(customrequest = "POST", postfields = raw(2048))
  curl::handle_setheaders(understated, "Content_Length" = "5")
  response <- await_response(fetch_backend_async(paste0("http://127.0.0.1:", port, "/"), understated))
  expect_equal(response$status_code, 413)
  expect_false(called)
  expect_equal(post(512)$status_code, 200)
  expect_true(called)
})

# ============================================================================
# Response headers
# ============================================================================

test_that("backend-local redirects preserve paths queries and fragments", {
  rewrite <- function(location) {
    raw <- charToRaw(paste0("HTTP/1.1 302 Found\r\nLocation: ", location, "\r\n\r\n"))
    proxy_response_headers(raw, "http://127.0.0.1:3001/", "app", "GET")$location
  }
  for (authority in c("http://127.0.0.1:3001", "//127.0.0.1:3001")) {
    for (suffix in c("", "?foo=1", "#section", "/next?x=a%2Fb#section", "//nested/path")) {
      path <- if (startsWith(suffix, "/")) suffix else paste0("/", suffix)
      expect_equal(rewrite(paste0(authority, suffix)), paste0("/proxy/app", path))
    }
  }
  for (location in c("//example.com/next", "https://example.com/?foo=1", "http://127.0.0.1:3002/next",
    "http://127.0.0.1:3001.example.com/next", "../next?x=1", "?foo=1", "#section")) {
    expect_equal(rewrite(location), location)
  }
  expect_equal(rewrite("/next?foo=1#section"), "/proxy/app/next?foo=1#section")
})

test_that("response headers preserve duplicates and remove transfer framing", {
  headers <- charToRaw(paste0("HTTP/1.1 200 OK\r\n",
    "Content-Disposition: attachment; filename=report.csv\r\n",
    "Cache-Control: private, no-store\r\nETag: abc\r\n",
    "Set-Cookie: a=1; Path=/; HttpOnly\r\nSet-Cookie: b=2; Path=/folder; Domain=127.0.0.1\r\n",
    "Connection: keep-alive, X-Internal\r\nX-Internal: hidden\r\n",
    "Transfer-Encoding: chunked\r\nContent-Length: 99\r\n\r\n"))
  result <- proxy_response_headers(headers, "http://127.0.0.1:3001/download", "app", "GET")
  expect_equal(result$`content-disposition`, "attachment; filename=report.csv")
  expect_equal(result$`cache-control`, "private, no-store")
  expect_equal(result$etag, "abc")
  expect_equal(unname(unlist(result[names(result) == "set-cookie"])),
    c("a=1; Path=/proxy/app/; HttpOnly", "b=2; Path=/proxy/app/folder"))
  expect_false(any(c("connection", "x-internal", "transfer-encoding", "content-length") %in% names(result)))
  expect_equal(proxy_response_headers(headers, "http://127.0.0.1:3001/", "app", "HEAD")$`content-length`, "99")
})

test_that("response headers keep bytes that are not valid UTF-8", {
  # A Latin-1 "é", as in a file name read from a Latin-1 file system
  latin1 <- function(...) c(charToRaw(paste0(...)), as.raw(0xE9))
  headers <- c(charToRaw("HTTP/1.1 302 Found\r\nContent-Type: text/csv\r\n"),
    latin1("Content-Disposition: attachment; filename=\"caf"), charToRaw(".csv\"\r\n"),
    latin1("Location: http://127.0.0.1:3001/caf"), charToRaw("\r\n"),
    latin1("Set-Cookie: s=caf"), charToRaw("; Path=/\r\n"),
    latin1("Connection: close, X-Caf"), charToRaw("\r\n"),
    # Not a field name, so left out
    latin1("Bad Nam"), charToRaw(": dropped\r\n\r\n"))
  result <- proxy_response_headers(headers, "http://127.0.0.1:3001/", "app", "GET")
  expect_named(result, c("content-type", "content-disposition", "location", "set-cookie"))
  bytes <- function(value) charToRaw(value)
  expect_equal(result$`content-type`, "text/csv")
  expect_identical(bytes(result$`content-disposition`), c(latin1("attachment; filename=\"caf"), charToRaw(".csv\"")))
  expect_identical(bytes(result$location), latin1("/proxy/app/caf"))
  expect_identical(bytes(result$`set-cookie`), c(latin1("s=caf"), charToRaw("; Path=/proxy/app/")))
  # A redirect to another host is not taken for the app's own address
  other <- c(latin1("HTTP/1.1 302 Found\r\nLocation: http://caf"), charToRaw(".example/\r\n\r\n"))
  expect_identical(bytes(proxy_response_headers(other, "http://127.0.0.1:3001/", "app", "GET")$location),
    c(latin1("http://caf"), charToRaw(".example/")))
})

test_that("response headers come from the final response only", {
  headers <- charToRaw(paste0("HTTP/1.1 100 Continue\r\nX-Interim: yes\r\n\r\n",
    "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nX-Note:  padded value \t\r\n\r\n"))
  result <- proxy_response_headers(headers, "http://127.0.0.1:3001/", "app", "GET")
  expect_named(result, c("content-type", "x-note"))
  expect_equal(result$`x-note`, "padded value")
  expect_length(proxy_response_headers(raw(), "http://127.0.0.1:3001/", "app", "GET"), 0)
})

test_that("cookies with no path get an app-scoped default path", {
  headers <- charToRaw("HTTP/1.1 200 OK\r\nSet-Cookie: session=abc; HttpOnly\r\n\r\n")
  result <- proxy_response_headers(headers, "http://127.0.0.1:3001/", "app", "GET")
  expect_equal(result$`set-cookie`, "session=abc; HttpOnly; Path=/proxy/app/")
  nested <- proxy_response_headers(headers, "http://127.0.0.1:3001/folder/page?x=1", "app", "GET")
  expect_equal(nested$`set-cookie`, "session=abc; HttpOnly; Path=/proxy/app/folder")
})
