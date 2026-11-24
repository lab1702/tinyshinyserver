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
  # The app answers the slow request only once the other requests, which
  # start 0.05 seconds in, have their answers. No wait is long enough on
  # every machine, and a proxy that the slow request held up would never
  # answer the others, so the slow request would never end either.
  completed <- character()
  release_slow <- NULL
  finished <- function(name) {
    completed <<- c(completed, name)
    if (all(c("health", "websocket") %in% completed) && !is.null(release_slow)) release_slow()
  }
  backend <- start_test_http_server(function(req) promises::promise(function(resolve, reject) {
    release_slow <<- function() resolve(create_html_response("slow result"))
    if (all(c("health", "websocket") %in% completed)) release_slow()
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
  slow <- promises::then(fetch_backend_async(paste0(proxy$url, "/slow"), curl::new_handle()), function(x) {
    completed <<- c(completed, "slow"); x
  })
  health <- promises::promise(function(resolve, reject) {
    later::later(function() resolve(fetch_backend_async(paste0(proxy$url, "/health"), curl::new_handle())), 0.05)
  })
  health <- promises::then(health, function(x) {finished("health"); x})
  ws <- NULL
  # Close the client before stopping the server that owns its peer.
  on.exit(if (!is.null(ws)) ws$close(), add = TRUE, after = FALSE)
  websocket_response <- promises::promise(function(resolve, reject) {
    later::later(function() {
      ws <<- websocket::WebSocket$new(sub("^http", "ws", proxy$url))
      ws$onOpen(function(event) ws$send("still responsive"))
      ws$onMessage(function(event) {
        finished("websocket")
        resolve(event$data)
      })
      ws$onError(function(event) reject(simpleError(event$message)))
    }, 0.05)
  })
  result <- await_response(promises::promise_all(slow = slow, health = health, ws = websocket_response), timeout = 30)
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
  stop_test_http_server(backend)
  restarted <- NULL
  on.exit(if (!is.null(restarted)) httpuv::stopServer(restarted), add = TRUE)
  # The app must be found listening before its startup window ends, which a
  # loaded machine could delay past a short one
  config <- proxy_test_config(backend$port, timeout = 5)
  config$set_app_starting("app")
  # Another program can take the freed port before the app listens on it again
  rebind_tried <- FALSE
  rebind_failed <- FALSE
  later::later(function() {
    rebind_tried <<- TRUE
    restarted <<- tryCatch(httpuv::startServer("127.0.0.1", backend$port, list(call = function(req) {
      posts <<- posts + 1
      create_html_response("started")
    })), error = function(e) {
      rebind_failed <<- TRUE
      NULL
    })
  }, 0.2)
  unrelated_ran <- FALSE
  later::later(function() {unrelated_ran <<- TRUE}, 0.05)
  result <- forward_request("POST", backend$url, list(rook.input = list(read = function() charToRaw("payload"))), "app", config)
  expect_true(promises::is.promise(result))
  expect_false(unrelated_ran)
  response <- await_response(result, timeout = 10)
  # A program that took the port answers before the app would have listened
  deadline <- Sys.time() + 5
  while (!rebind_tried && Sys.time() < deadline) later::run_now(0.01)
  if (rebind_failed) skip("A stopped test server's port could not be listened on again")
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

  # A response that cannot have a body is sent without one, and the file
  # that a body was written to is removed. libcurl reads no body for HEAD,
  # so the transfer is replaced by one that returns a written body.
  for (case in list(list(method = "GET", status = 304L), list(method = "GET", status = 204L),
                    list(method = "HEAD", status = 200L))) {
    spooled <- tempfile("tss-response-")
    writeBin(payload, spooled)
    local_mocked_bindings(fetch_backend_async = function(url, handle, ...) promises::promise_resolve(list(
      status_code = case$status, headers = charToRaw(sprintf("HTTP/1.1 %d X\r\n\r\n", case$status)),
      content = list(file = spooled, owned = TRUE)
    )))
    response <- await_response(forward_request(case$method, paste0(backend$url, "/large"), list(), "app", config))
    expect_equal(response$status, case$status)
    expect_null(response$body)
    expect_false(file.exists(spooled), info = paste(case$method, case$status))
  }
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
  # On a loaded check machine the time limit can pass before any of the
  # response has arrived, and nothing was then written
  if (length(paths) == 0) skip_on_cran()
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
  local_mocked_bindings(fetch_backend_async = function(url, handle, ...) promises::promise_resolve(NULL))
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
  # libcurl 8.20.0 and later write the hex digits of a percent-encoded
  # character in upper case, so "%2e" can reach the app as "%2E"
  upper_hex <- function(x) gsub("(%[0-9A-Fa-f]{2})", "\\U\\1", x, perl = TRUE)
  expect_identical(upper_hex(seen), upper_hex(paths))
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
    fetch_backend_async = function(url, handle, ...) promises::promise_reject(structure(
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
  stop_test_http_server(backend)
  # Windows retries a refused loopback connection for about two seconds
  refused <- tryCatch(
    await_response(fetch_backend_async(backend$url, curl::new_handle(connecttimeout_ms = 500))),
    error = function(e) e
  )
  # On a shared check machine another program can take the freed port, and
  # the transfer then connects
  if (!inherits(refused, "backend_connect_error")) skip_on_cran()
  expect_s3_class(refused, "backend_connect_error")
  expect_s3_class(refused, "error")

  release <- NULL
  stalled <- start_test_http_server(function(req) promises::promise(function(resolve, reject) {
    release <<- resolve
  }))
  on.exit(httpuv::stopServer(stalled$server), add = TRUE)
  # curl must see the connection open before the transfer times out, which a
  # loaded machine could delay past a shorter limit
  timed_out <- tryCatch(
    await_response(fetch_backend_async(stalled$url, curl::new_handle(timeout_ms = 1000))),
    error = function(e) e
  )
  # On a loaded check machine the transfer can time out before it connects
  if (is.null(release)) skip_on_cran()
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
  # The app here runs in this R session, which a busy machine can hold up for
  # longer than libcurl's usual wait for the app's answer. libcurl would then
  # send the body before the app could refuse it.
  local_mocked_bindings(EXPECT_CONTINUE_TIMEOUT_MS = 30000)
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
  socket_waits <- 0
  later_real <- later::later
  later_fd_real <- later::later_fd
  local_mocked_bindings(
    later = function(func, delay = 0, ...) {
      if (delay > 0 && delay <= 0.01) timer_polls <<- timer_polls + 1
      later_real(func, delay, ...)
    },
    later_fd = function(func, ...) {
      socket_waits <<- socket_waits + 1
      later_fd_real(func, ...)
    },
    .package = "later"
  )
  body <- raw(4 * 1024^2)
  handle <- curl::new_handle(customrequest = "POST", postfields = body)
  response <- await_response(fetch_backend_async(backend$url, handle))
  expect_equal(response$status_code, 200)
  expect_equal(received, length(body))
  expect_lt(timer_polls, 5)
  # A transfer that polled on a timer of any length, or on none, would never
  # wait on its socket
  expect_gt(socket_waits, 0)
})

test_that("startup grace expiry returns 503 without blocking timers", {
  backend <- start_test_http_server(function(req) create_html_response("unused"))
  stop_test_http_server(backend)
  # The grace period must not have run out before the request arrives, or no
  # wait happens; a loaded machine could take longer than a short one
  config <- proxy_test_config(backend$port, timeout = 1)
  config$set_app_starting("app")
  ran <- FALSE
  later::later(function() {ran <<- TRUE}, 0.05)
  result <- forward_request("GET", backend$url, list(), "app", config)
  expect_true(promises::is.promise(result))
  result <- await_response(result)
  expect_true(ran)
  expect_equal(result$status, 503)
  expect_match(result$body, "starting up")
})

test_that("async transfer failures become 502 responses", {
  config <- proxy_test_config(3001)
  local_mocked_bindings(
    wait_for_backend = function(url, wait_seconds, ...) promises::promise_resolve(TRUE),
    fetch_backend_async = function(url, handle, ...) promises::promise_reject(simpleError("transfer failed")),
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

test_that("a browser that closes its WebSocket closes the app's socket and frees the app", {
  backend_closed <- FALSE
  backend <- start_test_http_server(function(req) create_html_response("backend"), on_ws = function(ws) {
    ws$onMessage(function(binary, message) ws$send(paste("echo", message)))
    ws$onClose(function() backend_closed <<- TRUE)
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  config$config$apps[[1]]$resident <- FALSE
  checks <- character()
  cm <- ConnectionManager$new(config, list(stop_app_immediately = function(app_name) NULL))
  schedule <- cm$schedule_session_check
  assign("schedule_session_check", function(app_name, ...) {
    checks <<- c(checks, app_name)
    schedule(app_name, ...)
  }, envir = cm)
  proxy <- start_test_http_server(function(req) create_html_response("proxy"), on_ws = function(ws) {
    handle_websocket_connection(ws, config, cm)
  })
  on.exit(httpuv::stopServer(proxy$server), add = TRUE)
  client <- websocket::WebSocket$new(paste0(sub("^http", "ws", proxy$url), "/proxy/app/websocket/"))
  on.exit(client$close(), add = TRUE, after = FALSE)
  echoed <- promises::promise(function(resolve, reject) {
    client$onOpen(function(event) client$send("init"))
    client$onMessage(function(event) resolve(event$data))
    client$onError(function(event) reject(simpleError(event$message)))
  })
  expect_equal(await_response(echoed), "echo init")
  expect_equal(config$get_app_connection_count("app"), 1)

  client$close()
  deadline <- Sys.time() + 5
  while (!backend_closed && Sys.time() < deadline) later::run_now(0.05)
  expect_true(backend_closed)
  expect_equal(config$get_app_connection_count("app"), 0)
  expect_length(config$get_all_ws_connections(), 0)
  expect_length(config$get_all_backend_connections(), 0)
  # The on-demand app's idle check is scheduled
  expect_true("app" %in% checks)
  cm$cancel_session_check("app")
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
  stop_test_http_server(backend)
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
  expect_equal(response$status_code, 307)
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
  # So long that no check runs out between a response and the expectations
  # after it, on any machine; the last check is given a short wait below
  config$HTTP_SESSION_GRACE_SECONDS <- 3600
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
  # The last response left a check pending, which stops the unused app
  expect_false(is.null(config$pending_session_checks$app))
  expect_true(cm$schedule_session_check("app", grace_seconds = 0.05))
  deadline <- Sys.time() + 5
  while (kills == 0 && Sys.time() < deadline) later::run_now(.01)
  if (kills == 0) skip_on_cran()
  expect_equal(kills, 1)
})

test_that("an app whose verified process has died no longer gets traffic on its port", {
  hits <- 0
  backend <- start_test_http_server(function(req) {
    hits <<- hits + 1
    create_html_response("foreign")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  alive <- TRUE
  config$add_app_process("app", list(is_alive = function() alive, get_pid = function() Sys.getpid()))
  local_mocked_bindings(process_owns_port = function(process, port) TRUE)
  expect_true(config$app_backend_verified("app"))

  # Until the health check removes the process, another program may hold
  # its port, and is not asked whether it owns it
  alive <- FALSE
  local_mocked_bindings(process_owns_port = function(process, port) stop("A dead process owns no port"))
  expect_false(config$app_backend_verified("app"))
  response <- await_response(forward_request("GET", backend$url,
    list(HTTP_COOKIE = "session=secret"), "app", config))
  expect_equal(response$status, 503)
  expect_equal(hits, 0)
})

test_that("an app's traffic does not reach a port that its process does not own", {
  # As the test below, but without starting a process, so that it runs on CRAN
  skip_if_not(ps::ps_is_supported())
  hits <- 0
  backend <- start_test_http_server(function(req) {
    hits <<- hits + 1
    create_html_response("foreign")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  # A live process other than this R session, which holds the port
  foreign <- list(is_alive = function() TRUE, get_pid = function() ps::ps_ppid())
  config$add_app_process("app", foreign)
  expect_true(process_owns_port(test_backend_process(), backend$port))
  expect_false(process_owns_port(foreign, backend$port))
  expect_false(config$app_backend_verified("app"))

  logged <- capture_test_log(response <- await_response(forward_request("GET", backend$url,
    list(HTTP_COOKIE = "session=secret"), "app", config)))
  expect_equal(response$status, 503)
  expect_true(any(grepl("its process does not own port", logged, fixed = TRUE)))

  closed <- FALSE
  client <- list(request = list(PATH_INFO = "/proxy/app/websocket/", HTTP_COOKIE = "session=secret"),
    close = function() closed <<- TRUE)
  cm <- ConnectionManager$new(config)
  capture_test_log({
    cm$add_client_connection("s", client, "app", "127.0.0.1", "test")
    expect_null(cm$create_backend_connection("app", "s", client))
  })
  expect_true(closed)
  expect_null(config$get_backend_connection("s"))
  expect_equal(hits, 0)
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
  expect_false(await_response(pm$check_app_ready("app", backend$port, app,
    started_at = Sys.time() - config$APP_STARTUP_TIMEOUT_SECONDS - 1)))
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

test_that("headers of the client's connection to the proxy are not passed on to an app", {
  # Proxy credentials in particular would reach every app, and its logs
  req <- httpuv_request(c("proxy-authorization" = "Basic c2VjcmV0", "proxy-authenticate" = "Basic",
    "te" = "trailers", "trailer" = "Expires", "upgrade" = "h2c", "keep-alive" = "timeout=5",
    "transfer-encoding" = "chunked", "accept" = "text/html"))
  expect_equal(forwarded_request_headers(req), list(ACCEPT = "text/html"))
})

test_that("headers that the client's Connection header names are not passed on to an app", {
  req <- httpuv_request(c("connection" = "close, X-Forwarded-User , x-hop", "x-forwarded-user" = "admin",
    "x-hop" = "1", "proxy-connection" = "keep-alive", "accept" = "text/html"))
  expect_equal(forwarded_request_headers(req), list(ACCEPT = "text/html"))
})

test_that("forwarding many request headers takes time in proportion to their number", {
  # httpuv admits about 80 KB of headers, which can be some 13,000 of them
  large <- list(HEADERS = stats::setNames(as.list(rep("", 16000)), paste0("a", seq_len(16000))))
  expect_length(forwarded_request_headers(large), 16000)
  # It takes some hundredths of a second, and work that grew with the square
  # of the count would take seconds; how long a loaded check machine takes is
  # not this package's to bound
  skip_on_cran()
  elapsed <- system.time(forwarded_request_headers(large))[["elapsed"]]
  expect_lt(elapsed, 0.5)
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

test_that("an app that sets a __Host- cookie is reported once", {
  rm(list = ls(host_cookie_apps), envir = host_cookie_apps)
  on.exit(rm(list = ls(host_cookie_apps), envir = host_cookie_apps), add = TRUE)
  headers <- function(cookie) charToRaw(paste0("HTTP/1.1 200 OK\r\nSet-Cookie: ", cookie, "\r\n\r\n"))
  logged <- capture_test_log({
    for (i in 1:3) {
      result <- proxy_response_headers(headers("__Host-sid=1; Path=/; Secure"), "http://127.0.0.1:3001/", "app", "GET")
    }
    proxy_response_headers(headers("sid=1; Path=/"), "http://127.0.0.1:3001/", "other", "GET")
    proxy_response_headers(headers("__host-sid=1; Secure"), "http://127.0.0.1:3001/", "other", "GET")
  })
  # The cookie is still scoped to its app, never shared with every app
  expect_equal(result$`set-cookie`, "__Host-sid=1; Path=/proxy/app/; Secure")
  expect_length(logged, 2)
  expect_match(logged[1], "App app sets a __Host- cookie", fixed = TRUE)
  expect_match(logged[2], "App other sets a __Host- cookie", fixed = TRUE)
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

  # libcurl adds a chunked response's trailer fields after its headers
  trailers <- charToRaw(paste0("HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nSet-Cookie: s=1\r\n",
    "Transfer-Encoding: chunked\r\n\r\nX-Checksum: abc\r\n\r\n"))
  result <- proxy_response_headers(trailers, "http://127.0.0.1:3001/", "app", "GET")
  expect_named(result, c("content-type", "set-cookie"))
  expect_equal(result$`set-cookie`, "s=1; Path=/proxy/app/")
})

test_that("a CR inside an app's header value does not start another header", {
  # As from an app that puts a visitor's text in a download's file name
  headers <- charToRaw(paste0("HTTP/1.1 200 OK\r\n",
    "Content-Disposition: attachment; filename=\"a\rSet-Cookie: sid=attacker\"\r\n",
    "X-Two: a\r\rRefresh: 0\r\n",
    "Content-Type: text/csv\r\n\r\n"))
  result <- proxy_response_headers(headers, "http://127.0.0.1:3001/", "app", "GET")
  expect_named(result, c("content-disposition", "x-two", "content-type"))
  expect_equal(result$`content-disposition`, "attachment; filename=\"a Set-Cookie: sid=attacker\"")
  expect_equal(result$`x-two`, "a  Refresh: 0")
  expect_equal(result$`content-type`, "text/csv")

  # Lines that end at a bare LF are read as libcurl reads them
  result <- proxy_response_headers(charToRaw("HTTP/1.1 200 OK\nContent-Type: text/plain\n\n"),
    "http://127.0.0.1:3001/", "app", "GET")
  expect_equal(result$`content-type`, "text/plain")
})

test_that("every Path of a cookie is scoped to the app, however it is spaced", {
  cookie <- function(value) {
    headers <- charToRaw(paste0("HTTP/1.1 200 OK\r\nSet-Cookie: ", value, "\r\n\r\n"))
    proxy_response_headers(headers, "http://127.0.0.1:3001/", "app", "GET")$`set-cookie`
  }
  # Browsers use the last Path and trim the spaces around its name
  expect_equal(cookie("sid=1; Path=/; Path =/"), "sid=1; Path=/proxy/app/; Path =/proxy/app/")
  expect_equal(cookie("sid=1; path = /x; Domain = localhost; HttpOnly"), "sid=1; path = /proxy/app/x; HttpOnly")
  expect_equal(cookie("sid=1;Path=/a"), "sid=1;Path=/proxy/app/a")
})

test_that("cookies with no path get an app-scoped default path", {
  headers <- charToRaw("HTTP/1.1 200 OK\r\nSet-Cookie: session=abc; HttpOnly\r\n\r\n")
  result <- proxy_response_headers(headers, "http://127.0.0.1:3001/", "app", "GET")
  expect_equal(result$`set-cookie`, "session=abc; HttpOnly; Path=/proxy/app/")
  nested <- proxy_response_headers(headers, "http://127.0.0.1:3001/folder/page?x=1", "app", "GET")
  expect_equal(nested$`set-cookie`, "session=abc; HttpOnly; Path=/proxy/app/folder")
})

test_that("a request path cannot add attributes to a cookie's default path", {
  headers <- charToRaw("HTTP/1.1 200 OK\r\nSet-Cookie: sid=abc123; HttpOnly\r\n\r\n")
  cookie <- function(path) proxy_response_headers(headers, paste0("http://127.0.0.1:3001", path), "app", "GET")$`set-cookie`
  # A ";" in the path would otherwise end the Path attribute
  crafted <- "/x;Domain=example.com;SameSite=None;Secure;Max-Age=31536000;y/page"
  expect_equal(cookie(crafted), "sid=abc123; HttpOnly; Path=/proxy/app/")
  # The directory is cut back to the last one before the character
  expect_equal(cookie("/a/b;Domain=example.com/c/page"), "sid=abc123; HttpOnly; Path=/proxy/app/a")
  expect_equal(cookie("/a/;x/page"), "sid=abc123; HttpOnly; Path=/proxy/app/a")
  expect_equal(cookie("/a,b/page"), "sid=abc123; HttpOnly; Path=/proxy/app/")
  # Characters that a URL path carries as they are, and encoded ones, are kept
  expect_equal(cookie("/a%3Bb/c-d_e.f~g/page"), "sid=abc123; HttpOnly; Path=/proxy/app/a%3Bb/c-d_e.f~g")
  for (path in c(crafted, "/a/b;Domain=example.com/c/page", "/a,b/page")) {
    expect_no_match(cookie(path), "Domain|SameSite|Max-Age", info = path)
  }
})

test_that("a response larger than max_response_size_mb is not kept or forwarded", {
  # A collector that passes the limit keeps nothing, in memory or on disk
  spooled <- function() list.files(tempdir(), pattern = "^tss-response-")
  before <- spooled()
  body <- response_body_collector(limit = 10, max_bytes = 25)
  body$write(as.raw(1:20))
  expect_false(body$exceeded())
  body$write(as.raw(1:20))
  expect_true(body$exceeded())
  body$write(as.raw(1:5))
  expect_identical(body$finish(), raw())
  expect_length(setdiff(spooled(), before), 0)

  payload <- as.raw(seq_len(4096) %% 251)
  backend <- start_test_http_server(function(req) {
    list(status = 200L, headers = list("Content-Type" = "application/octet-stream"), body = payload)
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  config$config$max_response_size_mb <- 1024 / 1024^2
  logged <- capture_test_log({
    response <- await_response(forward_request("GET", backend$url, list(), "app", config), timeout = 10)
  })
  expect_equal(response$status, 502)
  expect_true(any(grepl("larger than max_response_size_mb", logged, fixed = TRUE)))

  # Within the limit, the response is forwarded
  config$config$max_response_size_mb <- 1
  response <- await_response(forward_request("GET", backend$url, list(), "app", config), timeout = 10)
  expect_equal(response$status, 200)
  expect_identical(response$body, payload)
})

test_that("a response that passes max_response_size_mb while it arrives ends its transfer", {
  spooled <- function() list.files(tempdir(), pattern = "^tss-response-")
  before <- spooled()
  payload <- raw(8 * 1024^2)
  backend <- start_test_http_server(function(req) {
    list(status = 200L, headers = list("Content-Type" = "application/octet-stream"), body = payload)
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  config$config$max_response_size_mb <- 1
  # Written to a file early on, so that a cancelled transfer must remove it
  collector <- response_body_collector
  local_mocked_bindings(response_body_collector = function(...) collector(limit = 256 * 1024, ...))
  cancel <- curl::multi_cancel
  cancelled <- 0
  local_mocked_bindings(multi_cancel = function(handle) {
    cancelled <<- cancelled + 1
    cancel(handle)
  }, .package = "curl")
  logged <- capture_test_log({
    response <- await_response(forward_request("GET", backend$url, list(), "app", config), timeout = 30)
  })
  expect_equal(response$status, 502)
  # The transfer ended before the app had sent the whole response
  expect_equal(cancelled, 1)
  expect_true(any(grepl("larger than max_response_size_mb", logged, fixed = TRUE)))
  expect_length(setdiff(spooled(), before), 0)
})

test_that("a response that cannot be written to its file is not kept, and fails with the reason", {
  spooled <- function() list.files(tempdir(), pattern = "^tss-response-")
  before <- spooled()
  writer <- curl::file_writer

  # The file cannot be opened, as on a full disk
  local_mocked_bindings(file_writer = function(path, ...) {
    function(x, close = FALSE) stop("Failed to open file: disk is full")
  }, .package = "curl")
  body <- response_body_collector(limit = 10)
  expect_no_error(body$write(as.raw(1:20)))
  expect_s3_class(body$error(), "backend_response_spool_error")
  expect_match(conditionMessage(body$error()), "disk is full", fixed = TRUE)
  expect_false(body$exceeded())
  expect_no_error(body$write(as.raw(1:20)))
  expect_null(body$finish())
  expect_no_error(body$discard())

  # The data is written, but the file cannot be closed
  local_mocked_bindings(file_writer = function(path, ...) {
    real <- writer(path, ...)
    function(x, close = FALSE) {
      if (close) {
        real(raw(), close = TRUE)
        stop("Failed to flush file: disk is full")
      }
      real(x)
    }
  }, .package = "curl")
  body <- response_body_collector(limit = 10)
  body$write(as.raw(1:20))
  path <- environment(body$write)$path
  expect_true(file.exists(path))
  expect_null(body$finish())
  expect_match(conditionMessage(body$error()), "Failed to flush file", fixed = TRUE)
  expect_false(file.exists(path))

  # A proxied request then fails with HTTP 502, and the log says why
  local_mocked_bindings(file_writer = function(path, ...) {
    function(x, close = FALSE) stop("Failed to open file: disk is full")
  }, .package = "curl")
  payload <- as.raw(seq_len(65536) %% 251)
  backend <- start_test_http_server(function(req) {
    list(status = 200L, headers = list("Content-Type" = "application/octet-stream"), body = payload)
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  collector <- response_body_collector
  local_mocked_bindings(response_body_collector = function(...) collector(limit = 1024, ...))
  logged <- capture_test_log({
    response <- await_response(forward_request("GET", backend$url, list(), "app", config), timeout = 10)
  })
  expect_equal(response$status, 502)
  expect_no_match(response$body, "disk")
  expect_true(any(grepl("could not be written to a temporary file: Failed to open file: disk is full", logged,
    fixed = TRUE)))
  expect_length(setdiff(spooled(), before), 0)
})

test_that("a transfer that cannot be driven is cancelled and fails", {
  backend <- start_test_http_server(function(req) promises::promise(function(resolve, reject) {
    later::later(function() resolve(create_html_response("late")), 0.5)
  }))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  cancel <- curl::multi_cancel
  cancelled <- 0
  local_mocked_bindings(
    multi_fdset = function(...) stop("Cannot list the transfer's sockets"),
    multi_cancel = function(handle) {
      cancelled <<- cancelled + 1
      cancel(handle)
    },
    .package = "curl"
  )
  expect_error(await_response(fetch_backend_async(backend$url, new_backend_handle())),
    "Cannot list the transfer's sockets")
  expect_equal(cancelled, 1)
})

test_that("a request whose app restarts while it waits for the app is not forwarded", {
  config <- proxy_test_config(3001)
  config$set_app_starting("app")
  replacement <- list(is_alive = function() TRUE, get_pid = function() -1L)
  fetched <- FALSE
  local_mocked_bindings(
    wait_for_backend = function(url, wait_seconds, ...) promises::promise(function(resolve, reject) {
      # A restart replaces the app's process while the request waits
      later::later(function() {
        config$add_app_process("app", replacement)
        resolve(TRUE)
      }, 0)
    }),
    fetch_backend_async = function(...) {
      fetched <<- TRUE
      promises::promise_reject(simpleError("must not be forwarded"))
    }
  )
  result <- await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "app", config))
  expect_equal(result$status, 503)
  expect_match(result$body, "App 'app' restarted, please retry", fixed = TRUE)
  expect_equal(result$headers[["Retry-After"]], "2")
  expect_false(fetched)
})

test_that("a request that waits for a starting app is answered as soon as the app's process fails", {
  local_mocked_bindings(fetch_backend_async = function(...) promises::promise_reject(simpleError("Connection refused")))
  wait <- function(process_found_dead) {
    config <- proxy_test_config(3001, timeout = 15)
    # Long, so that what is left of it does not depend on the machine's speed
    config$config$restart_delay <- 3600
    alive <- TRUE
    config$add_app_process("app", list(is_alive = function() alive, get_pid = function() -1L))
    config$set_app_starting("app")
    # The app's process fails at startup, whether or not the readiness check
    # has found it yet
    later::later(function() {
      alive <<- FALSE
      if (process_found_dead) {
        config$remove_app_process("app")
        config$set_app_ready("app")
        config$note_start_failure("app")
      }
    }, 0.3)
    started <- Sys.time()
    result <- await_response(forward_request("GET", "http://127.0.0.1:3001/", list(), "app", config), timeout = 10)
    list(result = result, seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
      backoff = config$start_backoff_seconds("app"))
  }

  # The answer names the wait before the app can start again, not
  # appstart_timeout later, also when the request sees the failure first
  for (process_found_dead in c(TRUE, FALSE)) {
    found <- wait(process_found_dead)
    expect_lt(found$seconds, 10)
    expect_equal(found$result$status, 503)
    expect_match(found$result$body, "restarting after a failure", fixed = TRUE)
    retry <- as.numeric(found$result$headers[["Retry-After"]])
    expect_true(retry > 3000 && retry <= 3600, info = process_found_dead)
    expect_gt(found$backoff, 3000)
  }
})

test_that("an interrupt during a transfer's callbacks is noted for the event loop", {
  backend <- start_test_http_server(function(req) create_html_response("body"))
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  interrupt <- structure(class = c("interrupt", "condition"), list(message = "", call = NULL))
  # As a Ctrl-C that R notices while the proxy receives a response
  collector <- response_body_collector
  local_mocked_bindings(response_body_collector = function(...) {
    body <- collector(...)
    write <- body$write
    body$write <- function(...) {
      signalCondition(interrupt)
      write(...)
    }
    body
  })

  # The curl package runs the callbacks where no handler of the server's
  # reaches, and an interrupt there would only fail the transfer
  config <- ShinyServerConfig$new()
  response <- await_response(fetch_backend_async(backend$url, new_backend_handle(), config = config))
  expect_equal(response$status_code, 200)
  expect_true(config$interrupt_requested)

  # A request to an app, and the wait for one that starts, pass the config on
  config <- proxy_test_config(backend$port)
  local_mocked_bindings(process_owns_port = function(process, port) TRUE)
  expect_equal(await_response(forward_request("GET", backend$url, list(), "app", config))$status, 200)
  expect_true(config$interrupt_requested)
  config <- ShinyServerConfig$new()
  expect_true(await_response(wait_for_backend(backend$url, config = config)))
  expect_true(config$interrupt_requested)
})

test_that("an app receives no header that the client did not send", {
  seen <- NULL
  backend <- start_test_http_server(function(req) {
    seen <<- req$HEADERS
    create_html_response("ok")
  })
  on.exit(httpuv::stopServer(backend$server), add = TRUE)
  config <- proxy_test_config(backend$port)
  send <- function(headers) {
    expect_equal(await_response(forward_request("GET", backend$url, httpuv_request(headers), "app", config))$status, 200)
    seen
  }

  # libcurl adds "Accept: */*" to a request without one, and the curl package
  # a User-Agent that names this server's R version and platform
  bare <- send(list(host = "localhost"))
  expect_false(any(c("accept", "user-agent") %in% names(bare)))
  sent <- send(list(accept = "text/plain", "user-agent" = "client/1"))
  expect_equal(sent[["accept"]], "text/plain")
  expect_equal(sent[["user-agent"]], "client/1")
})
