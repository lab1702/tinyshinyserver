# HTTP and WebSocket Handlers Module
# Handles all HTTP requests and WebSocket connections

# httpuv receives a request's whole body before calling the app, and the proxy
# then reads it into memory to forward it, so reject oversized bodies from the
# headers alone, as Shiny's own server does. Chunked bodies have no declared
# size and are rejected, whatever length the request also declares. The
# headers are read under the names the client sent: req$CONTENT_LENGTH would
# also answer for a "Content_Length" header that states a smaller size.
reject_large_request_body <- function(req, max_bytes) {
  size <- 0
  declared <- request_header(req, "content-length")
  if (!is.null(request_header(req, "transfer-encoding"))) {
    size <- Inf
  } else if (!is.null(declared)) {
    size <- suppressWarnings(as.numeric(declared))
  }
  if (is.na(size) || size > max_bytes) {
    return(create_error_response("Request body too large", 413))
  }
  NULL
}

# HTTP request handler
handle_http_request <- function(req, config, template_manager, connection_manager, process_manager = NULL) {
  "Main HTTP request handler with routing"

  response <- respond_safely(answer_http_request(req, config, template_manager, connection_manager, process_manager))
  without_head_body(req, response)
}

answer_http_request <- function(req, config, template_manager, connection_manager, process_manager = NULL) {
  "Validate and route a proxy request"

  if (!is_allowed_proxy_host(req, config)) {
    return(create_error_response("Invalid Host header", 403))
  }

  # The landing page's links are absolute, but send /base to /base/ anyway
  base_path <- config$config$base_path %||% ""
  if (nzchar(base_path) && identical(req$PATH_INFO, base_path)) {
    return(create_redirect_response(paste0(base_path, "/"), req$QUERY_STRING))
  }

  # Validate request inputs
  validation_result <- validate_request_inputs(
    strip_base_path(req$PATH_INFO, base_path),
    req$REQUEST_METHOD,
    req$QUERY_STRING
  )

  if (!validation_result$valid) {
    return(validation_result$response)
  }

  path <- validation_result$path
  method <- validation_result$method
  query_string <- validation_result$query_string

  logger::log_debug("HTTP request: {method} {path}", method = method, path = path)

  # Route handling
  return(route_http_request(
    path, method, query_string, req, config,
    template_manager, connection_manager, process_manager
  ))
}

route_http_request <- function(path, method, query_string, req, config, template_manager, connection_manager,
                               process_manager = NULL) {
  "Route HTTP requests to appropriate handlers"

  # Proxy requests to apps, in any method the proxy forwards
  if (startsWith(path, "/proxy/")) {
    return(handle_proxy_request(
      path, method, query_string, req, config,
      process_manager, connection_manager, template_manager
    ))
  }

  # The proxy's own pages and endpoints are read-only
  own_route <- path %in% c("/", "", "/health", "/api/apps") || startsWith(path, "/templates/")
  if (own_route && !method %in% c("GET", "HEAD")) {
    return(create_method_not_allowed_response())
  }

  # Landing page
  if (path == "/" || path == "") {
    return(handle_landing_page(config, template_manager))
  }

  # Health check endpoint
  if (path == "/health") {
    return(handle_health_check())
  }

  # API endpoint for app status
  if (path == "/api/apps") {
    return(handle_apps_api(config, process_manager))
  }

  # Static files (CSS, JS, images)
  if (startsWith(path, "/templates/")) {
    return(handle_static_file(path, template_manager, request_header(req, "if-none-match")))
  }

  # 404 for unknown paths
  return(create_error_response("Not Found", 404))
}

handle_landing_page <- function(config, template_manager) {
  "Handle landing page requests"

  tryCatch(
    {
      html <- template_manager$generate_landing_page(config)
      return(create_html_response(html))
    },
    error = function(e) {
      logger::log_error("Error generating landing page: {error}", error = e$message)
      return(create_error_response("Internal Server Error", 500))
    }
  )
}

handle_health_check <- function() {
  "Handle health check requests"

  return(create_json_response(list(status = "healthy")))
}

handle_apps_api <- function(config, process_manager = NULL) {
  "Handle API requests for app status"

  # This would normally be in management_api.R but needed here for landing page
  tryCatch(
    {
      # App state lives in config, so a manager made here reports the same status
      process_manager <- process_manager %||% create_process_manager(config)
      apps_status <- process_manager$get_all_app_status()

      # The proxy port may be public, so expose only what the landing page
      # needs; paths, ports, and PIDs stay on the loopback management API
      apps_status <- lapply(apps_status, function(app) {
        app[intersect(c("name", "status", "resident", "connections"), names(app))]
      })

      response <- create_json_response(apps_status)
      # The proxy answers with and without the base_path prefix, so an open
      # landing page learns from this that a reload has changed it
      response$headers[["X-TinyShinyServer-Base-Path"]] <- config$config$base_path %||% ""
      return(response)
    },
    error = function(e) {
      logger::log_error("Error getting app status: {error}", error = e$message)
      return(create_error_response("Internal Server Error", 500))
    }
  )
}

handle_static_file <- function(path, template_manager, if_none_match = NULL) {
  "Handle static file requests (CSS, JS, images)"

  # Remove /templates/ prefix
  file_path <- gsub("^/templates/", "", path)

  return(template_manager$serve_static_file(file_path, if_none_match))
}

# The route to an app, /proxy/<app name><path the app sees>, for HTTP requests
# and WebSocket handshakes alike. `backend_path` is "" when the app name ends
# the path.
parse_proxy_path <- function(path) {
  invalid <- function(error) list(valid = FALSE, error = error)

  if (!is.character(path) || length(path) != 1 || is.na(path) ||
    !startsWith(path, "/proxy/") || startsWith(path, "/proxy//")) {
    return(invalid("Invalid proxy path"))
  }

  rest <- substring(path, nchar("/proxy/") + 1L)
  app_name <- sub("/.*$", "", rest)
  if (app_name == "") {
    return(invalid("Invalid proxy path"))
  }

  app_name_validation <- validate_app_name(app_name)
  if (!app_name_validation$valid) {
    return(invalid(paste("Invalid app name:", app_name_validation$error)))
  }

  list(valid = TRUE, app_name = app_name, backend_path = substring(rest, nchar(app_name) + 1L))
}

# Whether a request asks an R Markdown or Quarto app for a file other than
# the document it serves. rmarkdown::run() serves both, and unless a document
# is prerendered it renders, and so runs the code of, whatever file under the
# app directory a WebSocket's path names: a partial whose name starts with
# "_", a draft in a subdirectory, or an R script, which it spins into a
# report. As rmarkdown does, the path is decoded before "/websocket/" is
# removed, so that an encoded "%2Fwebsocket%2F" names the same file. Only the
# document's own name is allowed; any other name of a document or R script is
# refused, and so is a WebSocket for any other file, found with file.path()
# as rmarkdown finds it, which on Windows also finds "/_child.Rmd/". Of HTTP
# requests, rmarkdown answers those for a document's page; other files, such
# as images, are not rendered and are served as usual.
is_other_document_path <- function(document, backend_path, websocket = FALSE) {
  if (is.null(document) || !document$type %in% c("rmd", "qmd")) {
    return(FALSE)
  }
  path <- tryCatch(utils::URLdecode(backend_path), error = function(e) backend_path)
  # A decoded path that is not UTF-8 cannot be compared, and is refused
  tryCatch({
    if (websocket) path <- sub("/websocket/$", "", path)
    if (path %in% c("", "/") || is_document_name(path, document$file)) {
      return(FALSE)
    }
    if (grepl("\\.([rq]md|r)/*$", path, ignore.case = TRUE)) {
      return(TRUE)
    }
    if (!websocket) {
      return(FALSE)
    }
    # A socket path that leaves the document's directory names no file that
    # the document could render, and is refused before the file system is
    # asked about it: whether the socket were then refused or opened would
    # tell any client which files exist anywhere on the server.
    if (grepl("(^|/)\\.\\.(/|$)", path) || grepl("\\", path, fixed = TRUE)) {
      return(TRUE)
    }
    # A path that names no file, or a directory, renders nothing
    target <- file.path(dirname(document$file), path)
    file.exists(target) && !dir.exists(target)
  }, error = function(e) TRUE)
}

# Whether a request path, such as "/report.Rmd", names a document itself. A
# name in another letter case names it on a file system that ignores case,
# but on one that does not, it may name another file of the directory.
is_document_name <- function(path, file) {
  name <- sub("^/", "", path)
  if (identical(name, basename(file))) {
    return(TRUE)
  }
  identical(tolower(name), tolower(basename(file))) && !name %in% list.files(dirname(file), all.files = TRUE)
}

# The document that an app's latest process served, or that it would serve if
# it started now, for an app that has not run
served_document <- function(app_config, config) {
  recorded <- config$get_app_document(app_config$name)
  if (!is.null(recorded)) {
    return(recorded)
  }
  # A directory known not to hold an app is not read again at every request
  if (is.character(app_config$path) && !is.null(config$cached_start_problem(app_config))) {
    return(NULL)
  }
  tryCatch(detect_app_type(normalizePath(app_config$path, mustWork = FALSE)), error = function(e) NULL)
}

# Start an on-demand app that a request or WebSocket has arrived for, unless it
# is already running. FALSE when the app could not be started. The process
# manager logs the start, and why an app cannot start.
ensure_on_demand_app_started <- function(app_config, config, process_manager, trigger) {
  app_name <- app_config$name
  if (app_config$resident || is.null(process_manager) || is_process_alive(config$get_app_process(app_name))) {
    return(TRUE)
  }
  logger::log_debug("Starting non-resident app {app_name} on demand for {trigger}",
    app_name = app_name, trigger = trigger
  )
  isTRUE(process_manager$start_app_on_demand(app_name))
}

handle_proxy_request <- function(path, method, query_string, req, config, process_manager = NULL,
                                 connection_manager = NULL, template_manager = NULL) {
  "Handle proxy requests to Shiny apps"

  route <- parse_proxy_path(path)
  if (!route$valid) {
    return(create_error_response(route$error, 400))
  }

  app_name <- route$app_name
  app_config <- config$get_app_config(app_name)

  if (is.null(app_config)) {
    return(create_error_response("App not found", 404))
  }

  # Canonicalize the mount URL before starting an app or forwarding a body.
  if (route$backend_path == "") {
    return(create_redirect_response(paste0(config$config$base_path, path, "/"), query_string))
  }

  # Refused before an on-demand app starts, which nothing would then stop
  if (is_other_document_path(served_document(app_config, config), route$backend_path)) {
    return(create_error_response("Not Found", 404))
  }

  if (!ensure_on_demand_app_started(app_config, config, process_manager, "HTTP request")) {
    # An app whose process just failed starts again after restart_delay
    backoff <- ceiling(config$start_backoff_seconds(app_name))
    if (backoff > 0) {
      return(restart_wait_response(app_name, backoff, req, method, template_manager, app_config))
    }
    return(create_error_response("Failed to start app on demand", 502))
  }

  # A start records the document anew, as the directory now holds it. An app
  # started for nothing is checked for idleness, as after any request.
  if (is_other_document_path(config$get_app_document(app_name), route$backend_path)) {
    (connection_manager %||% create_connection_manager(config, process_manager))$schedule_session_check(app_name)
    return(create_error_response("Not Found", 404))
  }

  # Only the routing prefix is removed; the backend path is passed on verbatim.
  target_url <- paste0("http://127.0.0.1:", app_config$port, route$backend_path)

  # httpuv includes the leading question mark in QUERY_STRING.
  if (!is.null(query_string) && query_string != "") {
    target_url <- paste0(target_url, if (startsWith(query_string, "?")) "" else "?", query_string)
  }

  # Keep the backend alive until this request finishes, including failures.
  manager <- connection_manager %||% create_connection_manager(config, process_manager)
  manager$begin_http_request(app_name)
  released <- FALSE
  release <- function(response_bytes = 0) {
    if (!released) {
      released <<- TRUE
      manager$end_http_request(app_name, response_bytes)
    }
  }
  tryCatch({
    response <- forward_request(method, target_url, req, app_name, config, template_manager)
    if (promises::is.promise(response)) {
      # The response's size tells how long the browser may take to receive it
      sized <- promises::then(response, function(value) {
        release(tryCatch(response_body_bytes(value[["body"]]), error = function(e) 0))
        value
      })
      return(promises::finally(sized, release))
    }
    release(tryCatch(response_body_bytes(response[["body"]]), error = function(e) 0))
    response
  }, error = function(e) {
    release()
    stop(e)
  })
}

# Request bodies above this size are offered to the app with
# "Expect: 100-continue" before they are sent. libcurl writes a smaller body
# in one piece, right after the request headers.
EXPECT_CONTINUE_BYTES <- 65536
# How long libcurl waits for the app to agree to such a body before it sends
# the body anyway, as it does by default
EXPECT_CONTINUE_TIMEOUT_MS <- 1000

# Request headers that are not passed on to an app: they describe the client's
# own connection to the proxy, or httpuv has already acted on them.
UNFORWARDED_REQUEST_HEADERS <- c(
  "HOST", "CONNECTION", "KEEP_ALIVE", "TRANSFER_ENCODING",
  "TE", "TRAILER", "UPGRADE", "PROXY_AUTHORIZATION",
  "PROXY_AUTHENTICATE", "PROXY_CONNECTION", "CONTENT_LENGTH", "CONTENT_TYPE",
  # httpuv has already answered the client's expectation and read the body
  "EXPECT"
)

forwarded_request_headers <- function(req, skip = character()) {
  "The request headers to send to an app, for HTTP requests and WebSocket handshakes alike"

  sent <- request_headers(req)
  if (length(sent) == 0) {
    return(list())
  }
  # Selected in one pass: a request may carry thousands of headers, and
  # building the list one header at a time took seconds of the event loop
  sent_names <- names(sent)
  header_names <- toupper(sent_names)
  keys <- gsub("-", "_", header_names, fixed = TRUE)
  # The headers that the client's Connection header names belong to its
  # connection to the proxy, as the fixed ones above do. A name that is not
  # ASCII names no header, and trimws() cannot read one that is not UTF-8.
  connection <- as.character(unlist(sent[tolower(sent_names) == "connection"], use.names = FALSE))
  nominated <- unlist(strsplit(connection, ",", fixed = TRUE, useBytes = TRUE))
  nominated <- toupper(trimws(nominated[is_ascii(nominated)]))
  nominated <- gsub("-", "_", nominated[nominated != ""], fixed = TRUE)
  # An app's own server reads "X_Forwarded_User" as X-Forwarded-User, in
  # place of the value a reverse proxy set, so no header whose name has an
  # underscore is passed on. Of headers sent twice, the last one is kept.
  keep <- !grepl("_", sent_names, fixed = TRUE) &
    !keys %in% c(UNFORWARDED_REQUEST_HEADERS, nominated, skip) &
    !duplicated(header_names, fromLast = TRUE)
  headers <- as.list(sent[keep])
  names(headers) <- header_names[keep]
  headers
}

# The proxy's own answers that mean "try again shortly". A browser opening the
# app's page cannot act on a JSON error or a Retry-After header, so it receives
# a page that reloads itself, with the same status and headers; every other
# client receives the JSON. delay_seconds is how long the app waits before it
# can start, after a failure; the page's own limit on its wait begins then.
as_starting_page <- function(response, req, method, app_name, template_manager, retry_seconds,
                             answer_seconds = 2, delay_seconds = 0) {
  opens_page <- identical(method, "GET") &&
    grepl("text/html", request_header(req, "accept") %||% "", fixed = TRUE)
  if (is.null(template_manager) || !opens_page) {
    return(response)
  }
  page <- tryCatch(
    template_manager$generate_starting_page(app_name, retry_seconds, answer_seconds, delay_seconds),
    error = function(e) {
      logger::log_error("Error generating the starting page: {error}", error = conditionMessage(e))
      NULL
    }
  )
  if (!is.null(page)) {
    response$body <- page
    response$headers[["Content-Type"]] <- "text/html"
  }
  response
}

# The answer to a request for an app whose process failed, while the app waits
# backoff seconds, the rest of restart_delay, before it can start again
restart_wait_response <- function(app_name, backoff, req, method, template_manager, app_config) {
  as_starting_page(
    create_503_response(sprintf("App '%s' is restarting after a failure, please retry", app_name), backoff),
    req, method, app_name, template_manager, backoff, app_config$appstart_timeout %||% 2,
    delay_seconds = backoff
  )
}

forward_request <- function(method, target_url, req, app_name, config, template_manager = NULL) {
  "Forward HTTP requests without blocking the server's event loop"

  proxy_error <- function(e) {
    logger::log_error("Proxy error for app {app_name}: {error}",
      app_name = app_name, error = conditionMessage(e)
    )
    # curl errors name the app's internal address, so keep them in the log
    create_error_response("Bad Gateway", 502)
  }

  retry_seconds <- 2
  for_browser <- function(response) {
    as_starting_page(response, req, method, app_name, template_manager, retry_seconds,
      config$get_app_config(app_name)$appstart_timeout %||% 2
    )
  }
  unavailable <- function(message) {
    for_browser(create_503_response(message, retry_seconds))
  }

  tryCatch({
    startup_state <- config$get_app_startup_state(app_name)
    app_config <- config$get_app_config(app_name)
    request_process <- config$get_app_process(app_name)
    grace_period <- app_config$appstart_timeout %||% 2
    wait_seconds <- 0
    starting <- !is.null(startup_state) && startup_state$state == "starting"
    # A startup that took too long is reported to one request, and only if the
    # app still does not answer: its process is still running and may yet do
    # so. This is the one place that ends a timed-out startup state.
    timed_out <- !is.null(startup_state) && startup_state$state == "timeout"
    if (timed_out) config$set_app_ready(app_name)
    if (starting) {
      wait_seconds <- max(0, grace_period - startup_state$elapsed)
      if (wait_seconds == 0) {
        return(unavailable(sprintf("App '%s' is starting up, please retry", app_name)))
      }
    }

    # Capture the request body while the httpuv input stream is available.
    # A new curl handle below owns all transport state for this request only.
    headers <- forwarded_request_headers(req)
    body <- NULL
    if (!method %in% c("GET", "HEAD", "OPTIONS")) {
      if (!is.null(req$rook.input)) body <- req$rook.input$read()
      # A body is passed on when the client sent one, or declared an empty one
      # with "Content-Length: 0". It keeps the client's Content-Type: libcurl
      # gives a body without one "application/x-www-form-urlencoded", which a
      # header with an empty value prevents.
      declared <- !is.null(request_header(req, "content-length"))
      if (length(body) > 0 || declared) {
        body <- body %||% raw()
        headers[["Content-Type"]] <- request_header(req, "content-type") %||% ""
      } else {
        body <- NULL
      }
    }

    # A running app whose own process holds its port needs no probe. While an
    # app starts, or when its port cannot be verified, probe first so the
    # request is never sent before the app is ready.
    probe <- starting || !config$app_backend_verified(app_name)
    # The wait ends as soon as the app's process exits or is replaced: a
    # process that fails at startup never answers, and the request is told so
    # at once rather than after appstart_timeout
    process_ended <- function() {
      !identical(config$get_app_process(app_name), request_process) ||
        (probe && !is.null(request_process) && !is_process_alive(request_process))
    }
    backend_ready <- if (probe) {
      wait_for_backend(target_url, wait_seconds, stop_waiting = process_ended)
    } else {
      promises::promise_resolve(TRUE)
    }

    response <- promises::then(backend_ready, function(ready) {
      if (process_ended()) {
        # An app whose process failed starts again after restart_delay
        backoff <- ceiling(config$start_backoff_seconds(app_name))
        if (backoff > 0) {
          return(restart_wait_response(app_name, backoff, req, method, template_manager, app_config))
        }
        message <- if (is_process_alive(config$get_app_process(app_name))) {
          "App '%s' restarted, please retry"
        } else {
          "App '%s' stopped, please retry"
        }
        return(unavailable(sprintf(message, app_name)))
      }
      if (ready && !config$app_backend_verified(app_name)) {
        logger::log_warn("Not forwarding to app {app_name}: its process does not own port {port}",
          app_name = app_name, port = app_config$port
        )
        ready <- FALSE
      }
      if (!ready) {
        if (timed_out) {
          return(for_browser(create_error_response("App startup timed out", 502)))
        }
        message <- if (starting) {
          sprintf("App '%s' is starting up, please retry", app_name)
        } else {
          sprintf("App '%s' is not ready", app_name)
        }
        return(unavailable(message))
      }
      if (starting) config$set_app_ready(app_name)
      # Bound stalled transfers rather than total time, so slow downloads and
      # documents that render during the request still complete. The handle and
      # its pool are never reused, so close the backend connection when done
      # instead of leaving it open until garbage collection. The app receives
      # the path as the client sent it: libcurl would otherwise remove "." and
      # ".." segments, including their "%2e" spellings.
      handle <- new_backend_handle(
        customrequest = method, nobody = identical(method, "HEAD"), path_as_is = TRUE,
        connecttimeout = 10, low_speed_limit = 1, low_speed_time = 600, followlocation = FALSE,
        accept_encoding = "identity", http_content_decoding = FALSE, forbid_reuse = TRUE
      )
      # An app can refuse an upload from its headers alone, as Shiny does above
      # shiny.maxRequestSize, and then closes the connection. Body data sent
      # after that resets the connection, which can discard the app's answer
      # before it is read. So offer a large body first and send it once the
      # app agrees. The curl package turns this handshake off: it sets no wait
      # and handle_setheaders() adds an empty "Expect" header, hence the
      # lower-case name.
      if (length(body) > EXPECT_CONTINUE_BYTES) {
        headers[["expect"]] <- "100-continue"
        curl::handle_setopt(handle, expect_100_timeout_ms = EXPECT_CONTINUE_TIMEOUT_MS)
      }
      curl::handle_setheaders(handle, .list = headers)
      if (!is.null(body)) {
        curl::handle_setopt(handle, postfields = body)
      }
      max_response_bytes <- (config$config$max_response_size_mb %||% DEFAULT_MAX_RESPONSE_SIZE_MB) * 1024^2
      promises::then(fetch_backend_async(target_url, handle, max_response_bytes), function(response) {
        response_headers <- proxy_response_headers(
          response$headers, target_url, app_name, method, config$config$base_path %||% ""
        )
        # A response that cannot have a body is sent without one. Given an
        # empty body, httpuv compresses it for a client that accepts gzip and
        # sends the result, which may not follow these responses.
        status <- response$status_code
        if (identical(method, "HEAD") || status < 200 || status %in% c(204L, 304L)) {
          discard_response_body(response$content)
          return(list(status = status, headers = response_headers, body = NULL))
        }
        # httpuv sends the body unchanged, from memory or from the file a
        # large one was written to. Scanning or converting the bytes would
        # briefly multiply a large download's memory use.
        list(
          status = status,
          headers = response_headers,
          body = response$content
        )
      }, onRejected = function(e) {
        if (!inherits(e, "backend_connect_error")) stop(e)
        # The app no longer listens where it was verified, so check its port
        # again before the next request is forwarded.
        logger::log_warn("App {app_name} did not accept a connection: {error}",
          app_name = app_name, error = conditionMessage(e)
        )
        if (exists(app_name, envir = config$verified_backends, inherits = FALSE)) {
          rm(list = app_name, envir = config$verified_backends)
        }
        unavailable(sprintf("App '%s' is not ready", app_name))
      })
    })
    promises::then(response, onRejected = proxy_error)
  }, error = proxy_error)
}

# Whether strings hold only ASCII characters, which tolower() can read in any
# encoding, including "bytes"
is_ascii <- function(x) !grepl("[^\001-\177]", x, useBytes = TRUE)

# The header fields of an app's final response, from libcurl's raw header
# block, as a list named by lower-case field name. Each value keeps the bytes
# the app sent, other than a CR (see below). curl::parse_headers_list()
# returns no fields at all for a block that holds a byte that is not valid
# UTF-8, such as a Latin-1 file name, and the response would then be sent
# without its Content-Type, cookies, or redirect. A line whose field name is
# not an HTTP token is left out.
parse_response_headers <- function(raw_headers) {
  text <- rawToChar(raw_headers)
  Encoding(text) <- "bytes"
  # Lines end at LF, as libcurl reads them. A CR elsewhere is part of a value,
  # and becomes a space (RFC 9112, section 2.2): httpuv ends every header that
  # it sends with CRLF, so a CR passed on, or read as a line end here, would
  # let an app that puts a visitor's text in a header, such as a download's
  # file name, give that visitor's link a header of its own, such as a cookie.
  blocks <- strsplit(text, "\r?\n\r?\n", useBytes = TRUE)[[1]]
  if (length(blocks) == 0) {
    return(list())
  }
  # The final response's block is the last one that starts with a status line.
  # libcurl adds the trailer fields of a chunked response after it, and those
  # would otherwise take the place of its Content-Type, cookies, and redirect.
  responses <- which(startsWith(blocks, "HTTP/"))
  final <- if (length(responses) > 0) responses[length(responses)] else length(blocks)
  lines <- strsplit(blocks[final], "\r?\n", useBytes = TRUE)[[1]]
  lines <- gsub("\r", " ", lines, fixed = TRUE, useBytes = TRUE)
  colon <- regexpr(":", lines, fixed = TRUE, useBytes = TRUE)
  names <- substr(lines, 1, colon - 1)
  fields <- colon > 1 & grepl("^[!#$%&'*+.^_`|~0-9A-Za-z-]+$", names, useBytes = TRUE)
  values <- as.list(trimws(substr(lines[fields], colon[fields] + 1, nchar(lines[fields], type = "bytes"))))
  names(values) <- tolower(names[fields])
  values
}

# Preserve end-to-end headers, including repeated Set-Cookie fields. Transfer
# framing belongs to httpuv; libcurl already removes chunk framing, but content
# decoding is disabled so Content-Encoding still describes the returned bytes.
# A response the app sent without a Content-Type is passed on without one.
proxy_response_headers <- function(raw_headers, target_url, app_name, method, base_path = "") {
  headers <- parse_response_headers(raw_headers)
  connection <- as.character(unlist(headers[names(headers) == "connection"], use.names = FALSE))
  nominated <- trimws(unlist(strsplit(connection, ",", fixed = TRUE, useBytes = TRUE)))
  nominated <- tolower(nominated[is_ascii(nominated)])
  # httpuv adds its own Date to every response; the app's would be a second one
  excluded <- c("connection", "keep-alive", "transfer-encoding", "te", "trailer",
    "upgrade", "proxy-authenticate", "proxy-authorization", "date", nominated)
  if (method != "HEAD") excluded <- c(excluded, "content-length")
  headers <- headers[!names(headers) %in% excluded]
  # Redirects and cookie paths must name the app's public URL
  prefix <- paste0(base_path, "/proxy/", app_name)
  authority <- sub("^(https?://[^/?#]+).*", "\\1", target_url)
  for (i in seq_along(headers)) {
    value <- headers[[i]]
    if (names(headers)[i] == "location") {
      # Keep backend-local redirects on this app's public proxy route.
      parts <- regmatches(value, regexec("^(?:https?:)?//([^/?#]+)(.*)$", value,
        ignore.case = TRUE, perl = TRUE))[[1]]
      backend_authority <- sub("^https?://", "", authority)
      backend_local <- length(parts) == 3 && is_ascii(parts[2]) &&
        tolower(parts[2]) == tolower(backend_authority)
      if (backend_local) {
        value <- parts[3]
        if (!startsWith(value, "/")) value <- paste0("/", value)
      }
      if (backend_local || (startsWith(value, "/") && !startsWith(value, "//"))) {
        value <- paste0(prefix, value)
      }
      headers[[i]] <- value
    } else if (names(headers)[i] == "set-cookie") {
      # Backend cookies must not become shared cookies for every hosted app.
      # Browsers trim spaces around attribute names and values and use the
      # last Path, so every Path attribute is rewritten, however it is spaced.
      path_attribute <- "(;[[:space:]]*path[[:space:]]*=[[:space:]]*)(/[^;]*)"
      if (!grepl(path_attribute, value, ignore.case = TRUE, perl = TRUE)) {
        request_path <- sub("[?#].*$", "", substring(target_url, nchar(authority) + 1))
        default_path <- sub("/[^/]*$", "", request_path)
        # The request path is the client's, and a ";" in it would end the
        # Path attribute and start attributes of the client's choosing, such
        # as Domain. The default path is then cut back to the last directory
        # before any character that a URL path does not carry as it is, or
        # that separates cookie attributes. The cookie is sent to more of the
        # app's pages than a browser would choose, but never to another app.
        unsafe <- regexpr("[^A-Za-z0-9._~%!$&'()*+=:@/-]", default_path)
        if (unsafe > 0) default_path <- sub("/[^/]*$", "", substr(default_path, 1, unsafe - 1))
        if (default_path == "" || !startsWith(default_path, "/")) default_path <- "/"
        value <- paste0(value, "; Path=", default_path)
      }
      value <- gsub(path_attribute, paste0("\\1", prefix, "\\2"), value,
        ignore.case = TRUE, perl = TRUE)
      value <- gsub(";[[:space:]]*domain[[:space:]]*=[[:space:]]*\\.?((127\\.0\\.0\\.1)|localhost)[[:space:]]*(?=;|$)",
        "", value, ignore.case = TRUE, perl = TRUE)
      headers[[i]] <- value
    }
  }
  headers
}

# Apps listen on loopback, so their traffic must never follow http_proxy or
# ALL_PROXY: a proxy cannot reach the app, and would be sent users' cookies
# and request bodies.
new_backend_handle <- function(...) {
  curl::new_handle(..., noproxy = "*")
}

# A transfer that failed before a connection was established never reached
# the app. Such failures carry the class "backend_connect_error".
backend_fetch_error <- function(message, handle) {
  connected <- tryCatch(
    curl::handle_data(handle)$times[["connect"]] > 0,
    error = function(e) TRUE
  )
  structure(
    class = c(if (!isTRUE(connected)) "backend_connect_error", "simpleError", "error", "condition"),
    list(message = as.character(message), call = NULL)
  )
}

# An app's response is held in memory up to this size. A larger one is written
# to a temporary file as it arrives and sent from there, so that a big
# download does not occupy the memory of the process that serves every app.
RESPONSE_SPOOL_BYTES <- 8 * 1024^2

# Collects a response body from curl's data callback. finish() returns it in
# a form httpuv sends: a raw vector, or a file that httpuv deletes once it has
# been sent. A body larger than max_bytes is not kept: exceeded() then
# reports it, and the transfer is to be cancelled. Nor is a body that cannot
# be written to its file, as when the disk is full: error() then returns the
# reason, as it does a body that is too large, and finish() returns NULL.
response_body_collector <- function(limit = RESPONSE_SPOOL_BYTES, max_bytes = Inf) {
  chunks <- list()
  size <- 0
  path <- NULL
  writer <- NULL
  too_large <- FALSE
  failure <- NULL
  # Closing a writer whose file cannot be written can itself fail, and a
  # failure must still remove the file
  close_writer <- function() {
    if (!is.null(writer)) {
      closing <- writer
      writer <<- NULL
      tryCatch(closing(raw(), close = TRUE), error = function(e) NULL)
    }
  }
  discard <- function() {
    chunks <<- list()
    close_writer()
    if (!is.null(path)) {
      unlink(path)
      path <<- NULL
    }
  }
  # An error raised in curl's data callback reaches only the console, and the
  # transfer then fails with a message that names no cause. The cause is kept
  # instead, so that the request fails with it and the log says what happened.
  fail <- function(e) {
    failure <<- response_spool_error(e)
    discard()
  }
  list(
    write = function(data, final = FALSE) {
      if (length(data) == 0 || too_large || !is.null(failure)) {
        return(invisible(NULL))
      }
      size <<- size + length(data)
      if (size > max_bytes) {
        too_large <<- TRUE
        discard()
        return(invisible(NULL))
      }
      tryCatch({
        if (is.null(writer) && size > limit) {
          # curl's writer holds no R connection, of which a session has few.
          # A cleaner of /tmp may have removed R's session directory on a
          # server that runs for weeks; tempdir() then creates it again.
          path <<- tempfile("tss-response-", tmpdir = tempdir(check = TRUE))
          writer <<- curl::file_writer(path)
          for (chunk in chunks) writer(chunk)
          chunks <<- list()
        }
        if (is.null(writer)) chunks[[length(chunks) + 1L]] <<- data else writer(data)
      }, error = fail)
      invisible(NULL)
    },
    finish = function() {
      if (!is.null(failure)) {
        return(NULL)
      }
      if (is.null(writer)) {
        return(if (length(chunks) > 0) unlist(chunks, use.names = FALSE) else raw())
      }
      closed <- tryCatch({
        writer(raw(), close = TRUE)
        TRUE
      }, error = function(e) {
        fail(e)
        FALSE
      })
      if (!closed) {
        return(NULL)
      }
      # The file now belongs to the response: a later discard() leaves it
      spooled <- list(file = path, owned = TRUE)
      writer <<- NULL
      path <<- NULL
      spooled
    },
    discard = discard,
    exceeded = function() too_large,
    error = function() {
      if (too_large) response_too_large_error(max_bytes) else failure
    }
  )
}

# The error of a transfer whose response could not be written to its file
response_spool_error <- function(e) {
  structure(
    class = c("backend_response_spool_error", "simpleError", "error", "condition"),
    list(message = paste(
      "The app's response could not be written to a temporary file:", conditionMessage(e)
    ), call = NULL)
  )
}

# The error of a transfer whose response was larger than the proxy keeps
response_too_large_error <- function(max_bytes) {
  structure(
    class = c("backend_response_too_large", "simpleError", "error", "condition"),
    list(message = sprintf(
      "The app's response was larger than max_response_size_mb (%s MB) and was not forwarded",
      format(max_bytes / 1024^2, scientific = FALSE)
    ), call = NULL)
  )
}

# Remove the file of a collected body that will not be sent
discard_response_body <- function(body) {
  if (is.list(body) && !is.null(body$file)) unlink(body$file)
}

# Drive libcurl without blocking: after each step, wait for the transfer's own
# sockets, so other HTTP and WebSocket callbacks run in between and a transfer
# continues as soon as it can. Polling on a timer instead sent one buffer per
# tick, which capped uploads at a few megabytes per second. Each transfer has
# its own pool and fresh easy handle; neither cookies nor authentication state
# can survive into another request. The response's `content` is what
# response_body_collector() returns. A response larger than max_bytes ends
# the transfer: the proxy sends nothing before it has the whole response, so
# an app that streams without end, or that the visitor has stopped waiting
# for, would otherwise fill the disk and keep an on-demand app in use.
fetch_backend_async <- function(url, handle, max_bytes = Inf) {
  promises::promise(function(resolve, reject) {
    pool <- curl::new_pool()
    body <- response_body_collector(max_bytes = max_bytes)
    curl::curl_fetch_multi(url, handle = handle, pool = pool,
      data = body$write,
      done = function(response) {
        content <- if (is.null(body$error())) body$finish()
        if (!is.null(body$error())) {
          reject(body$error())
          return()
        }
        response$content <- content
        resolve(response)
      },
      fail = function(message) {
        body$discard()
        reject(backend_fetch_error(message, handle))
      }
    )
    # later_fd() passes the sockets' readiness, which multi_run() finds itself
    pump <- function(...) {
      tryCatch({
        result <- curl::multi_run(timeout = 0, pool = pool)
        # A body that is too large, or that cannot be written, ends the
        # transfer rather than leaving libcurl to receive it to the end
        if (result$pending > 0 && !is.null(body$error())) {
          curl::multi_cancel(handle)
          reject(body$error())
          return()
        }
        if (result$pending > 0) {
          fds <- curl::multi_fdset(pool)
          # libcurl's timers (connect and stall timeouts) need a call even
          # when no socket becomes ready; its timeout is in milliseconds
          wait <- if (fds$timeout < 0) 1 else min(fds$timeout / 1000, 1)
          if (length(fds$reads) + length(fds$writes) + length(fds$exceptions) == 0) {
            # Nothing to wait on yet, for example before the socket exists
            later::later(pump, min(wait, 0.01))
          } else {
            later::later_fd(pump, fds$reads, fds$writes, fds$exceptions, timeout = wait)
          }
        }
      }, error = function(e) {
        curl::multi_cancel(handle)
        body$discard()
        reject(e)
      })
    }
    later::later(pump, 0)
  })
}

# Probe TCP readiness without sending an HTTP request (in particular, never
# replay a POST while waiting for startup). Both the probe and retry are async.
# The wait ends early, with FALSE, once stop_waiting(), if given, is TRUE.
wait_for_backend <- function(url, wait_seconds = 0, stop_waiting = NULL) {
  promises::promise(function(resolve, reject) {
    # R has no clock that cannot be set back, so the wait adds up the steps of
    # the clock between probes and counts a step back as none: a clock set
    # back does not keep a request waiting for as long as it was set back.
    waited <- 0
    last <- Sys.time()
    seconds_left <- function() {
      now <- Sys.time()
      waited <<- waited + max(0, as.numeric(difftime(now, last, units = "secs")))
      last <<- now
      wait_seconds - waited
    }
    probe <- function() {
      remaining <- seconds_left()
      timeout_ms <- if (wait_seconds > 0) max(1, min(1000, remaining * 1000)) else 1000
      # Close the probe connection when it completes. Before libcurl 8, a
      # connect-only connection otherwise stays open in the unused pool until
      # garbage collection.
      handle <- new_backend_handle(connect_only = TRUE, forbid_reuse = TRUE,
        timeout_ms = ceiling(timeout_ms))
      promises::then(fetch_backend_async(url, handle),
        onFulfilled = function(response) resolve(TRUE),
        onRejected = function(error) {
          remaining <- seconds_left()
          if (remaining > 0 && !(is.function(stop_waiting) && isTRUE(stop_waiting()))) {
            later::later(probe, min(0.1, remaining))
          } else {
            resolve(FALSE)
          }
        }
      )
    }
    probe()
  })
}

is_allowed_proxy_host <- function(req, config) {
  "Whether the proxy should serve a request with this Host header"

  # A loopback-only proxy must not answer other names, or a DNS-rebinding
  # page could become same-origin with it and drive local apps
  proxy_host <- config$config$proxy_host %||% "127.0.0.1"
  if (proxy_host %in% c("127.0.0.1", "localhost", "::1")) {
    return(is_loopback_host_header(request_header(req, "host")))
  }
  TRUE
}

is_same_origin_websocket <- function(req) {
  "Whether a WebSocket handshake has no Origin or one matching the request host"

  origin <- request_header(req, "origin")
  if (is.null(origin) || identical(origin, "")) {
    return(TRUE) # Non-browser clients do not send Origin
  }
  # Browsers send host names in ASCII, and tolower() cannot read a value that
  # is not UTF-8; other values below are ignored for the same reason
  if (!is_ascii(origin)) {
    return(FALSE)
  }
  origin <- tolower(trimws(origin))
  match <- regmatches(origin, regexec("^(https?)://([^/]+)$", origin))[[1]]
  if (length(match) != 3) {
    return(FALSE)
  }
  normalize <- function(authority, default_port) {
    sub(paste0(":", default_port, "$"), "", tolower(trimws(authority)))
  }
  default_port <- if (match[2] == "https") "443" else "80"
  origin_authority <- normalize(match[3], default_port)

  # A handshake that reached the reverse proxy over HTTPS belongs to the HTTPS
  # site. A page on the plain-HTTP site of the same host, which an attacker on
  # the network can write, may not open it with that site's credentials.
  # Only this direction is refused: a reverse proxy behind another one that
  # ends TLS reports "http" for pages that the browser loaded over HTTPS.
  # Browsers cannot set this header on a handshake, so it is not forged by a
  # page, and a client that sets it only makes the check stricter.
  forwarded_proto <- request_header(req, "x-forwarded-proto")
  if (!is.null(forwarded_proto) && is_ascii(forwarded_proto) && match[2] == "http" &&
      identical(tolower(trimws(strsplit(forwarded_proto, ",", fixed = TRUE)[[1]][1])), "https")) {
    return(FALSE)
  }

  hosts <- request_header(req, "host")
  # A reverse proxy on this machine may rewrite Host but report the original
  remote_addr <- req$REMOTE_ADDR
  forwarded_host <- request_header(req, "x-forwarded-host")
  if (!is.null(remote_addr) && remote_addr %in% c("127.0.0.1", "::1", "::ffff:127.0.0.1") &&
      !is.null(forwarded_host)) {
    hosts <- c(hosts, strsplit(forwarded_host, ",", fixed = TRUE, useBytes = TRUE)[[1]])
  }
  hosts <- hosts[!is.na(hosts) & is_ascii(hosts)]
  hosts <- hosts[trimws(hosts) != ""]
  origin_authority %in% normalize(hosts, default_port)
}

# WebSocket handler
handle_websocket_connection <- function(ws, config, connection_manager, process_manager = NULL) {
  "Handle new WebSocket connections"

  # Handshakes that are refused below are logged by their reason, a flood of
  # them only at DEBUG; an accepted one is logged once it is added.
  logger::log_debug("WebSocket handshake received")
  reject <- function(reason, text) {
    connection_manager$note_rejected_handshake(reason, text)
    ws$close()
  }

  if (!is_allowed_proxy_host(ws$request, config)) {
    reject("host", paste("Rejecting WebSocket with Host", log_value(request_header(ws$request, "host"))))
    return()
  }

  # Browsers attach cookies and cached credentials to cross-site WebSocket
  # handshakes, so only accept sessions opened by pages served from this host
  if (!is_same_origin_websocket(ws$request)) {
    reject("origin", paste("Rejecting cross-origin WebSocket from", log_value(request_header(ws$request, "origin"))))
    return()
  }

  # Generate session ID
  session_id <- generate_session_id()

  # Determine which app this WebSocket is for
  request_path_validation <- validate_path(strip_base_path(ws$request$PATH_INFO, config$config$base_path))
  if (!request_path_validation$valid) {
    reject("path", paste("Invalid WebSocket path:", request_path_validation$error))
    return()
  }

  # The query string is passed on to the app, so it has the limit of an HTTP
  # request's
  query_validation <- validate_query_string(ws$request$QUERY_STRING)
  if (!query_validation$valid) {
    reject("query", paste("Invalid WebSocket query string:", query_validation$error))
    return()
  }

  # Do not fall back to another app when the path names none
  route <- parse_proxy_path(request_path_validation$sanitized)
  if (!route$valid) {
    reject("route", paste("Could not determine app for WebSocket connection:", route$error))
    return()
  }
  app_name <- route$app_name

  logger::log_debug("WebSocket routed to app: {app_name}", app_name = app_name)

  app_config <- config$get_app_config(app_name)
  if (is.null(app_config)) {
    reject("app", paste("Rejecting WebSocket for unknown app:", app_name))
    return()
  }

  # Refused before an on-demand app starts, which nothing would then stop
  if (is_other_document_path(served_document(app_config, config), route$backend_path, websocket = TRUE)) {
    reject("document", sprintf("Rejecting WebSocket for a document that app %s does not serve", app_name))
    return()
  }

  if (!ensure_on_demand_app_started(app_config, config, process_manager, "WebSocket connection")) {
    logger::log_error("Failed to start app {app_name} on demand for WebSocket", app_name = app_name)
    ws$close()
    return()
  }

  # A start records the document anew, as the directory now holds it
  if (is_other_document_path(config$get_app_document(app_name), route$backend_path, websocket = TRUE)) {
    connection_manager$schedule_session_check(app_name)
    reject("document", sprintf("Rejecting WebSocket for a document that app %s does not serve", app_name))
    return()
  }

  # Check if app is still starting up
  if (config$is_app_starting(app_name)) {
    logger::log_info("App {app_name} is starting, closing WebSocket with retry message", app_name = app_name)
    connection_manager$schedule_session_check(app_name,
      (app_config$appstart_timeout %||% 2) + config$HTTP_SESSION_GRACE_SECONDS)
    ws$send(jsonlite::toJSON(list(
      error = "App is starting",
      message = "The application is starting up. Please refresh the page in a few seconds.",
      retry_after_seconds = 3
    ), auto_unbox = TRUE))
    ws$close()
    return()
  }

  # Get client connection info
  client_ip <- get_client_ip(ws$request)
  user_agent <- request_header(ws$request, "user-agent") %||% "unknown"

  # Add connection to manager
  connection_manager$add_client_connection(session_id, ws, app_name, client_ip, user_agent)

  # Set up message handler
  # A refused message ends the session at once. httpuv reports the socket
  # closed only once the browser answers the close, and until then delivers
  # what the browser sends, so a client that ignores the close would keep the
  # session and its app socket open.
  end_session <- function() {
    if (is.null(config$get_ws_connection(session_id))) {
      tryCatch(ws$close(), error = function(e) {})
    } else {
      connection_manager$close_client_connection(session_id)
    }
  }
  ws$onMessage(function(binary, message) {
    # A session that has ended, here or by the proxy, takes no more messages
    if (is.null(config$get_ws_connection(session_id))) {
      return(invisible(NULL))
    }
    tryCatch(
      {
        success <- connection_manager$handle_client_message(session_id, message, app_name)
        if (!success) {
          tryCatch(ws$send(jsonlite::toJSON(list(error = "Invalid message"), auto_unbox = TRUE)), error = function(e) {})
          end_session()
        }
      },
      error = function(e) {
        logger::log_error("Error handling WebSocket message: {error}", error = e$message)
        tryCatch(end_session(), error = function(e) {})
      }
    )
  })

  # Set up close handler
  ws$onClose(function() {
    connection_manager$remove_client_connection(session_id)
  })

  # Connect to the app now rather than on the browser's first message: on
  # some sockets, such as Shiny's autoreload socket, only the app speaks.
  # Messages that arrive before the app's socket opens are queued.
  tryCatch(
    connection_manager$create_backend_connection(app_name, session_id, ws),
    error = function(e) {
      logger::log_error("Could not connect to app {app_name}: {error}",
        app_name = app_name, error = conditionMessage(e)
      )
      connection_manager$close_client_connection(session_id)
    }
  )
}
