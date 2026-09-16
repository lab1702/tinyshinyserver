start_test_http_server <- function(call, on_ws = NULL) {
  for (port in sample(20000:50000, 3)) {
    server <- tryCatch(httpuv::startServer("127.0.0.1", port, list(call = call, onWSOpen = on_ws), quiet = TRUE),
      error = function(e) NULL)
    if (!is.null(server)) return(list(server = server, port = port, url = paste0("http://127.0.0.1:", port)))
  }
  skip("Loopback sockets unavailable")
}

# Await async handlers in tests while continuing to service httpuv/later events.
await_response <- function(value, timeout = 5) {
  if (!promises::is.promise(value)) return(value)
  finished <- FALSE
  result <- NULL
  error <- NULL
  promises::then(value,
    onFulfilled = function(response) { result <<- response; finished <<- TRUE },
    onRejected = function(e) { error <<- e; finished <<- TRUE }
  )
  deadline <- Sys.time() + timeout
  while (!finished && Sys.time() < deadline) later::run_now(0.01)
  if (!finished) stop("Async response did not finish within test timeout")
  if (!is.null(error)) stop(error)
  result
}

# Bypass mocked liveness/termination helpers and explicitly reap real test apps.
stop_test_app_processes <- function(config) {
  for (process in config$get_all_app_processes()) {
    if (is.function(process$kill_tree)) {
      process$kill_tree()
      process$wait(timeout = 5000)
    }
  }
  config$app_processes <- list()
}
