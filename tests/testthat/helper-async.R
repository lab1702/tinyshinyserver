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
