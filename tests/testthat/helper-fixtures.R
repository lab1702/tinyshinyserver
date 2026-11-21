# Fixtures shared by tests in more than one file

# Undo setup_logging() and remove the log directory it wrote to
restore_test_logging <- function(log_dir) {
  logger::log_threshold("INFO", namespace = "tinyshinyserver")
  logger::log_appender(logger::appender_console, namespace = "tinyshinyserver")
  unlink(log_dir, recursive = TRUE)
}

# The package's log lines written while `code` runs
capture_test_log <- function(code) {
  logged <- character()
  logger::log_threshold("INFO", namespace = "tinyshinyserver")
  logger::log_appender(function(lines) logged <<- c(logged, lines), namespace = "tinyshinyserver")
  on.exit(logger::log_appender(logger::appender_console, namespace = "tinyshinyserver"), add = TRUE)
  force(code)
  logged
}

# A request as httpuv presents it. HEADERS keeps the header names as the
# client sent them, in lower case. The HTTP_ entries write "-" as "_", so a
# header spelled with an underscore replaces the one spelled with a hyphen.
httpuv_request <- function(headers, ...) {
  req <- list(HEADERS = headers, ...)
  for (name in sort(names(headers), method = "radix")) {
    req[[paste0("HTTP_", toupper(gsub("-", "_", name)))]] <- headers[[name]]
  }
  if (!is.null(req$HTTP_CONTENT_LENGTH)) req$CONTENT_LENGTH <- req$HTTP_CONTENT_LENGTH
  if (!is.null(req$HTTP_CONTENT_TYPE)) req$CONTENT_TYPE <- req$HTTP_CONTENT_TYPE
  req
}

# The configuration that read_config() returns for a minimal file plus the given settings
read_test_config <- function(...) {
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  settings <- utils::modifyList(
    list(apps = list(list(name = "app", path = "/tmp")), log_dir = tempdir(), starting_port = 3001),
    list(...)
  )
  writeLines(jsonlite::toJSON(settings, auto_unbox = TRUE), path)
  ShinyServerConfig$new()$read_config(path)
}
