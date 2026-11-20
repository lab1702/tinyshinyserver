# Fixtures shared by tests in more than one file

# Undo setup_logging() and remove the log directory it wrote to
restore_test_logging <- function(log_dir) {
  logger::log_threshold("INFO", namespace = "tinyshinyserver")
  logger::log_appender(logger::appender_console, namespace = "tinyshinyserver")
  unlink(log_dir, recursive = TRUE)
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
