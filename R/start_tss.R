#' Start Tiny Shiny Server
#'
#' Starts the proxy, the management server, and the resident apps listed in a
#' JSON configuration file, then serves requests until the server is shut down.
#' On-demand apps start when they are first accessed.
#'
#' @param config Path to the JSON configuration file, as a character string.
#'   Defaults to \file{config.json} in the current working directory. See
#'   \code{\link{config-format}} for the file format.
#'
#' @return The internal server object (a reference class object of class
#'   \code{TinyShinyServer}), invisibly, after the server stops. The function
#'   is called for its side effect: it blocks the R session until it is
#'   interrupted (for example, with Ctrl-C) or shut down through the management
#'   dashboard or API. If an unexpected error stops the server, the function
#'   signals an error once the servers and apps have stopped, so that a script
#'   run with \command{Rscript} exits with a failure status. The returned
#'   object cannot be started again.
#'
#' @details
#' See \code{\link{config-format}} for the required fields, defaults, app
#' lifecycle settings, and port assignment. Relative app paths and the log
#' directory are resolved from the R working directory, not from the
#' configuration file's directory.
#'
#' With the default ports, the server is reached at:
#' \itemize{
#'   \item Landing page: \verb{http://localhost:3838}
#'   \item Management dashboard: \verb{http://localhost:3839}
#'   \item Each app: \verb{http://localhost:3838/proxy/{app_name}/}
#' }
#'
#' @examples
#' if (interactive()) {
#'   (function() {
#'     example_dir <- tempfile("tss-example-")
#'     dir.create(example_dir)
#'     old_dir <- setwd(example_dir)
#'     on.exit({
#'       setwd(old_dir)
#'       unlink(example_dir, recursive = TRUE)
#'     }, add = TRUE)
#'     examples_path <- system.file("examples", package = "tinyshinyserver")
#'     file.copy(examples_path, ".", recursive = TRUE)
#'     start_tss(config = "examples/config.json")
#'   })()
#' }
#'
#' @export
start_tss <- function(config = "config.json") {
  if (!is.character(config) || length(config) != 1 || is.na(config) || !nzchar(config)) {
    stop("`config` must be the path to a configuration file, as a single string", call. = FALSE)
  }
  if (!file.exists(config)) {
    stop(sprintf("Configuration file not found: %s", config))
  }
  # Create and start the server
  server <- TinyShinyServer$new(config)
  server$start()
  invisible(server)
}
