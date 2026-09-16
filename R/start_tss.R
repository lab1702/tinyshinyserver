#' Start Tiny Shiny Server
#'
#' Launch the Tiny Shiny Server using a configuration JSON file. This starts
#' a multi-application Shiny server with automatic health monitoring, session
#' management, and WebSocket support.
#'
#' @param config Character path to a configuration JSON file. Defaults to
#'   "config.json" in the current working directory. The configuration file
#'   should specify apps, ports, and other server settings.
#'
#' @return Invisibly returns the TinyShinyServer instance after the server stops.
#'   This function blocks until interrupted (Ctrl-C) or shut down via the
#'   management interface.
#'
#' @details
#' See \code{\link{config-format}} for required fields, defaults, app lifecycle
#' settings, and port assignment. Relative app paths and the log directory are
#' resolved from the R working directory, not the configuration file's directory.
#'
#' Access points with the default ports:
#' \itemize{
#'   \item Main landing page: \verb{http://localhost:3838}
#'   \item Management interface: \verb{http://localhost:3839}
#'   \item Individual apps: \verb{http://localhost:3838/proxy/{app_name}/}
#' }
#'
#' @examples
#' if (interactive()) {
#'   library(tinyshinyserver)
#'   examples_path <- system.file("examples", package = "tinyshinyserver")
#'   temp_path <- tempdir()
#'   file.copy(examples_path, temp_path, recursive = TRUE)
#'   setwd(temp_path)
#'   start_tss(config = "examples/config.json")
#' }
#'
#' @export
start_tss <- function(config = "config.json") {
  if (!file.exists(config)) {
    stop(sprintf("Configuration file not found: %s", config))
  }
  # Create and start the server
  server <- TinyShinyServer$new(config)
  server$start()
  invisible(server)
}
