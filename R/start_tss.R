#' Start Tiny Shiny Server
#'
#' Launch the Tiny Shiny Server using a configuration JSON file.
#'
#' @param config Character path to a configuration JSON file. Defaults to
#'   "config.json" in the current working directory.
#' @return Invisibly returns the TinyShinyServer instance after starting.
#' @examples
#' \dontrun{
#'   library(tinyshinyserver)
#'   start_tss(config = "./config.json")
#' }
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