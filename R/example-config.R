#' Example Configuration File
#'
#' The package includes an example configuration file, \file{config.json},
#' and the four apps that it serves. Use them to try the server, or as a
#' starting point for your own configuration. See \code{\link{config-format}}
#' for a description of each field.
#'
#' @details
#' The example configuration uses the default proxy and management ports
#' (3838 and 3839), assigns app ports from 3001, and serves these apps:
#' \describe{
#'   \item{\code{sales}}{A Shiny app in one file, \file{app.R}. Resident: it
#'     starts with the server.}
#'   \item{\code{inventory}}{A Shiny app in two files, \file{ui.R} and
#'     \file{server.R}. On demand: it starts when first opened.}
#'   \item{\code{reports}}{An interactive R Markdown flexdashboard. On
#'     demand. Needs Pandoc and the \pkg{DT}, \pkg{plotly}, \pkg{dplyr}, and
#'     \pkg{flexdashboard} packages.}
#'   \item{\code{dashboard}}{A Quarto dashboard. Resident, so the server
#'     needs the Quarto CLI from the start. Needs the \pkg{DT},
#'     \pkg{plotly}, and \pkg{dplyr} packages.}
#' }
#'
#' The app paths and the log directory are relative, so copy the
#' \file{examples} directory and run the server from the directory that
#' contains the copy, as in the example below. Locate the installed files
#' with \code{system.file("examples", package = "tinyshinyserver")}.
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
#'     cat(readLines("examples/config.json"), sep = "\n")
#'     start_tss(config = "examples/config.json")
#'   })()
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{start_tss}} for starting the server
#'   \item \code{\link{config-format}} for configuration file format details
#' }
#'
#' @name example-config
NULL
