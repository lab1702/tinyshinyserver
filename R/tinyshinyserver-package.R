#' tinyshinyserver: Tiny Shiny Server - Lightweight Multi-App Shiny Proxy
#'
#' A lightweight, WebSocket-enabled proxy server for hosting multiple Shiny
#' applications with automatic health monitoring, session management, and
#' resource cleanup.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{start_tss}}}{Start the Tiny Shiny Server with a configuration file}
#' }
#'
#' @section Key features:
#' \itemize{
#'   \item Host multiple Shiny applications behind a single proxy
#'   \item Resident (always-running) and on-demand application modes
#'   \item WebSocket support with session affinity
#'   \item Web dashboard for monitoring, restarting apps, and shutting down
#'   \item Automatic restart of failed resident apps
#'   \item Cross-platform support (Windows, Linux, macOS)
#'   \item Support for interactive R Markdown documents and Quarto dashboards
#' }
#'
#' @section Getting started:
#' See \code{\link{example-config}} to copy and run the included apps and to
#' learn what they require. Use \code{\link{start_tss}} to launch the server and
#' \code{\link{config-format}} to configure your own apps.
#'
#' @section Package resources:
#' The installed package includes:
#' \itemize{
#'   \item Example apps and an example configuration file,
#'     \file{config.json}, in the \file{examples} directory
#'   \item HTML templates and styles for the web pages in the \file{templates}
#'     directory
#' }
#'
#' Use \code{system.file("examples", package = "tinyshinyserver")} to locate
#' the example files.
#'
#' @docType package
#' @name tinyshinyserver-package
#' @aliases tinyshinyserver
#' @keywords package
#' @importFrom callr r_bg
#' @importFrom glue glue
#' @importFrom httpuv startServer service stopServer
#' @importFrom later later run_now
#' @importFrom methods new
#' @importFrom quarto quarto_serve
#' @importFrom rmarkdown run
#' @importFrom shiny runApp
#' @importFrom tools file_ext
#' @importFrom utils capture.output head sessionInfo tail
#' @importFrom websocket WebSocket
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
