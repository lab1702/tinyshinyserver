#' tinyshinyserver: Tiny Shiny Server - Lightweight Multi-App Shiny Proxy
#'
#' Serves several Shiny apps, interactive R Markdown documents, and Quarto
#' dashboards from one R session. A lightweight proxy forwards HTTP and
#' WebSocket traffic to the apps, each of which runs in its own R process.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{start_tss}}}{Start the Tiny Shiny Server with a configuration file}
#' }
#'
#' @section Key features:
#' \itemize{
#'   \item One proxy for many apps, each in its own R process
#'   \item Resident apps, which start with the server and restart after a
#'     failure, and on-demand apps, which start when first opened
#'   \item Shiny apps, R Markdown documents with \code{runtime: shiny}, and
#'     Quarto documents with \code{server: shiny}
#'   \item A web dashboard and API to monitor apps and connections, restart
#'     apps, reload the configuration, and shut down
#'   \item Windows, Linux, and macOS support
#' }
#'
#' @section Getting started:
#' See \code{\link{example-config}} to copy and run the included apps and to
#' learn what they require. Use \code{\link{start_tss}} to launch the server
#' and \code{\link{config-format}} to configure your own apps. The README, at
#' \url{https://github.com/lab1702/tinyshinyserver}, covers deployment behind
#' a reverse proxy, the management API, and troubleshooting.
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
