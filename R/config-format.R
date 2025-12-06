#' Configuration File Format
#'
#' Details about the JSON configuration file format used by \code{\link{start_tss}}.
#'
#' @section Configuration Structure:
#' The configuration file should be a valid JSON file with the following structure:
#'
#' \preformatted{
#' {
#'   "apps": [
#'     {
#'       "name": "app-name",
#'       "path": "./path/to/app",
#'       "resident": true|false
#'     }
#'   ],
#'   "starting_port": 3001,
#'   "proxy_port": 3838,
#'   "proxy_host": "127.0.0.1",
#'   "management_port": 3839,
#'   "restart_delay": 5,
#'   "health_check_interval": 10,
#'   "log_dir": "./logs"
#' }
#' }
#'
#' @section Required Fields:
#' \describe{
#'   \item{\code{apps}}{Array of Shiny applications to host. Each app must have \code{name} and \code{path}.}
#'   \item{\code{starting_port}}{Starting port number for automatic app port assignment.}
#'   \item{\code{log_dir}}{Directory where server and application logs will be written.}
#' }
#'
#' @section Optional Fields:
#' \describe{
#'   \item{\code{proxy_port}}{Port for the main proxy server (default: 3838).}
#'   \item{\code{proxy_host}}{Host interface to bind to (default: "127.0.0.1").}
#'   \item{\code{management_port}}{Port for the management interface (default: 3839).}
#'   \item{\code{restart_delay}}{Seconds to wait before restarting failed apps (default: 5).}
#'   \item{\code{health_check_interval}}{Seconds between health checks (default: 10).}
#' }
#'
#' @section Application Configuration:
#' Each application in the \code{apps} array can have:
#' \describe{
#'   \item{\code{name}}{Unique identifier used in URLs and logs. Required.}
#'   \item{\code{path}}{File system path to the app directory. Required.}
#'   \item{\code{resident}}{Boolean. If \code{true}, app runs continuously. If \code{false} (default), app starts on-demand.}
#' }
#'
#' @section Host Configuration:
#' The \code{proxy_host} field controls which network interface the server binds to:
#' \itemize{
#'   \item \code{"127.0.0.1"} or \code{"localhost"}: Localhost only (most secure)
#'   \item \code{"0.0.0.0"}: All network interfaces (allows external access)
#'   \item \code{"::1"}: IPv6 localhost
#'   \item \code{"::"}: All IPv6 interfaces
#' }
#'
#' @section Port Assignment:
#' Apps are automatically assigned ports starting from \code{starting_port}, skipping
#' any reserved ports (\code{proxy_port} and \code{management_port}). For example,
#' with \code{starting_port: 3001}, apps might get ports 3001, 3002, 3003, etc.,
#' but will skip 3838 and 3839 if those are the proxy and management ports.
#'
#' @examples
#' \dontrun{
#' # Example configuration file:
#' config_content <- '
#' {
#'   "apps": [
#'     {
#'       "name": "dashboard",
#'       "path": "./apps/dashboard",
#'       "resident": true
#'     },
#'     {
#'       "name": "reports",
#'       "path": "./apps/reports",
#'       "resident": false
#'     }
#'   ],
#'   "starting_port": 3001,
#'   "proxy_port": 3838,
#'   "management_port": 3839,
#'   "log_dir": "./logs"
#' }'
#'
#' # Write to file and use
#' writeLines(config_content, "my-config.json")
#' start_tss(config = "my-config.json")
#' }
#'
#' @seealso
#' \code{\link{start_tss}} for starting the server with a configuration file.
#'
#' Use \code{system.file("examples", "config.json", package = "tinyshinyserver")}
#' to see a complete example configuration file.
#'
#' @name config-format
#' @docType data
#' @keywords datasets
NULL
