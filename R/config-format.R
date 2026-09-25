#' Configuration File Format
#'
#' Describes the JSON configuration file that \code{\link{start_tss}} reads.
#'
#' @section Configuration Structure:
#' The configuration file must be valid JSON with the following structure:
#'
#' \preformatted{
#' {
#'   "apps": [
#'     {
#'       "name": "app-name",
#'       "path": "./path/to/app",
#'       "resident": true,
#'       "appstart_timeout": 2
#'     }
#'   ],
#'   "starting_port": 3001,
#'   "proxy_port": 3838,
#'   "proxy_host": "127.0.0.1",
#'   "management_port": 3839,
#'   "restart_delay": 5,
#'   "health_check_interval": 10,
#'   "max_request_size_mb": 100,
#'   "title": "Tiny Shiny Server",
#'   "log_dir": "./logs"
#' }
#' }
#'
#' Relative application paths and \code{log_dir} are resolved from the R working
#' directory (\code{getwd()}), not the configuration file's directory. JSON does
#' not allow comments.
#'
#' @section Required Fields:
#' \describe{
#'   \item{\code{apps}}{Array of apps to host. Each app must have a \code{name} and a \code{path}; see \sQuote{Application Configuration}.}
#'   \item{\code{starting_port}}{First port to try when assigning ports to apps; see \sQuote{Port Assignment}.}
#'   \item{\code{log_dir}}{Directory for the server and app logs. The server writes \code{server.log}, and each app writes \code{<name>_output.log} and \code{<name>_error.log}; the previous run's logs are kept as \code{<name>_output.prev.log} and \code{<name>_error.prev.log}.}
#' }
#'
#' @section Optional Fields:
#' \describe{
#'   \item{\code{proxy_port}}{Port for the proxy, which serves the landing page and the apps (default: 3838).}
#'   \item{\code{proxy_host}}{Network interface the proxy listens on (default: \code{"127.0.0.1"}); see \sQuote{Host Configuration}.}
#'   \item{\code{management_port}}{Port for the management dashboard and API (default: 3839).}
#'   \item{\code{restart_delay}}{Seconds to wait before restarting a failed resident app; a non-negative finite number (default: 5).}
#'   \item{\code{health_check_interval}}{Seconds between health checks; a positive finite number (default: 10).}
#'   \item{\code{max_request_size_mb}}{Largest HTTP request body, in megabytes, that the proxy accepts; a positive finite number (default: 100). Larger bodies, and bodies sent with chunked transfer encoding, are rejected with HTTP 413 before they are read. Apps that raise \code{shiny.maxRequestSize} above this need a larger value.}
#'   \item{\code{title}}{Name shown in the browser tab and top bar of the landing and management pages; a non-empty string of up to 100 characters (default: \code{"Tiny Shiny Server"}).}
#' }
#'
#' @section Application Configuration:
#' Each application in the \code{apps} array can have:
#' \describe{
#'   \item{\code{name}}{Unique identifier used in URLs and log file names: 1--50 ASCII letters, digits, underscores, or hyphens. Required.}
#'   \item{\code{path}}{Path to the app directory, either absolute or relative to the R working directory. Required.}
#'   \item{\code{resident}}{Boolean. If \code{true}, the app starts with the server and is restarted if its process fails. If \code{false} (the default), the app starts on demand and stops 30 seconds after its last WebSocket connection closes.}
#'   \item{\code{appstart_timeout}}{Seconds, measured from app startup, that a request waits for the app to become ready before the proxy returns HTTP 503; a positive finite number (default: 2). Fractional seconds are supported.}
#' }
#'
#' @section Host Configuration:
#' The \code{proxy_host} field controls the proxy's listening interface. The
#' management server and backend apps always bind to \code{127.0.0.1}.
#' Supported proxy hosts are:
#' \itemize{
#'   \item \code{"127.0.0.1"} or \code{"localhost"}: Localhost only (most secure)
#'   \item \code{"0.0.0.0"}: All network interfaces (allows external access)
#'   \item \code{"::1"}: IPv6 localhost
#'   \item \code{"::"}: All IPv6 interfaces
#' }
#'
#' To protect against DNS rebinding, the management server rejects requests
#' whose \code{Host} header is not \code{localhost}, \code{127.0.0.1}, or
#' \code{[::1]}, and so does the proxy when \code{proxy_host} is a loopback
#' address. A reverse proxy on the same machine must therefore forward the
#' upstream address as the host and set \code{X-Forwarded-Host} to the public
#' host. A reverse proxy on another machine, used with \code{proxy_host} set to
#' \code{"0.0.0.0"} or \code{"::"}, must instead preserve the public
#' \code{Host} header. App WebSocket connections are accepted only when the
#' browser \code{Origin} matches the request host.
#'
#' App processes listen on their own loopback ports without these checks, so a
#' DNS-rebinding page in a browser on the server machine can still reach a
#' running app directly on its port.
#'
#' @section Port Assignment:
#' Apps are assigned ports in configuration order, starting from
#' \code{starting_port} and skipping \code{proxy_port}, \code{management_port},
#' and ports already in use. For example, with \code{starting_port} set to 3001
#' and no conflicts, three apps receive ports 3001, 3002, and 3003. A range that
#' reaches 3838 or 3839 skips those ports when they are the proxy and management
#' ports.
#'
#' @section Reloading:
#' The management dashboard's \emph{Reload Config & Restart All} button reads
#' the configuration file again, stops every app, and starts the resident apps
#' under the new configuration; ports are assigned again at that point. Changes
#' to \code{proxy_host}, \code{proxy_port}, or \code{management_port} are
#' rejected and take effect only when the server restarts.
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
#'     config_content <- '{
#'       "apps": [
#'         {"name": "sales", "path": "./examples/sales", "resident": true},
#'         {"name": "inventory", "path": "./examples/inventory", "resident": false}
#'       ],
#'       "starting_port": 3001,
#'       "proxy_port": 3838,
#'       "management_port": 3839,
#'       "log_dir": "./logs"
#'     }'
#'     writeLines(config_content, "my-config.json")
#'     start_tss(config = "my-config.json")
#'   })()
#' }
#'
#' @seealso
#' \code{\link{start_tss}} for starting the server with a configuration file.
#'
#' Use \code{system.file("examples", "config.json", package = "tinyshinyserver")}
#' to see a complete example configuration file.
#'
#' @name config-format
NULL
