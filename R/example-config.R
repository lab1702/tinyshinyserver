#' Example Configuration File
#'
#' Path to the example configuration file included with the package.
#'
#' @description
#' This provides the path to the example \code{config.json} file that demonstrates
#' the proper configuration format for \code{\link{start_tss}}. The file includes
#' sample applications and typical server settings.
#'
#' @details
#' The example configuration includes:
#' \itemize{
#'   \item Four sample Shiny applications (sales, inventory, reports, dashboard)
#'   \item Mix of resident and on-demand application modes
#'   \item Standard port configuration (proxy: 3838, management: 3839)
#'   \item Automatic port assignment starting from 3001
#'   \item Logging configuration
#' }
#'
#' @format
#' A JSON file with the standard configuration structure. See \code{\link{config-format}}
#' for details about the configuration format.
#'
#' @examples
#' \dontrun{
#' # Get the example configuration file path
#' config_file <- system.file("examples", "config.json", package = "tinyshinyserver")
#' 
#' # View the configuration
#' config_content <- readLines(config_file)
#' cat(config_content, sep = "\\n")
#' 
#' # Copy examples to current directory and start server
#' examples_path <- system.file("examples", package = "tinyshinyserver")
#' file.copy(examples_path, ".", recursive = TRUE)
#' start_tss(config = "examples/config.json")
#' }
#'
#' @seealso
#' \itemize{
#'   \item \code{\link{start_tss}} for starting the server
#'   \item \code{\link{config-format}} for configuration file format details
#' }
#'
#' @name example-config
#' @docType data
#' @keywords datasets
NULL