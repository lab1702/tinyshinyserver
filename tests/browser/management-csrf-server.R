# Isolated servers for management-csrf.cjs; never starts or stops real apps.
pkgload::load_all(".", quiet = TRUE)
folder <- commandArgs(trailingOnly = TRUE)[1]
serve <- function(call) {
  for (port in sample(20000:50000, 10)) {
    server <- tryCatch(httpuv::startServer("127.0.0.1", port, list(call = call)), error = function(e) NULL)
    if (!is.null(server)) return(list(server = server, url = paste0("http://127.0.0.1:", port)))
  }
  stop("No test port available")
}
main <- function() {
  # A real file, so a reload request is checked against it
  config_file <- file.path(folder, "config.json")
  writeLines(jsonlite::toJSON(list(log_dir = folder, starting_port = 3001,
    apps = list(list(name = "app", path = "/test/app"))), auto_unbox = TRUE), config_file)
  config <- create_server_config(config_file)
  restarts <- 0
  pm <- list(get_app_status = function(name) list(status = "running"),
    get_all_app_status = function() list(app = list(name = "app", status = "running", resident = TRUE,
      path = "/test/app", port = 3001, connections = 0)),
    restart_app = function(name) { restarts <<- restarts + 1; list(success = TRUE, message = "Restart scheduled") })
  tm <- create_template_manager()
  # Record POSTs as the server answered them; browser events can miss subframe requests
  posts <- character()
  management <- serve(function(req) {
    response <- handle_management_request(req, config, pm, tm)
    if (req$REQUEST_METHOD == "POST") posts <<- c(posts, paste(req$PATH_INFO, response$status))
    response
  })
  on.exit(httpuv::stopServer(management$server), add = TRUE)
  foreign <- serve(function(req) {
    if (req$PATH_INFO == "/state") return(create_json_response(list(restarts = restarts,
      reload = isTRUE(config$reload_requested), shutdown = isTRUE(config$shutdown_requested), posts = I(posts))))
    create_html_response("<!doctype html><title>Foreign origin</title>")
  })
  on.exit(httpuv::stopServer(foreign$server), add = TRUE)
  writeLines(jsonlite::toJSON(list(management = management$url, foreign = foreign$url), auto_unbox = TRUE), file.path(folder, "urls.json"))
  deadline <- Sys.time() + 60
  while (!file.exists(file.path(folder, "stop")) && Sys.time() < deadline) later::run_now(.01)
}
main()
