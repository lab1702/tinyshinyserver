# Template Manager Module
# Handles HTML template loading and rendering

DEFAULT_SERVER_TITLE <- "Tiny Shiny Server"

# Template Manager Class
TemplateManager <- setRefClass("TemplateManager",
  fields = list(
    template_dir = "character",
    base_url = "character",
    server_title = "character"
  ),
  methods = list(
    initialize = function(template_directory = "templates", base_url_path = "",
                          title = DEFAULT_SERVER_TITLE) {
      template_dir <<- template_directory
      base_url <<- base_url_path
      server_title <<- title %||% DEFAULT_SERVER_TITLE
    },
    title_html = function() {
      "The server title, escaped for HTML and immune to later template substitution"
      gsub("{", "&#123;", html_escape(server_title), fixed = TRUE)
    },
    load_template = function(template_name) {
      "Load a template file and return its content"

      template_path <- file.path(template_dir, paste0(template_name, ".html"))

      if (!file.exists(template_path)) {
        stop("Template not found: ", template_path)
      }

      tryCatch(
        {
          content <- readLines(template_path, encoding = "UTF-8", warn = FALSE)
          return(paste(content, collapse = "\n"))
        },
        error = function(e) {
          stop("Error reading template: ", e$message)
        }
      )
    },
    render_template = function(template_name, variables = list()) {
      "Render a template with variable substitution"

      template_content <- load_template(template_name)

      # Add base_url to variables
      variables$base_url <- base_url

      # Perform variable substitution
      for (var_name in names(variables)) {
        placeholder <- paste0("{{", var_name, "}}")
        replacement <- as.character(variables[[var_name]])
        template_content <- gsub(placeholder, replacement, template_content, fixed = TRUE)
      }

      return(template_content)
    },
    generate_landing_page = function(config) {
      "Generate the landing page HTML"

      # Generate app cards
      app_cards <- generate_app_cards(config$config$apps)

      # Render template
      return(render_template("landing_page", list(
        app_cards = app_cards,
        title = title_html()
      )))
    },
    generate_management_page = function() {
      "Generate the management page HTML"

      # Capture session information: R version, platform, and operating system
      session_info_output <- capture.output(sessionInfo())
      session_info_text <- paste(head(session_info_output, 3), collapse = "\n")
      # Escape HTML characters
      session_info_text <- html_escape(session_info_text)

      return(render_template("management_page", list(
        session_info = session_info_text,
        title = title_html()
      )))
    },
    generate_app_cards = function(apps) {
      "Generate HTML for app cards"

      if (length(apps) == 0) {
        return("<p>No applications configured</p>")
      }

      # Sort apps alphabetically by name
      sorted_apps <- apps[order(sapply(apps, function(app) app$name))]

      cards_html <- ""
      for (app_config in sorted_apps) {
        # HTML escape app name for security
        safe_name <- html_escape(app_config$name)

        initial <- toupper(substr(safe_name, 1, 1))

        card_html <- sprintf('
    <a class="app-card-link is-disabled" aria-disabled="true" title="Loading...">
      <div class="app-card app-disabled" data-app="%1$s">
        <div class="app-card-head">
          <span class="app-avatar" aria-hidden="true">%2$s</span>
          <h3>%1$s</h3>
        </div>
        <div class="app-status">
          <span class="status-badge" id="status-%1$s">Loading...</span>
          <span class="connections-count" id="connections-%1$s">0 connections</span>
        </div>
        <span class="app-open" aria-hidden="true">Loading...</span>
      </div>
    </a>', safe_name, initial)

        cards_html <- paste0(cards_html, card_html)
      }

      return(cards_html)
    },
    create_error_page = function(error_code, error_message, details = NULL) {
      "Create a standardized error page"

      error_html <- sprintf(
        '
<!DOCTYPE html>
<html>
<head>
  <title>Error %1$d</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="color-scheme" content="light dark">
  <script src="%2$s/templates/scripts/theme.js"></script>
  <link rel="stylesheet" href="%2$s/templates/styles/main.css">
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>Error %1$d</h1>
      <p>%3$s</p>
    </div>
    %4$s
  </div>
</body>
</html>',
        error_code, base_url, html_escape(error_message),
        if (!is.null(details)) paste0('<div class="info"><p>', html_escape(details), "</p></div>") else ""
      )

      return(error_html)
    },
    create_success_page = function(title, message, details = NULL) {
      "Create a standardized success page"

      success_html <- sprintf(
        '
<!DOCTYPE html>
<html>
<head>
  <title>%1$s</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <meta name="color-scheme" content="light dark">
  <script src="%2$s/templates/scripts/theme.js"></script>
  <link rel="stylesheet" href="%2$s/templates/styles/main.css">
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>%1$s</h1>
      <p>%3$s</p>
    </div>
    %4$s
  </div>
</body>
</html>',
        html_escape(title), base_url, html_escape(message),
        if (!is.null(details)) paste0('<div class="info"><p>', html_escape(details), "</p></div>") else ""
      )

      return(success_html)
    },
    serve_static_file = function(file_path) {
      "Serve a static file (CSS, JS, images)"

      # Security check - explicitly reject directory traversal attempts
      # Check for ".." as a path component before normalization
      path_parts <- strsplit(file_path, "[/\\\\]")[[1]]
      if (".." %in% path_parts) {
        return(list(
          status = 403,
          headers = list("Content-Type" = "text/plain"),
          body = "Access denied"
        ))
      }

      # Normalize the template directory first (this exists, so normalizePath works reliably)
      template_base <- normalizePath(template_dir, mustWork = FALSE)

      # Construct full path using the normalized base for consistent path handling
      full_path <- file.path(template_base, file_path)

      # Check if file exists first - for non-existent files, the ".." check above
      # is sufficient. normalizePath with mustWork=FALSE behaves inconsistently
      # on Windows for non-existent files, so we check existence first.
      if (!file.exists(full_path)) {
        return(list(
          status = 404,
          headers = list("Content-Type" = "text/plain"),
          body = "File not found"
        ))
      }

      # For existing files, normalize and verify the path stays within template directory
      # Using mustWork=TRUE since we've confirmed the file exists
      normalized_path <- normalizePath(full_path, mustWork = TRUE)

      # On Windows, use case-insensitive comparison and normalize slashes
      if (.Platform$OS.type == "windows") {
        norm_path <- tolower(gsub("/", "\\\\", normalized_path))
        norm_base <- tolower(gsub("/", "\\\\", template_base))
        path_check <- !startsWith(norm_path, norm_base)
      } else {
        path_check <- !startsWith(normalized_path, template_base)
      }

      if (path_check) {
        return(list(
          status = 403,
          headers = list("Content-Type" = "text/plain"),
          body = "Access denied"
        ))
      }

      # Determine content type
      file_ext <- tools::file_ext(normalized_path)
      content_type <- switch(tolower(file_ext),
        "css" = "text/css",
        "js" = "application/javascript",
        "html" = "text/html",
        "png" = "image/png",
        "jpg" = "image/jpeg",
        "jpeg" = "image/jpeg",
        "gif" = "image/gif",
        "svg" = "image/svg+xml",
        "ico" = "image/x-icon",
        "text/plain"
      )

      # Read file content
      tryCatch(
        {
          if (content_type %in% c("image/png", "image/jpeg", "image/gif", "image/x-icon")) {
            # Binary file
            content <- readBin(normalized_path, "raw", file.info(normalized_path)$size)
          } else {
            # Text file
            content <- paste(readLines(normalized_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
          }

          return(list(
            status = 200,
            headers = list("Content-Type" = content_type),
            body = content
          ))
        },
        error = function(e) {
          return(list(
            status = 500,
            headers = list("Content-Type" = "text/plain"),
            body = paste("Error reading file:", e$message)
          ))
        }
      )
    },
    get_available_templates = function() {
      "Get list of available templates"

      if (!dir.exists(template_dir)) {
        return(character(0))
      }

      template_files <- list.files(template_dir, pattern = "\\.html$", recursive = FALSE)
      template_names <- gsub("\\.html$", "", template_files)

      return(template_names)
    }
  )
)

# Create template manager factory function
create_template_manager <- function(template_directory = NULL, base_url = "",
                                    title = DEFAULT_SERVER_TITLE) {
  # If no template_directory specified, use package resources
  if (is.null(template_directory)) {
    template_directory <- system.file("templates", package = "tinyshinyserver")
    if (template_directory == "") {
      # Fallback for development/testing
      template_directory <- "templates"
    }
  }
  return(TemplateManager$new(template_directory, base_url, title))
}
