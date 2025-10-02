# Template Manager Module
# Handles HTML template loading and rendering

# Template Manager Class
TemplateManager <- setRefClass("TemplateManager",
  fields = list(
    template_dir = "character",
    base_url = "character"
  ),
  methods = list(
    initialize = function(template_directory = "templates", base_url_path = "") {
      template_dir <<- template_directory
      base_url <<- base_url_path
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

      # Capture session information (first 3 lines only)
      session_info_output <- capture.output(sessionInfo())
      session_info_text <- paste(head(session_info_output, 3), collapse = "\n")
      # Escape HTML characters
      session_info_text <- html_escape(session_info_text)

      # Generate app cards
      app_cards <- generate_app_cards(config$config$apps)

      # Render template
      return(render_template("landing_page", list(
        session_info = session_info_text,
        app_cards = app_cards
      )))
    },
    generate_management_page = function() {
      "Generate the management page HTML"

      return(render_template("management_page", list()))
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

        card_html <- sprintf('
    <a class="app-card-link" style="pointer-events: none; cursor: not-allowed;" title="Loading...">
      <div class="app-card app-disabled" data-app="%1$s">
        <h3>%1$s</h3>
        <div class="app-status">
          <span class="status-badge" id="status-%1$s">Loading...</span>
          <span class="connections-count" id="connections-%1$s">0 connections</span>
        </div>
      </div>
    </a>', safe_name)

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
  <title>Error %d</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="%s/templates/styles/main.css">
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>Error %d</h1>
      <p>%s</p>
    </div>
    %s
  </div>
</body>
</html>',
        error_code, base_url, error_code, html_escape(error_message),
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
  <title>%s</title>
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <link rel="stylesheet" href="%s/templates/styles/main.css">
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>%s</h1>
      <p>%s</p>
    </div>
    %s
  </div>
</body>
</html>',
        html_escape(title), base_url, html_escape(title), html_escape(message),
        if (!is.null(details)) paste0('<div class="info"><p>', html_escape(details), "</p></div>") else ""
      )

      return(success_html)
    },
    serve_static_file = function(file_path) {
      "Serve a static file (CSS, JS, images)"

      # Security check - only allow files within template directory
      normalized_path <- normalizePath(file.path(template_dir, file_path), mustWork = FALSE)
      template_base <- normalizePath(template_dir, mustWork = FALSE)

      if (!startsWith(normalized_path, template_base)) {
        return(list(
          status = 403,
          headers = list("Content-Type" = "text/plain"),
          body = "Access denied"
        ))
      }

      if (!file.exists(normalized_path)) {
        return(list(
          status = 404,
          headers = list("Content-Type" = "text/plain"),
          body = "File not found"
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
    validate_template_variables = function(variables) {
      "Validate template variables for security"

      for (var_name in names(variables)) {
        var_value <- variables[[var_name]]

        # Check for potentially dangerous content
        if (is.character(var_value)) {
          # Basic check for script injection attempts
          if (grepl("<script", var_value, ignore.case = TRUE) ||
            grepl("javascript:", var_value, ignore.case = TRUE) ||
            grepl("on[a-z]+=", var_value, ignore.case = TRUE)) {
            warning("Potentially dangerous content detected in template variable: ", var_name)
            variables[[var_name]] <- html_escape(var_value)
          }
        }
      }

      return(variables)
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
create_template_manager <- function(template_directory = NULL, base_url = "") {
  # If no template_directory specified, use package resources
  if (is.null(template_directory)) {
    template_directory <- system.file("templates", package = "tinyshinyserver")
    if (template_directory == "") {
      # Fallback for development/testing
      template_directory <- "templates"
    }
  }
  return(TemplateManager$new(template_directory, base_url))
}

# Template caching for better performance
template_cache <- new.env()

get_cached_template <- function(template_path, max_age_seconds = 300) {
  "Get template from cache or load if not cached/expired"

  cache_key <- template_path
  current_time <- Sys.time()

  if (exists(cache_key, envir = template_cache)) {
    cached_item <- get(cache_key, envir = template_cache)

    # Check if cache is still valid
    if (difftime(current_time, cached_item$timestamp, units = "secs") < max_age_seconds) {
      return(cached_item$content)
    }
  }

  # Load template and cache it
  if (file.exists(template_path)) {
    content <- paste(readLines(template_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")

    # Store in cache
    assign(cache_key, list(
      content = content,
      timestamp = current_time
    ), envir = template_cache)

    return(content)
  }

  return(NULL)
}

clear_template_cache <- function() {
  "Clear the template cache"
  rm(list = ls(envir = template_cache), envir = template_cache)
}
