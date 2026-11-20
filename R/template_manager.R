# Template Manager Module
# Handles HTML template loading and rendering

DEFAULT_SERVER_TITLE <- "Tiny Shiny Server"

# How long a browser may reuse a style sheet or script without asking again.
# The files change only when the package is upgraded.
STATIC_FILE_MAX_AGE_SECONDS <- 300

# Whether an If-None-Match request header names the entity tag of a file
etag_matches <- function(if_none_match, etag) {
  if (!is.character(if_none_match) || length(if_none_match) != 1 || is.na(if_none_match)) {
    return(FALSE)
  }
  # A cache between the browser and the server may have marked the tag weak
  candidates <- sub("^W/", "", trimws(strsplit(if_none_match, ",", fixed = TRUE)[[1]]))
  any(candidates %in% c(etag, "*"))
}

# Template Manager Class
TemplateManager <- setRefClass("TemplateManager",
  fields = list(
    template_dir = "character",
    base_url = "character",
    server_title = "character",
    # Templates and the session information are read once: neither changes
    # while the server runs, and every page is built on the thread that also
    # proxies the apps
    template_cache = "environment",
    session_info = "character"
  ),
  methods = list(
    initialize = function(template_directory = "templates", base_url_path = "",
                          title = DEFAULT_SERVER_TITLE) {
      template_dir <<- template_directory
      base_url <<- base_url_path %||% ""
      server_title <<- title %||% DEFAULT_SERVER_TITLE
      template_cache <<- new.env(parent = emptyenv())
      session_info <<- character()
    },
    title_html = function() {
      "The server title, escaped for HTML and immune to later template substitution"
      gsub("{", "&#123;", html_escape(server_title), fixed = TRUE)
    },
    load_template = function(template_name) {
      "Load a template file and return its content"

      template_path <- file.path(template_dir, paste0(template_name, ".html"))
      cached <- template_cache[[template_path]]
      if (!is.null(cached)) {
        return(cached)
      }

      if (!file.exists(template_path)) {
        stop("Template not found: ", template_path)
      }

      content <- tryCatch(
        paste(readLines(template_path, encoding = "UTF-8", warn = FALSE), collapse = "\n"),
        error = function(e) {
          stop("Error reading template: ", e$message)
        }
      )
      assign(template_path, content, envir = template_cache)
      content
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

      # R version, platform, and operating system, escaped for HTML
      if (length(session_info) == 0) {
        session_info <<- html_escape(paste(head(capture.output(sessionInfo()), 3), collapse = "\n"))
      }

      return(render_template("management_page", list(
        session_info = session_info,
        title = title_html()
      )))
    },
    generate_starting_page = function(app_name, retry_seconds = 2) {
      "Generate the page a browser shows, and reloads, until an app is ready"

      return(render_template("starting_page", list(
        app_name = html_escape(app_name),
        retry_seconds = retry_seconds,
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
    serve_static_file = function(file_path, if_none_match = NULL) {
      "Serve a static file (CSS, JS, images), or HTTP 304 for a copy the browser already holds"

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
      # Directories are not served.
      if (!file.exists(full_path) || dir.exists(full_path)) {
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

      # Let browsers keep the file. Without these headers they fetch it again
      # for every page, including each reload of the starting page. A 304
      # has no body element: httpuv would send an empty body compressed.
      info <- file.info(normalized_path)
      cache_headers <- list(
        "Cache-Control" = paste0("max-age=", STATIC_FILE_MAX_AGE_SECONDS),
        "ETag" = sprintf('"%.0f-%.0f"', info$size, as.numeric(info$mtime))
      )
      if (etag_matches(if_none_match, cache_headers$ETag)) {
        return(list(status = 304L, headers = cache_headers, body = NULL))
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
            content <- readBin(normalized_path, "raw", info$size)
          } else {
            # Text file
            content <- paste(readLines(normalized_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
          }

          return(list(
            status = 200,
            headers = c(list("Content-Type" = content_type), cache_headers),
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
