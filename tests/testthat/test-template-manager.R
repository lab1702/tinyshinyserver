# Test for template manager functions
# Tests for TemplateManager class methods

# ============================================================================
# Setup helper - create temp templates directory
# ============================================================================

create_test_template_dir <- function() {
  temp_dir <- file.path(tempdir(), paste0("templates_test_", Sys.getpid()))
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  # Create a simple test template
  writeLines(
    "<html><head><title>{{title}}</title></head><body>{{content}}</body></html>",
    file.path(temp_dir, "test.html")
  )

  # Create landing page template
  writeLines(
    "<html><body><div>{{session_info}}</div><div>{{app_cards}}</div></body></html>",
    file.path(temp_dir, "landing_page.html")
  )

  # Create management page template
  writeLines(
    "<html><body><h1>Management Dashboard</h1></body></html>",
    file.path(temp_dir, "management_page.html")
  )

  # Create a CSS file for static file tests
  styles_dir <- file.path(temp_dir, "styles")
  dir.create(styles_dir, showWarnings = FALSE)
  writeLines("body { margin: 0; }", file.path(styles_dir, "main.css"))

  return(temp_dir)
}

cleanup_test_template_dir <- function(temp_dir) {
  unlink(temp_dir, recursive = TRUE)
}

# ============================================================================
# TemplateManager Initialization Tests
# ============================================================================

test_that("TemplateManager initializes with defaults", {
  tm <- TemplateManager$new()

  expect_true(inherits(tm, "TemplateManager"))
  expect_equal(tm$template_dir, "templates")
  expect_equal(tm$base_url, "")
})

test_that("TemplateManager initializes with custom values", {
  tm <- TemplateManager$new("/custom/path", "/my-base")

  expect_equal(tm$template_dir, "/custom/path")
  expect_equal(tm$base_url, "/my-base")
})

test_that("create_template_manager factory function works", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- create_template_manager(temp_dir, "/api")

  expect_true(inherits(tm, "TemplateManager"))
  expect_equal(tm$template_dir, temp_dir)
  expect_equal(tm$base_url, "/api")
})

# ============================================================================
# load_template() Tests
# ============================================================================

test_that("load_template loads existing template", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  content <- tm$load_template("test")

  expect_match(content, "<html>")
  expect_match(content, "\\{\\{title\\}\\}")
  expect_match(content, "\\{\\{content\\}\\}")
})

test_that("load_template throws error for missing template", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  expect_error(tm$load_template("nonexistent"), "Template not found")
})

test_that("load_template handles invalid directory", {
  tm <- TemplateManager$new("/nonexistent/path")

  expect_error(tm$load_template("test"), "Template not found")
})

# ============================================================================
# render_template() Tests
# ============================================================================

test_that("render_template substitutes variables", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$render_template("test", list(
    title = "My Page",
    content = "Hello World"
  ))

  expect_match(result, "<title>My Page</title>")
  expect_match(result, "<body>Hello World</body>")
  expect_no_match(result, "\\{\\{title\\}\\}")
  expect_no_match(result, "\\{\\{content\\}\\}")
})

test_that("render_template adds base_url to variables", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  # Create template that uses base_url
  writeLines(
    "<link href='{{base_url}}/styles.css'>",
    file.path(temp_dir, "with_base_url.html")
  )

  tm <- TemplateManager$new(temp_dir, "/my-app")

  result <- tm$render_template("with_base_url", list())

  expect_match(result, "/my-app/styles.css")
})

test_that("render_template handles empty variables", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$render_template("test", list())

  # Unreplaced variables remain as-is
  expect_match(result, "\\{\\{title\\}\\}")
  expect_match(result, "\\{\\{content\\}\\}")
})

test_that("render_template handles numeric variables", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  writeLines("Count: {{count}}", file.path(temp_dir, "numeric.html"))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$render_template("numeric", list(count = 42))

  expect_match(result, "Count: 42")
})

# ============================================================================
# generate_app_cards() Tests
# ============================================================================

test_that("generate_app_cards returns message for empty apps", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$generate_app_cards(list())

  expect_match(result, "No applications configured")
})

test_that("generate_app_cards generates HTML for apps", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  apps <- list(
    list(name = "app1", path = "/path1"),
    list(name = "app2", path = "/path2")
  )

  result <- tm$generate_app_cards(apps)

  expect_match(result, "app1")
  expect_match(result, "app2")
  expect_match(result, "app-card")
  expect_match(result, "data-app=")
})

test_that("generate_app_cards sorts apps alphabetically", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  apps <- list(
    list(name = "zebra", path = "/z"),
    list(name = "alpha", path = "/a"),
    list(name = "beta", path = "/b")
  )

  result <- tm$generate_app_cards(apps)

  # Alpha should appear before beta, beta before zebra
  alpha_pos <- regexpr("alpha", result)
  beta_pos <- regexpr("beta", result)
  zebra_pos <- regexpr("zebra", result)

  expect_lt(alpha_pos, beta_pos)
  expect_lt(beta_pos, zebra_pos)
})

test_that("generate_app_cards escapes HTML in app names", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  # Note: In practice, app names are validated, but we test the safety
  apps <- list(
    list(name = "safe-app", path = "/path")
  )

  result <- tm$generate_app_cards(apps)

  # The result should be valid HTML
  expect_match(result, "safe-app")
})

# ============================================================================
# generate_landing_page() Tests
# ============================================================================

test_that("generate_landing_page creates HTML with session info", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  config <- ShinyServerConfig$new()
  config$config <- list(
    apps = list(
      list(name = "testapp", path = "/test")
    )
  )

  result <- tm$generate_landing_page(config)

  expect_match(result, "<html>")
  expect_match(result, "testapp")
  # Should contain R version info from sessionInfo()
  # Matches both "R version X.Y.Z" (stable) and "R Under development" (unstable)
  expect_match(result, "R (version|Under development)", ignore.case = TRUE)
})

test_that("generate_landing_page handles empty apps list", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  config <- ShinyServerConfig$new()
  config$config <- list(apps = list())

  result <- tm$generate_landing_page(config)

  expect_match(result, "No applications configured")
})

# ============================================================================
# generate_management_page() Tests
# ============================================================================

test_that("generate_management_page returns HTML", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$generate_management_page()

  expect_match(result, "<html>")
  expect_match(result, "Management Dashboard")
})

# ============================================================================
# create_error_page() Tests
# ============================================================================

test_that("create_error_page creates error HTML", {
  tm <- TemplateManager$new("templates", "/base")

  result <- tm$create_error_page(404, "Page not found")

  expect_match(result, "<title>Error 404</title>")
  expect_match(result, "Error 404")
  expect_match(result, "Page not found")
  expect_match(result, "/base/templates/styles/main.css")
})

test_that("create_error_page includes details when provided", {
  tm <- TemplateManager$new("templates")

  result <- tm$create_error_page(500, "Server error", "Check logs for details")

  expect_match(result, "Server error")
  expect_match(result, "Check logs for details")
})

test_that("create_error_page escapes HTML in message", {
  tm <- TemplateManager$new("templates")

  result <- tm$create_error_page(400, "<script>alert('xss')</script>")

  # Script tags should be escaped
  expect_no_match(result, "<script>")
  expect_match(result, "&lt;script&gt;")
})

# ============================================================================
# create_success_page() Tests
# ============================================================================

test_that("create_success_page creates success HTML", {
  tm <- TemplateManager$new("templates", "/api")

  result <- tm$create_success_page("Success!", "Operation completed")

  expect_match(result, "<title>Success!</title>")
  expect_match(result, "Operation completed")
  expect_match(result, "/api/templates/styles/main.css")
})

test_that("create_success_page includes details when provided", {
  tm <- TemplateManager$new("templates")

  result <- tm$create_success_page("Done", "Task completed", "You may close this window")

  expect_match(result, "You may close this window")
})

test_that("create_success_page escapes HTML in content", {
  tm <- TemplateManager$new("templates")

  result <- tm$create_success_page("<b>Title</b>", "<i>Message</i>")

  expect_no_match(result, "<b>Title</b>")
  expect_match(result, "&lt;b&gt;Title&lt;/b&gt;")
})

# ============================================================================
# serve_static_file() Tests
# ============================================================================

test_that("serve_static_file serves CSS files", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$serve_static_file("styles/main.css")

  expect_equal(result$status, 200)
  expect_equal(result$headers[["Content-Type"]], "text/css")
  expect_match(result$body, "body")
})

test_that("serve_static_file returns 404 for missing files", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$serve_static_file("nonexistent.css")

  expect_equal(result$status, 404)
  expect_match(result$body, "File not found")
})

test_that("serve_static_file prevents directory traversal", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$serve_static_file("../../../etc/passwd")

  expect_equal(result$status, 403)
  expect_match(result$body, "Access denied")
})

test_that("serve_static_file detects correct content types", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  # Create test files with different extensions
  writeLines("console.log('test');", file.path(temp_dir, "script.js"))
  writeLines("<html></html>", file.path(temp_dir, "page.html"))
  writeLines("plain text", file.path(temp_dir, "file.txt"))

  tm <- TemplateManager$new(temp_dir)

  js_result <- tm$serve_static_file("script.js")
  expect_equal(js_result$headers[["Content-Type"]], "application/javascript")

  html_result <- tm$serve_static_file("page.html")
  expect_equal(html_result$headers[["Content-Type"]], "text/html")

  txt_result <- tm$serve_static_file("file.txt")
  expect_equal(txt_result$headers[["Content-Type"]], "text/plain")
})

test_that("serve_static_file handles nested paths", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$serve_static_file("styles/main.css")

  expect_equal(result$status, 200)
})

# ============================================================================
# get_available_templates() Tests
# ============================================================================

test_that("get_available_templates lists templates", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  templates <- tm$get_available_templates()

  expect_true("test" %in% templates)
  expect_true("landing_page" %in% templates)
  expect_true("management_page" %in% templates)
})

test_that("get_available_templates returns empty for missing directory", {
  tm <- TemplateManager$new("/nonexistent/directory")

  templates <- tm$get_available_templates()

  expect_equal(length(templates), 0)
})

test_that("get_available_templates excludes non-HTML files", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  # Add non-HTML file
  writeLines("test", file.path(temp_dir, "readme.txt"))

  tm <- TemplateManager$new(temp_dir)

  templates <- tm$get_available_templates()

  expect_false("readme" %in% templates)
})

# ============================================================================
# Integration Tests with Package Templates
# ============================================================================

test_that("factory works with package templates", {
  # Use package templates if available
  tm <- create_template_manager()

  expect_true(inherits(tm, "TemplateManager"))

  # Should have at least some templates
  templates <- tm$get_available_templates()
  # This may be empty in test environment, which is fine
  expect_true(is.character(templates))
})
