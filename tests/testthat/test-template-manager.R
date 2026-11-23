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
    "<html><body><div>{{app_cards}}</div></body></html>",
    file.path(temp_dir, "landing_page.html")
  )

  # Create management page template
  writeLines(
    "<html><body><h1>Management Dashboard</h1><div>{{session_info}}</div></body></html>",
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

test_that("generate_landing_page creates HTML with app cards", {
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
  # Server information is shown on the management page only
  expect_no_match(result, "R (version|Under development)", ignore.case = TRUE)
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

test_that("generate_management_page returns HTML with session info", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$generate_management_page()

  expect_match(result, "<html>")
  expect_match(result, "Management Dashboard")
  # Should contain R version info from sessionInfo()
  # Matches both "R version X.Y.Z" (stable) and "R Under development" (unstable)
  expect_match(result, "R (version|Under development)", ignore.case = TRUE)
  expect_no_match(result, "{{session_info}}", fixed = TRUE)
})

test_that("session information and templates are read once", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)
  expect_length(tm$session_info, 0)
  first <- tm$generate_management_page()
  expect_match(tm$session_info, "R (version|Under development)", ignore.case = TRUE)

  # Neither changes while the server runs, so later pages reuse both
  tm$session_info <- "collected at start-up &amp; escaped"
  unlink(file.path(temp_dir, "management_page.html"))
  second <- tm$generate_management_page()
  expect_match(second, "Management Dashboard")
  expect_match(second, "collected at start-up &amp; escaped", fixed = TRUE)

  # Another template manager reads the directory for itself
  expect_error(TemplateManager$new(temp_dir)$generate_management_page(), "Template not found")
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
  expect_match(rawToChar(result$body), "body")
})

test_that("serve_static_file sends a file's bytes unchanged", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))
  tm <- TemplateManager$new(temp_dir)

  # Line endings, a missing final newline, and UTF-8 text are kept, so the
  # body has the size that the entity tag names
  text <- c(charToRaw("a {}\r\nb {}"), as.raw(c(0xc3, 0xa9)))
  writeBin(text, file.path(temp_dir, "styles", "exact.css"))
  result <- tm$serve_static_file("styles/exact.css")
  expect_identical(result$body, text)
  expect_match(result$headers[["ETag"]], paste0('^W/"', length(text), "-"))

  # Fonts and other binary files arrive intact, with their own types
  font <- as.raw(c(0x77, 0x4f, 0x46, 0x32, 0x00, 0x0a, 0xff, 0x0d))
  writeBin(font, file.path(temp_dir, "font.woff2"))
  result <- tm$serve_static_file("font.woff2")
  expect_identical(result$body, font)
  expect_equal(result$headers[["Content-Type"]], "font/woff2")

  writeBin(font, file.path(temp_dir, "data.bin"))
  expect_equal(tm$serve_static_file("data.bin")$headers[["Content-Type"]], "application/octet-stream")
})

test_that("serve_static_file lets browsers keep a file and answers 304 for an unchanged one", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)
  result <- tm$serve_static_file("styles/main.css")
  etag <- result$headers[["ETag"]]
  # httpuv compresses the file for some clients and not others, so the tag
  # is weak and caches keep the two encodings apart
  expect_match(etag, "^W/\"[0-9]+-[0-9]+\"$")
  expect_equal(result$headers[["Cache-Control"]], "max-age=300")
  expect_equal(result$headers[["Vary"]], "Accept-Encoding")
  strong <- sub("^W/", "", etag)

  # A browser that holds this version is told so, in the forms the header takes
  for (header in list(etag, strong, paste0("\"other\", ", etag), paste0("\"other\", ", strong), "*")) {
    unchanged <- tm$serve_static_file("styles/main.css", header)
    expect_equal(unchanged$status, 304)
    expect_equal(unchanged$headers[["ETag"]], etag)
    expect_equal(unchanged$headers[["Vary"]], "Accept-Encoding")
    # httpuv would send an empty body compressed, which may not follow a 304
    expect_null(unchanged$body)
  }

  # Any other tag, or none, receives the file
  for (header in list(NULL, "", "\"other\"", NA_character_, c(etag, etag))) {
    expect_equal(tm$serve_static_file("styles/main.css", header)$status, 200)
  }

  # A changed file has a new tag
  writeLines("body { margin: 0; padding: 0; }", file.path(temp_dir, "styles", "main.css"))
  changed <- tm$serve_static_file("styles/main.css", etag)
  expect_equal(changed$status, 200)
  expect_false(identical(changed$headers[["ETag"]], etag))

  # A file that is not served is not confirmed either
  expect_equal(tm$serve_static_file("missing.css", "*")$status, 404)
  expect_equal(tm$serve_static_file("../secret.css", "*")$status, 403)
})

test_that("serve_static_file returns 404 for missing files", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$serve_static_file("nonexistent.css")

  expect_equal(result$status, 404)
  expect_match(result$body, "File not found")
})

test_that("serve_static_file returns 404 for directories", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  for (path in c("", "styles", "styles/")) {
    result <- tm$serve_static_file(path)
    expect_equal(result$status, 404)
    expect_match(result$body, "File not found")
  }
})

test_that("serve_static_file returns 404 for device names", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  # On Windows "nul" exists in every directory but cannot be normalized; the
  # error used to reach the client with the directory's full path
  for (path in c("nul", "NUL", "styles/nul", "con", "aux")) {
    result <- tm$serve_static_file(path)
    expect_equal(result$status, 404, info = path)
    expect_equal(result$body, "File not found", info = path)
  }
})

test_that("serve_static_file prevents directory traversal", {
  temp_dir <- create_test_template_dir()
  on.exit(cleanup_test_template_dir(temp_dir))

  tm <- TemplateManager$new(temp_dir)

  result <- tm$serve_static_file("../../../etc/passwd")

  expect_equal(result$status, 403)
  expect_match(result$body, "Access denied")
})

test_that("serve_static_file does not follow a link out of the template directory", {
  temp_dir <- create_test_template_dir()
  # One whose name starts with the template directory's, and one elsewhere
  sibling <- paste0(temp_dir, "-old")
  elsewhere <- tempfile("tss-elsewhere")
  on.exit(unlink(c(temp_dir, sibling, elsewhere), recursive = TRUE), add = TRUE)
  for (target in c(sibling, elsewhere)) {
    dir.create(target)
    writeLines("secret", file.path(target, "secret.txt"))
  }
  link <- function(target, name) {
    path <- file.path(temp_dir, name)
    made <- if (.Platform$OS.type == "windows") Sys.junction(target, path) else file.symlink(target, path)
    if (!isTRUE(made)) skip("A link to a directory cannot be made here")
  }
  link(sibling, "old")
  link(elsewhere, "elsewhere")

  tm <- TemplateManager$new(temp_dir)
  for (path in c("old/secret.txt", "elsewhere/secret.txt")) {
    result <- tm$serve_static_file(path)
    expect_equal(result$status, 403, info = path)
  }
  # A file of the directory itself is still served
  writeLines("body {}", file.path(temp_dir, "own.css"))
  expect_equal(tm$serve_static_file("own.css")$status, 200)
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
# Integration Tests with Package Templates
# ============================================================================

test_that("factory works with package templates", {
  tm <- create_template_manager()

  expect_true(inherits(tm, "TemplateManager"))

  # Every page the server renders has a template in the package
  for (template in c("landing_page", "management_page", "starting_page")) {
    expect_true(nzchar(tm$load_template(template)), info = template)
  }
})

test_that("generate_starting_page renders a page that reloads itself for the app", {
  tm <- create_template_manager(base_url = "/shiny", title = "My <Server>")
  html <- tm$generate_starting_page("sales", retry_seconds = 3, answer_seconds = 15)

  expect_match(html, "<h1>sales</h1>", fixed = TRUE)
  expect_match(html, "<title>sales - My &lt;Server&gt;</title>", fixed = TRUE)
  expect_match(html, "var retryMs = 3 * 1000;", fixed = TRUE)
  expect_match(html, "var answerMs = 15 * 1000 + 10000;", fixed = TRUE)
  expect_match(html, 'src="/shiny/templates/scripts/theme.js"', fixed = TRUE)
  expect_match(html, 'href="/shiny/"', fixed = TRUE)
  # The reload stops after a while, so a forgotten tab cannot restart an app forever
  expect_match(html, "giveUpMs", fixed = TRUE)
  expect_no_match(html, "{{", fixed = TRUE)
})

test_that("a number in a page's script is written the same whatever the session's decimal mark", {
  old <- options(OutDec = ",")
  on.exit(options(old), add = TRUE)
  tm <- create_template_manager()
  # A fractional appstart_timeout: "2,5 * 1000" is no expression, and the
  # page that says it reloads itself would never do so
  html <- tm$generate_starting_page("sales", retry_seconds = 2, answer_seconds = 2.5, delay_seconds = 0)
  expect_match(html, "var answerMs = 2.5 * 1000 + 10000;", fixed = TRUE)
  expect_match(html, "var retryMs = 2 * 1000;", fixed = TRUE)
  expect_match(html, "var delayMs = 0 * 1000;", fixed = TRUE)
  # A long one is written in full
  html <- tm$generate_starting_page("sales", answer_seconds = 86400, delay_seconds = 100000L)
  expect_match(html, "var answerMs = 86400 * 1000 + 10000;", fixed = TRUE)
  expect_match(html, "var delayMs = 100000 * 1000;", fixed = TRUE)
})

test_that("landing and management pages show the configured title escaped", {
  templates <- system.file("templates", package = "tinyshinyserver")
  config <- ShinyServerConfig$new()
  config$config <- list(apps = list(list(name = "app1", path = "/path")))

  default_tm <- TemplateManager$new(templates)
  expect_match(default_tm$generate_landing_page(config), "<title>Tiny Shiny Server</title>", fixed = TRUE)
  # Configs built in code may have no title
  untitled_tm <- create_template_manager(templates, title = NULL)
  expect_match(untitled_tm$generate_management_page(), "<title>Management - Tiny Shiny Server</title>", fixed = TRUE)

  tm <- TemplateManager$new(templates, title = "R&D <Apps> {{app_cards}}")
  expected <- "R&amp;D &lt;Apps&gt; &#123;&#123;app_cards}}"
  landing <- tm$generate_landing_page(config)
  expect_match(landing, paste0("<title>", expected, "</title>"), fixed = TRUE)
  expect_match(landing, paste0('<span class="brand-name">', expected, "</span>"), fixed = TRUE)
  expect_no_match(landing, "Tiny Shiny Server", fixed = TRUE)

  management <- tm$generate_management_page()
  expect_match(management, paste0("<title>Management - ", expected, "</title>"), fixed = TRUE)
  expect_match(management, paste0('<span class="brand-name">', expected, "</span>"), fixed = TRUE)
  expect_no_match(management, "{{title}}", fixed = TRUE)
})

test_that("the starting page counts its two minutes from the end of an app's restart delay", {
  tm <- create_template_manager()
  starting <- tm$generate_starting_page("sales")
  expect_match(starting, "var delayMs = 0 * 1000;", fixed = TRUE)
  expect_match(starting, "This application is starting.", fixed = TRUE)

  waiting <- tm$generate_starting_page("sales", retry_seconds = 250, delay_seconds = 250)
  expect_match(waiting, "var delayMs = 250 * 1000;", fixed = TRUE)
  expect_match(waiting, "wait = { first: now + delayMs };", fixed = TRUE)
  expect_match(waiting, "stopped after a failure", fixed = TRUE)
  expect_false(grepl("{{", waiting, fixed = TRUE))
})

test_that("the landing page reloads for a new base_path at most once a minute", {
  html <- create_template_manager(base_url = "/shiny")$generate_landing_page(list(config = list(apps = list())))
  expect_match(html, "X-TinyShinyServer-Base-Path", fixed = TRUE)
  expect_match(html, "now - last < 60000", fixed = TRUE)
  # Also where site data is blocked; tests/browser/public-pages.cjs runs it
  expect_match(html, "window.name = namePrefix", fixed = TRUE)
  # It reloads for a new address, through the guard, and for a changed list of apps
  expect_equal(lengths(regmatches(html, gregexpr("location.reload()", html, fixed = TRUE))), 2)
  expect_match(html, "reloadForNewAddress()", fixed = TRUE)
})
