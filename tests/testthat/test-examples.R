# Tests for the bundled example apps

# ============================================================================
# Dashboard example
# ============================================================================

test_that("the dashboard example does not depend on the day it is rendered", {
  # Its page is rendered once and served again at later starts, while the
  # server chunk runs at every start. Dates taken from the clock would make the
  # rendered date range drift away from the data, until no data is shown.
  qmd <- readLines(system.file("examples", "dashboard", "dashboard.qmd", package = "tinyshinyserver"))
  expect_false(any(grepl("Sys.Date()", qmd, fixed = TRUE)))
  expect_false(any(grepl("Sys.time()", qmd, fixed = TRUE)))
})

# ============================================================================
# Inventory example
# ============================================================================

test_that("inventory example rejects item counts above its limit", {
  server <- source(system.file("examples", "inventory", "server.R", package = "tinyshinyserver"),
    local = TRUE)$value
  shiny::testServer(function(input, output, session) server(input, output), {
    session$setInputs(items = 10000)
    expect_match(output$inventoryTable, "Product 10000", fixed = TRUE)
    session$setInputs(items = 10001)
    expect_error(output$inventoryTable, "from 0 to 10,000", class = "validation")
  })
})

# ============================================================================
# Example configuration
# ============================================================================

test_that("the example configuration is valid and names the bundled apps", {
  # The README and every help page start the server with this file, and no
  # example runs it during a check
  # Its app paths are read from the directory that holds "examples", as the
  # help pages copy it
  examples <- system.file("examples", package = "tinyshinyserver")
  old_dir <- setwd(dirname(examples))
  on.exit(setwd(old_dir), add = TRUE)
  config <- ShinyServerConfig$new()
  settings <- config$read_config(file.path(examples, "config.json"))
  expect_length(config$ignored_fields, 0)
  types <- vapply(settings$apps, function(app) {
    detect_app_type(normalizePath(app$path))$type
  }, character(1))
  expect_setequal(types, c("shiny", "rmd", "qmd"))
  expect_true(all(vapply(settings$apps, function(app) is.null(app_start_problem(app)), logical(1))))
})
