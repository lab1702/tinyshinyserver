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
