# Tests for the bundled example apps

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
