library(shiny)

function(input, output) {
  output$inventoryTable <- renderTable({
    count <- input$items
    validate(need(
      is.numeric(count) && length(count) == 1L && is.finite(count) &&
        count >= 0 && count == floor(count),
      "Enter a non-negative whole number of items."
    ))
    data.frame(
      Item = sprintf("Product %d", seq_len(count)),
      Quantity = sample(1:100, count, replace = TRUE),
      Price = round(runif(count, 10, 100), 2)
    )
  })
}
