library(shiny)

function(input, output) {
  output$inventoryTable <- renderTable({
    count <- input$items
    validate(need(
      is.numeric(count) && length(count) == 1L && is.finite(count) &&
        count >= 0 && count <= 10000 && count == floor(count),
      "Enter a whole number of items from 0 to 10,000."
    ))
    data.frame(
      Item = sprintf("Product %d", seq_len(count)),
      Quantity = sample(1:100, count, replace = TRUE),
      Price = round(runif(count, 10, 100), 2)
    )
  })
}
