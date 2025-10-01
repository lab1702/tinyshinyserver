library(shiny)

function(input, output) {
  output$inventoryTable <- renderTable({
    data.frame(
      Item = paste("Product", 1:input$items),
      Quantity = sample(1:100, input$items, replace = TRUE),
      Price = round(runif(input$items, 10, 100), 2)
    )
  })
}
