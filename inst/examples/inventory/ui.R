library(shiny)

fluidPage(
  titlePanel("Inventory Management"),
  sidebarLayout(
    sidebarPanel(
      h3("Inventory Controls"),
      numericInput("items", "Number of Items:", value = 100, min = 0, step = 1)
    ),
    mainPanel(
      tableOutput("inventoryTable")
    )
  )
)
