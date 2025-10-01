library(shiny)

fluidPage(
  titlePanel("Inventory Management"),
  sidebarLayout(
    sidebarPanel(
      h3("Inventory Controls"),
      numericInput("items", "Number of Items:", value = 100)
    ),
    mainPanel(
      tableOutput("inventoryTable")
    )
  )
)
