library(shiny)

ui <- fluidPage(
  titlePanel("Sales Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Sales App"),
      p("This is a sample sales dashboard.")
    ),
    mainPanel(
      plotOutput("salesPlot")
    )
  )
)

server <- function(input, output) {
  output$salesPlot <- renderPlot({
    plot(1:10, rnorm(10), main = "Sample Sales Data")
  })
}

shinyApp(ui = ui, server = server)
