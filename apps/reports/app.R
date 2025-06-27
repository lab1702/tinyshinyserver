library(rmarkdown)
library(flexdashboard)

# Run the R Markdown document as a Shiny app
rmarkdown::run("report.Rmd", shiny_args = list(port = getOption("shiny.port", 3003), host = "127.0.0.1"))
