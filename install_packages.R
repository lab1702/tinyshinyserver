# Install required packages
packages <- c("shiny", "callr", "jsonlite", "later", "plumber", "httr", "digest", "httpuv", "websocket", "rmarkdown", "flexdashboard", "DT", "plotly", "dplyr")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "https://cran.rstudio.com/")
  }
}

cat("All packages installed successfully!\n")
