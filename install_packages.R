# Install required packages
packages <- c("shiny", "callr", "jsonlite", "later", "httr", "digest", "httpuv", "websocket", "future", "promises", "logger", "rmarkdown", "flexdashboard", "DT", "plotly", "dplyr")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

cat("All packages installed successfully!\n")
