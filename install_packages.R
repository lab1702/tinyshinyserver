# Install required packages
packages <- c(
  # Core Shiny and server infrastructure
  "shiny",
  "callr",
  "jsonlite",
  "later",
  "httr",
  "digest",
  "httpuv",
  "websocket",
  "future",
  "promises",
  "logger",

  # Document rendering engines
  "rmarkdown", # For .Rmd files
  "quarto", # For .qmd files
  "flexdashboard", # R Markdown dashboards

  # Dashboard and visualization packages
  "DT", # Interactive data tables
  "plotly", # Interactive plots
  "dplyr" # Data manipulation
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

cat("All packages installed successfully!\n")
