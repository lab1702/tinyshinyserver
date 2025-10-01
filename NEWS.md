# tinyshinyserver 0.1.0

## Major Changes

* **Package conversion**: Converted the standalone tiny-shiny-server application into a proper R package.
* **New API**: The main entry point is now `start_tss(config = "config.json")` instead of running `main.R` directly.
* **Package resources**: Templates, CSS files, and example applications are now properly packaged and accessible via `system.file()`.
* **Example applications**: All example apps (sales, inventory, reports, dashboard) are included in the package under `inst/examples/`.

## Features

* **Multi-App Hosting**: Host multiple Shiny applications on different ports behind a single proxy
* **Resident & On-Demand Apps**: Choose between always-running apps or on-demand startup
* **WebSocket Support**: Full WebSocket proxy with session affinity for real-time Shiny apps
* **Real-time Management Interface**: Professional web-based dashboard for monitoring and control
* **Health Monitoring**: Automatic health checks with app restart on failure
* **Memory Management**: Built-in connection cleanup and resource management
* **Cross-platform Support**: Works on Windows, Linux, and macOS
* **R Markdown & Quarto Support**: Native support for interactive documents

## Installation

```r
# Install from source
devtools::install(".")

# Load the package
library(tinyshinyserver)

# Copy examples and start server  
examples_path <- system.file("examples", package = "tinyshinyserver")
file.copy(examples_path, ".", recursive = TRUE)
start_tss(config = "examples/config.json")
```

## API

* `start_tss(config = "config.json")` - Start the Tiny Shiny Server with the specified configuration file

## Dependencies

The package automatically installs required dependencies:
- Core: shiny, callr, jsonlite, later, httr, digest, httpuv, websocket
- Async: future, promises
- Visualization: DT, plotly, dplyr  
- Documentation: rmarkdown, quarto, flexdashboard
- Logging: logger