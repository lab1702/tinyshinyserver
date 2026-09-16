# tinyshinyserver

<!-- badges: start -->
![R Package](https://img.shields.io/badge/R-package-blue.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Platform](https://img.shields.io/badge/Platform-Windows%20%7C%20Linux%20%7C%20macOS-lightgrey.svg)
<!-- badges: end -->

A lightweight, WebSocket-enabled proxy server for hosting multiple Shiny applications with automatic health monitoring, session management, and resource cleanup.

Use it to host Shiny apps, interactive R Markdown documents, and Quarto dashboards behind one proxy. Apps can stay running or start on demand. For deployments beyond localhost, see [Network access and authentication](#network-access-and-authentication).

## Installation

### From CRAN

```r
# Install from CRAN
install.packages("tinyshinyserver")
```

### From GitHub

```r
# Install from GitHub
remotes::install_github("lab1702/tinyshinyserver")
```

### From source

```bash
# Clone and install locally
git clone https://github.com/lab1702/tinyshinyserver.git
cd tinyshinyserver
Rscript -e "devtools::install('.')"
```

### Prerequisites

- **R** (≥ 4.0)
- **Pandoc** (for R Markdown apps)
- **Quarto CLI** (optional, for Quarto dashboards)

Required R dependencies are installed with the package. The bundled reports and Quarto dashboard also need the example packages installed below; Pandoc and the Quarto CLI must be available separately.

## Quick start

Run these commands in R from the directory where you want to keep the examples:

```r
install.packages(c("DT", "plotly", "dplyr", "flexdashboard"))
library(tinyshinyserver)

examples_path <- system.file("examples", package = "tinyshinyserver")
file.copy(examples_path, ".", recursive = TRUE)
start_tss(config = "examples/config.json")
```

The example configuration includes a Quarto app that starts immediately. Install the Quarto CLI before using the full configuration, or use the sales-only configuration below to try a standard Shiny app.

With the default ports, open:

- Landing page (`http://localhost:3838`): app links and status
- Management dashboard (`http://localhost:3839`): monitoring, restarts, and shutdown
- Sales app (`http://localhost:3838/proxy/sales/`): an individual app

`start_tss()` occupies the R console until shutdown. Click **Shutdown Server** in the management dashboard or press **Ctrl-C** in R to close connections and stop the app processes. For scripted shutdown, see [Management API](#management-api).

## Features

- Host multiple apps behind one HTTP and WebSocket proxy, with a separate R process for each app.
- Choose resident or on-demand apps to balance startup delay and resource use.
- Monitor app status and WebSocket connections, restart individual apps, and shut down from a web dashboard.
- Automatically restart failed resident processes and clean up stale connections.
- Serve standard Shiny apps, interactive R Markdown documents, and Quarto dashboards.
- Capture server logs and each app's output and errors.

## Configuration

The server reads a JSON configuration file. After copying the examples, save this sales-only configuration as `config.json` in your working directory and run `start_tss()`:

```json
{
  "apps": [
    {
      "name": "sales",
      "path": "./examples/sales",
      "resident": true
    }
  ],
  "starting_port": 3001,
  "proxy_port": 3838,
  "proxy_host": "127.0.0.1",
  "management_port": 3839,
  "log_dir": "./logs"
}
```

`apps`, `starting_port`, and `log_dir` are required. Other server settings have defaults. Relative app paths and `log_dir` are resolved from the R working directory (`getwd()`), **not from the configuration file's directory**. Absolute paths are also supported.

JSON does not allow comments. See `help("config-format")` for the complete reference.

### Included examples

The package includes four example applications:

| App | Type | Description |
|-----|------|-------------|
| **sales** | Single-file Shiny | Simple dashboard with sample data |
| **inventory** | Multi-file Shiny | Interactive tables (ui.R + server.R) |
| **reports** | R Markdown | Flexdashboard with `runtime: shiny` |
| **dashboard** | Quarto | Modern dashboard with `server: shiny` |

The bundled [example configuration](inst/examples/config.json) keeps `sales` and `dashboard` resident; `inventory` and `reports` start on demand. In the source repository, the apps are under `inst/examples/`; the quick start copies them to `examples/`.

### App ports and lifecycle

Apps receive ports in configuration order, starting from `starting_port`. Allocation skips the proxy port, management port, and ports already in use. With the example settings and no conflicts, the four apps receive ports 3001–3004. Users access apps through `/proxy/{app_name}/` on the proxy port.

| Mode | Starts | Stops | Use when |
|------|--------|-------|----------|
| Resident (`"resident": true`) | At server startup; restarted after a process failure | At server shutdown or restart | Immediate access matters |
| On-demand (`"resident": false`, default) | On the first HTTP request or WebSocket connection | When unused, as described below | Saving resources matters more than startup delay |

On-demand apps stop when their last WebSocket connection closes, provided no HTTP requests remain. If requests are in flight, shutdown waits for them to finish and allows 30 seconds for a new page to establish its WebSocket session. Visits that never open a WebSocket session allow 30 seconds without HTTP activity before shutdown. Failed on-demand apps start again on the next request.

`appstart_timeout` controls how long a request waits for startup readiness (default: 2 seconds, measured from app startup). If the app is still starting, the proxy returns HTTP 503. Increase this value for slow-starting apps; fractional seconds are supported.

### Configuration options

| Option | Description | Default |
|--------|-------------|---------|
| `apps` | Array of Shiny applications to host | Required |
| `apps[].name` | Unique URL identifier: 1–50 ASCII letters, digits, underscores, or hyphens | Required |
| `apps[].path` | App directory; relative to the R working directory, or absolute | Required |
| `apps[].resident` | Keep app running continuously (true) or start on-demand (false) | false |
| `apps[].appstart_timeout` | Seconds from app startup to wait for readiness before returning HTTP 503; positive, finite number (fractional seconds supported) | 2 |
| `starting_port` | Starting port for auto-assignment | Required |
| `log_dir` | Directory for log files | Required |
| `proxy_port` | Port for the proxy server | 3838 |
| `proxy_host` | Host interface for proxy server (localhost, 127.0.0.1, 0.0.0.0, ::1, ::) | "127.0.0.1" |
| `management_port` | Port for the management interface | 3839 |
| `restart_delay` | Non-negative finite seconds to wait before restarting failed apps | 5 |
| `health_check_interval` | Positive finite seconds between health checks | 10 |

## Network access and authentication

`proxy_host` controls the proxy's listening interface. The management server and backend apps always bind to `127.0.0.1`.

| `proxy_host` | Access |
|--------------|--------|
| `"127.0.0.1"` (default) or `"localhost"` | Local machine |
| `"0.0.0.0"` | All IPv4 interfaces |
| `"::1"` | IPv6 localhost |
| `"::"` | All IPv6 interfaces |

The server is intended for development and internal use. It does not provide built-in authentication or TLS. For external access, put an authenticated HTTPS reverse proxy in front of it and restrict direct access with firewall rules.

### Caddy example

Keep `proxy_host` set to `"127.0.0.1"` when running Caddy on the same machine. Replace the domains, usernames, and hash placeholders below. Generate each password hash with `caddy hash-password`; Caddy's [`basic_auth` directive](https://caddyserver.com/docs/caddyfile/directives/basic_auth) requires hashed passwords.

```caddyfile
myapp.example.com {
    reverse_proxy 127.0.0.1:3838
    basic_auth {
        username REPLACE_WITH_PASSWORD_HASH
    }
}

manage.myapp.example.com {
    reverse_proxy 127.0.0.1:3839
    basic_auth {
        admin REPLACE_WITH_ADMIN_PASSWORD_HASH
    }
}
```

Include the management site only if remote administration is needed. Keep cross-origin CORS access disabled on its reverse proxy; see [Management API](#management-api) for the required request header.

## Monitoring and management

Both web pages refresh status every 5 seconds and follow your system's light or dark theme.

| Page | Default URL | Capabilities |
|------|-------------|--------------|
| Landing page | http://localhost:3838 | App links, status, connection counts, and R environment details |
| Management dashboard | http://localhost:3839 | App modes, process IDs, ports, paths, connection details, restarts, and server shutdown |

App status is **running**, **dormant** (an unused on-demand app), **stopped**, or **crashed**. Running and dormant apps can be opened from the landing page; opening a dormant app starts it. Stopped or crashed tiles are disabled, and all tiles are disabled when the server is unreachable.

The management dashboard shows active WebSocket connections, including client IP addresses, user agents, connection times, and last activity. Restart controls are available for running, stopped, and crashed apps. Dormant apps start on access. Restarting an app disconnects its users.

### Proxy endpoints

These read-only endpoints are served on the proxy port:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Returns `{"status": "healthy"}` while the proxy is responding; does not check each app |
| `/api/apps` | GET | Application status used by the landing page |

### Management API

These endpoints are served on the management port (default: 3839). POST requests require the `X-TinyShinyServer-Request: management` header to prevent cross-origin browser requests. The dashboard sends this header automatically. Cross-origin CORS access must remain disabled on any reverse proxy:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/status` | GET | System overview (apps, connections) |
| `/api/apps` | GET | Detailed application status |
| `/api/connections` | GET | Active connection details |
| `/api/apps/{name}/restart` | POST | Restart specific application |
| `/api/shutdown` | POST | Graceful server shutdown |

**Example usage:**
```bash
# Get system status
curl http://localhost:3839/api/status

# Restart the sales app
curl -X POST -H "X-TinyShinyServer-Request: management" http://localhost:3839/api/apps/sales/restart

# Shutdown server
curl -X POST -H "X-TinyShinyServer-Request: management" http://localhost:3839/api/shutdown
```

## Application structure

Point each app's `path` at a directory containing one of these entry points:

| App type | Files | Requirement |
|----------|-------|-------------|
| Single-file Shiny | `app.R` | Standard Shiny app |
| Multi-file Shiny | `ui.R` and `server.R` | Both files present |
| R Markdown | A `.Rmd` document | `runtime: shiny` and Pandoc |
| Quarto | A `.qmd` document | `server: shiny` and the Quarto CLI |

Detection prefers standard Shiny files, then R Markdown, then Quarto. For document apps, the first matching file returned by `list.files()` is used; keep one entry document per directory to make the choice explicit.

To add an app, create its directory, add an entry with `name` and `path` to the configuration, and restart the server.

For local development, run a copied example directly from R:

```r
shiny::runApp("examples/sales", port = 3001)
rmarkdown::run("examples/reports/report.Rmd",
               shiny_args = list(port = 3003, host = "127.0.0.1"))
quarto::quarto_serve("examples/dashboard/dashboard.qmd",
                     port = 3004, host = "127.0.0.1")
```

Run one command at a time, with tinyshinyserver stopped or a different port selected.

## Memory management

The server includes automatic memory management features:

- **Connection Cleanup**: Removes stale connections after 30 minutes of inactivity
- **Queue Limits**: Limits pending message queues to 100 messages per connection
- **Process Cleanup**: Removes dead process objects from memory
- **File Handle Management**: Ensures proper cleanup of log file handles

Cleanup runs automatically every 5 minutes and logs activity for monitoring.

## Logging

### Log files

With `"log_dir": "./logs"`, the server writes:

- `logs/server.log` - Main server logs
- `logs/{app_name}_output.log` - Per-app stdout logs
- `logs/{app_name}_error.log` - Per-app stderr logs

### Log levels

- `INFO` - Normal operations
- `WARN` - Warning conditions (e.g., queue limits reached)
- `ERROR` - Error conditions requiring attention

## Architecture

The R process running `start_tss()` serves the proxy and management interfaces. Each active app runs in a separate background R process on its assigned localhost port. The proxy forwards HTTP requests and WebSocket messages to that app.

Periodic checks detect exited app processes and restart resident apps. These checks do not assess whether a running app is responsive or producing correct results.

Source code lives in `R/`, generated help pages in `man/`, example apps in `inst/examples/`, and shared styles in `inst/templates/`.

## Troubleshooting

### Common issues

<details>
<summary><strong>Apps won't start</strong></summary>

- Check that the app directory exists and contains valid Shiny code
- Check the startup logs for port conflicts and assigned app ports
- Check app-specific error logs in `logs/{app_name}_error.log`
- Use `?start_tss` for configuration help
</details>

<details>
<summary><strong>WebSocket connection failures</strong></summary>

- Ensure backend app is running and healthy
- Check for firewall issues blocking WebSocket connections
- If using a reverse proxy, confirm it forwards WebSocket upgrades
- Monitor logs for WebSocket connection messages
</details>

<details>
<summary><strong>Management interface not accessible</strong></summary>

- Verify server is running: check R console output
- Access via http://localhost:3839 (not external IP)
- Ensure no firewall is blocking localhost connections
- Check logs for management server startup messages
</details>

### Inspecting logs

In R, list the logs under your configured `log_dir`:

```r
list.files("logs", pattern = "\\.log$")
```

To follow the main log from a terminal:

```bash
# Linux/macOS
tail -f logs/server.log
```

```powershell
# PowerShell
Get-Content logs/server.log -Wait
```

## Contributing

To contribute:

1. **Fork** the repository
2. **Create** a feature branch: `git checkout -b feature/amazing-feature`
3. **Make** your changes and add tests
4. **Test** thoroughly: `devtools::check()`
5. **Submit** a pull request

### Development setup

Clone the repository in a terminal:

```bash
git clone https://github.com/lab1702/tinyshinyserver.git
cd tinyshinyserver
```

Then run in R from the repository root:

```r
devtools::install_deps(dependencies = TRUE)
devtools::load_all()
devtools::check()
```

Edit help-page documentation in the roxygen comments in `R/`, then regenerate `man/` with `devtools::document()`.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.

## Help and support

- **R help**: `?tinyshinyserver`, `?start_tss`, `help("config-format")`, `help("example-config")`
- **Bug reports**: [GitHub Issues](https://github.com/lab1702/tinyshinyserver/issues)
