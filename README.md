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

- **R** (≥ 4.1.0)
- **Pandoc** (only for R Markdown apps)
- **Quarto CLI** (only for Quarto apps)

Required R packages are installed with tinyshinyserver. The example R Markdown report and Quarto dashboard also need the packages installed in the quick start below. Pandoc and the Quarto CLI are not R packages and must be installed separately.

## Quick start

Run these commands in R from the directory where you want to keep the examples:

```r
install.packages(c("DT", "plotly", "dplyr", "flexdashboard"))
library(tinyshinyserver)

examples_path <- system.file("examples", package = "tinyshinyserver")
file.copy(examples_path, ".", recursive = TRUE)
start_tss(config = "examples/config.json")
```

The example configuration includes a resident Quarto dashboard, which starts with the server and needs the Quarto CLI. Install the Quarto CLI before using the full configuration, or use the [sales-only configuration](#configuration) below to try a standard Shiny app.

With the default ports, open:

- Landing page (`http://localhost:3838`): app links and status
- Management dashboard (`http://localhost:3839`): monitoring, restarts, configuration reloads, and shutdown
- Sales app (`http://localhost:3838/proxy/sales/`): an individual app

`start_tss()` occupies the R console until shutdown. Click **Shutdown Server** in the management dashboard or press **Ctrl-C** in R to close connections and stop the app processes. For scripted shutdown, see [Management API](#management-api).

## Features

- Host multiple apps behind one HTTP and WebSocket proxy, with a separate R process for each app.
- Choose resident or on-demand apps to balance startup delay and resource use.
- Monitor app status and WebSocket connections, restart individual apps, reload the configuration, and shut down from a web dashboard.
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
| **dashboard** | Quarto | Dashboard with `server: shiny` |

The bundled [example configuration](inst/examples/config.json) keeps `sales` and `dashboard` resident; `inventory` and `reports` start on demand. In the source repository, the apps are under `inst/examples/`; the quick start copies them to `examples/`.

### App ports and lifecycle

Apps receive ports in configuration order, starting from `starting_port`. Allocation skips the proxy port, management port, and ports already in use. With the example settings and no conflicts, the four apps receive ports 3001–3004. Users access apps through `/proxy/{app_name}/` on the proxy port.

| Mode | Starts | Stops | Use when |
|------|--------|-------|----------|
| Resident (`"resident": true`) | At server startup; restarted after a process failure | At server shutdown or restart | Immediate access matters |
| On-demand (`"resident": false`, default) | On the first HTTP request or WebSocket connection | When unused, as described below | Saving resources matters more than startup delay |

On-demand apps stop 30 seconds after their last WebSocket connection closes, provided no new connection opens and no HTTP requests remain; the grace period lets a reloaded or newly opened page reconnect without restarting the app. If requests are in flight, shutdown waits for them to finish before the grace period applies. If a visit never opens a WebSocket session, the app stops after 30 seconds without HTTP activity. A failed on-demand app starts again on the next request.

`appstart_timeout` controls how long a request waits for a starting app to become ready (default: 2 seconds, measured from app startup). If the app is still starting after that, the proxy returns HTTP 503. Increase this value for slow-starting apps; fractional seconds are supported.

The proxy forwards traffic to an app only when the app's own process is listening on the app's port. If another program holds that port, for example a second server instance with the same `starting_port`, the proxy returns HTTP 503 and logs a warning instead of sending the other program your users' requests.

The proxy applies these limits:

- If a running app does not accept connections, the proxy returns HTTP 503. Once the app accepts a connection, a proxied HTTP request fails with HTTP 502 only if the app sends no data for 10 minutes; there is no limit on total transfer time.
- Requests with query strings longer than 8,192 characters are rejected with HTTP 400.
- Request bodies larger than `max_request_size_mb` (default: 100 MB), or sent with chunked transfer encoding, are rejected with HTTP 413 before they are read.
- A WebSocket message from the browser larger than 1 MB closes the session. This check runs only after the whole message has been received, so it does not limit the memory that a single very large message uses; expose the proxy port only to trusted or authenticated clients.
- WebSocket messages from an app to the browser may be up to 2 GB.

### Configuration options

| Option | Description | Default |
|--------|-------------|---------|
| `apps` | Array of apps to host | Required |
| `apps[].name` | Unique URL identifier: 1–50 ASCII letters, digits, underscores, or hyphens | Required |
| `apps[].path` | App directory; relative to the R working directory, or absolute | Required |
| `apps[].resident` | Keep the app running continuously (`true`) or start it on demand (`false`) | `false` |
| `apps[].appstart_timeout` | Seconds, measured from app startup, that a request waits for the app to become ready before HTTP 503; a positive finite number (fractional seconds supported) | 2 |
| `starting_port` | First port to try when assigning app ports | Required |
| `log_dir` | Directory for log files | Required |
| `proxy_port` | Port for the proxy server | 3838 |
| `proxy_host` | Network interface for the proxy server (`localhost`, `127.0.0.1`, `0.0.0.0`, `::1`, or `::`) | `"127.0.0.1"` |
| `management_port` | Port for the management dashboard and API | 3839 |
| `restart_delay` | Seconds to wait before restarting a failed resident app; a non-negative finite number | 5 |
| `health_check_interval` | Seconds between health checks; a positive finite number | 10 |
| `max_request_size_mb` | Largest HTTP request body, in megabytes, that the proxy accepts; larger or chunked bodies get HTTP 413 | 100 |
| `title` | Name shown in the browser tab and top bar of the landing and management pages; up to 100 characters | `"Tiny Shiny Server"` |

## Network access and authentication

`proxy_host` controls the proxy's listening interface. The management server and backend apps always bind to `127.0.0.1`.

| `proxy_host` | Access |
|--------------|--------|
| `"127.0.0.1"` (default) or `"localhost"` | Local machine |
| `"0.0.0.0"` | All IPv4 interfaces |
| `"::1"` | IPv6 localhost |
| `"::"` | All IPv6 interfaces |

The server is intended for development and internal use. It does not provide built-in authentication or TLS. For external access, put an authenticated HTTPS reverse proxy in front of it and restrict direct access with firewall rules.

The proxy checks the `Host` header to block DNS rebinding only when `proxy_host` is a loopback address. With `"0.0.0.0"` or `"::"`, any website visited by someone who can reach the proxy port can point its own domain at this server and then read and control the apps through that person's browser. A firewall that admits a whole network therefore does not keep the apps private from the web. In that setup, allow direct connections to the proxy port only from the reverse proxy.

The `Host` checks protect only the proxy and management ports. Each app process listens on `127.0.0.1` at its own port (assigned in order from `starting_port`) and does not check the `Host` header, so a website visited in a browser on the server machine can still use DNS rebinding to reach a running app directly on that port. Do not browse untrusted websites on the server machine while apps that handle sensitive data are running.

### Caddy example

Keep `proxy_host` set to `"127.0.0.1"` when running Caddy on the same machine. Replace the domains, usernames, and hash placeholders below. Generate each password hash with `caddy hash-password`; Caddy's [`basic_auth` directive](https://caddyserver.com/docs/caddyfile/directives/basic_auth) requires hashed passwords.

```caddyfile
myapp.example.com {
    reverse_proxy 127.0.0.1:3838 {
        header_up Host {upstream_hostport}
    }
    basic_auth {
        username REPLACE_WITH_PASSWORD_HASH
    }
}

manage.myapp.example.com {
    reverse_proxy 127.0.0.1:3839 {
        header_up Host {upstream_hostport}
    }
    basic_auth {
        admin REPLACE_WITH_ADMIN_PASSWORD_HASH
    }
}
```

A reverse proxy must meet these requirements:

- **On the same machine** (`proxy_host` is a loopback address): the proxy rejects requests whose `Host` header is not `localhost`, `127.0.0.1`, or `[::1]`, so the reverse proxy must forward the upstream address as the host, as `header_up Host {upstream_hostport}` does above. It must also set `X-Forwarded-Host` to the public host, which Caddy does by default, because app WebSocket connections are accepted only when the browser's `Origin` matches that host.
- **On another machine** (`proxy_host` is `"0.0.0.0"` or `"::"`): the reverse proxy must preserve the public `Host` header, because `X-Forwarded-Host` is trusted only from the same machine. For nginx, use `proxy_set_header Host $http_host;`, not `$host`, which drops a non-default port and makes the WebSocket `Origin` check fail.
- **Management site**: include it only if remote administration is needed. The management server always rejects requests whose `Host` header is not `localhost`, `127.0.0.1`, or `[::1]`, so its reverse proxy must also forward the upstream address as the host. Keep CORS disabled on its reverse proxy; see [Management API](#management-api) for the required request header.

## Monitoring and management

The landing page and management dashboard refresh their status every 5 seconds. They follow your system's light or dark theme by default. The theme button in the top bar switches between light and dark, and the browser remembers that choice until you switch back to match the system.

| Page | Default URL | Capabilities |
|------|-------------|--------------|
| Landing page | http://localhost:3838 | App links, status, connection counts, and R environment details |
| Management dashboard | http://localhost:3839 | App modes, process IDs, ports, paths, connection details, restarts, configuration reloads, and server shutdown |

App status is **running**, **dormant** (an on-demand app that is not running), **stopped**, or **crashed**. Running and dormant apps can be opened from the landing page; opening a dormant app starts it. Tiles for stopped or crashed apps are disabled, and all tiles are disabled when the server is unreachable.

The management dashboard lists active WebSocket connections with their client IP addresses, user agents, connection times, and last activity. Running, stopped, and crashed apps can be restarted; dormant apps start when accessed. Restarting an app disconnects its users.

**Reload Config & Restart All** reads the configuration file again, stops every app, and disconnects all users. It then assigns app ports from the new configuration and starts the resident apps; on-demand apps start when next opened. The file is checked first, so an invalid file, a `log_dir` where the server log cannot be written, or a change to `proxy_host`, `proxy_port`, or `management_port`, is reported without stopping anything; those three settings take effect only when the server restarts. If an app cannot be stopped, the previous configuration stays in effect and its stopped resident apps start again.

### Proxy endpoints

These read-only endpoints are served on the proxy port:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Returns `{"status": "healthy"}` while the proxy is responding; does not check each app |
| `/api/apps` | GET | Name, status, mode, and connection count for each app, used by the landing page |

### Management API

These endpoints are served on the management port (default: 3839). POST requests require the `X-TinyShinyServer-Request: management` header, which blocks cross-origin browser requests. The dashboard sends this header automatically. CORS must remain disabled on any reverse proxy in front of the management server.

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/api/status` | GET | System overview (apps, connections) |
| `/api/apps` | GET | Detailed application status |
| `/api/connections` | GET | Active connection details |
| `/api/apps/{name}/restart` | POST | Restart the named application |
| `/api/reload` | POST | Reload the configuration file and restart all applications |
| `/api/shutdown` | POST | Graceful server shutdown |

For example:

```bash
# Get system status
curl http://localhost:3839/api/status

# Restart the sales app
curl -X POST -H "X-TinyShinyServer-Request: management" http://localhost:3839/api/apps/sales/restart

# Reload the configuration and restart all apps
curl -X POST -H "X-TinyShinyServer-Request: management" http://localhost:3839/api/reload

# Shut down the server
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

Detection prefers standard Shiny files, then R Markdown, then Quarto. File extensions are case-sensitive. For document apps, the first matching file in alphabetical order is used; keep one entry document per directory to make the choice explicit.

To add an app, create its directory, add an entry with `name` and `path` to the configuration, and click **Reload Config & Restart All** in the management dashboard.

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

The server limits its own memory use:

- **Connection cleanup**: Connections inactive for 30 minutes are closed and removed.
- **Queue limits**: Messages waiting for an app's WebSocket to open are limited to 100 per connection; when the queue is full, the oldest messages are dropped.
- **Process cleanup**: Objects for exited app processes are removed.
- **Log files**: App output is written directly to log files instead of being held in memory. One previous run's logs are kept per app.

Cleanup runs every 5 minutes and is recorded in the server log.

## Logging

### Log files

With `"log_dir": "./logs"`, the server writes:

- `logs/server.log`: main server log
- `logs/{app_name}_output.log`: each app's standard output
- `logs/{app_name}_error.log`: each app's standard error

When an app starts, its logs from the previous run are kept as `{app_name}_output.prev.log` and `{app_name}_error.prev.log`.

### Log levels

The server log records messages at these levels:

- `INFO`: normal operations
- `WARN`: warning conditions, such as a full message queue
- `ERROR`: errors that need attention

## Architecture

The R process running `start_tss()` serves the proxy and management interfaces. Each active app runs in a separate background R process on its assigned localhost port. The proxy forwards HTTP requests and WebSocket messages to that app.

Periodic checks detect exited app processes and restart resident apps. These checks do not assess whether a running app is responsive or producing correct results.

In the source repository, the code is in `R/`, generated help pages are in `man/`, example apps are in `inst/examples/`, and web page templates and styles are in `inst/templates/`.

## Troubleshooting

### Common issues

<details>
<summary><strong>Apps won't start</strong></summary>

- Check that the app directory exists and contains a supported [entry point](#application-structure)
- Check `server.log` for port conflicts and the assigned app ports
- Check the app's error log, `logs/{app_name}_error.log`; after a crash and automatic restart, the crash output is in `logs/{app_name}_error.prev.log`
- See `help("config-format")` for configuration help
</details>

<details>
<summary><strong>WebSocket connection failures</strong></summary>

- Check the app's status on the management dashboard
- Check that no firewall is blocking WebSocket connections
- If you use a reverse proxy, confirm that it forwards WebSocket upgrades
- If you use a reverse proxy on the same machine, confirm that it forwards the upstream address as `Host` and sets `X-Forwarded-Host` to the public host; a reverse proxy on another machine must preserve the public `Host`. Requests for other hosts and cross-origin WebSocket connections are rejected
- Check `server.log` for WebSocket connection messages
</details>

<details>
<summary><strong>Management interface not accessible</strong></summary>

- Check the R console to confirm that the server is running
- Open http://localhost:3839 on the server machine, not through its external IP address; requests whose `Host` header is not `localhost`, `127.0.0.1`, or `[::1]` receive HTTP 403
- Check that no firewall is blocking localhost connections
- Check `server.log` for management server startup messages
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

## Help and support

- **R help**: `?tinyshinyserver`, `?start_tss`, `help("config-format")`, `help("example-config")`
- **Bug reports**: [GitHub Issues](https://github.com/lab1702/tinyshinyserver/issues)
