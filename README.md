# tinyshinyserver

<!-- badges: start -->
![R Package](https://img.shields.io/badge/R-package-blue.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Platform](https://img.shields.io/badge/Platform-Windows%20%7C%20Linux%20%7C%20macOS-lightgrey.svg)
<!-- badges: end -->

A lightweight, WebSocket-enabled proxy server for hosting multiple Shiny applications with automatic health monitoring, session management, and resource cleanup.

**🎯 Perfect for:**
- Hosting multiple Shiny apps behind a single server
- Development environments with multiple projects
- Small-scale production deployments
- R Markdown and Quarto dashboard hosting

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

### From Source

```r
# Clone and install locally
git clone https://github.com/lab1702/tinyshinyserver.git
cd tinyshinyserver
Rscript -e "devtools::install('.')"
```

### Prerequisites

- **R** (≥ 4.0)
- **Pandoc** (for R Markdown apps)
- **Quarto CLI** (optional, for Quarto dashboards)

The package automatically installs required R dependencies:

**Core:** shiny, callr, jsonlite, later, curl, promises, digest, httpuv, websocket, openssl
**Async:** future
**Docs:** rmarkdown, quarto
**Utils:** logger

**Optional (for examples):** DT, plotly, dplyr, flexdashboard

## 🚀 Quick Start

### 1. Load

```r
# Load the package
library(tinyshinyserver)
```

### 2. Get Example Apps

```r
# Copy included example apps to your directory
examples_path <- system.file("examples", package = "tinyshinyserver")
file.copy(examples_path, ".", recursive = TRUE)
```

### 3. Start the Server

```r
# Start with the example configuration
start_tss(config = "examples/config.json")
```

### 4. Access Your Server

🌐 **Main Interface:** http://localhost:3838  
⚙️ **Management Dashboard:** http://localhost:3839  
📱 **Individual Apps:** http://localhost:3838/proxy/{app_name}/

### 5. Get Help

```r
# Package overview and getting started
?tinyshinyserver

# Main function help
?start_tss

# Configuration format reference
?config-format
```

## Features

- **Multi-App Hosting**: Host multiple Shiny applications on different ports behind a single proxy
- **Resident & On-Demand Apps**: Choose between always-running apps for immediate access or on-demand apps that start when accessed and stop immediately when unused
- **WebSocket Support**: Full WebSocket proxy with session affinity for real-time Shiny apps
- **Real-time Status Updates**: Landing page shows live app status and connection counts with auto-refresh
- **Management Interface**: Professional web-based dashboard for monitoring and controlling all applications
- **Real-time Monitoring**: Live connection tracking with IP addresses and user agents
- **Individual App Control**: Restart specific applications without affecting others
- **Health Monitoring**: Automatic health checks with app restart on failure
- **Memory Management**: Built-in connection cleanup and resource management
- **Startup Reliability**: Intelligent port availability checking with retry logic for slow-starting applications
- **Graceful Shutdown**: Multiple shutdown options including web-based controls
- **Dark Mode Support**: Automatic theme detection for better user experience
- **R Markdown Support**: Native support for interactive R Markdown documents with `runtime: shiny`
- **Quarto Support**: Full support for interactive Quarto dashboards with `server: shiny`
- **Binary Asset Support**: Handles images, fonts, and other binary files correctly
- **Comprehensive Logging**: Structured logging with per-app log files

## 📋 Configuration

The server uses a JSON configuration file. Here's a minimal example:

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

📖 **See `?config-format` for complete configuration reference**

### 🛑 Stopping the Server

**Recommended**: Use the management interface at http://localhost:3839 and click "Shutdown Server".

**Alternative methods:**
- Press `Ctrl-C` in the R console
- API call: `curl -X POST -H "X-TinyShinyServer-Request: management" http://localhost:3839/api/shutdown`

✅ **Graceful shutdown** closes all connections and cleans up resources.

## 📦 Included Examples

The package includes four example applications:

| App | Type | Description |
|-----|------|-------------|
| **sales** | Single-file Shiny | Simple dashboard with sample data |
| **inventory** | Multi-file Shiny | Interactive tables (ui.R + server.R) |
| **reports** | R Markdown | Flexdashboard with `runtime: shiny` |
| **dashboard** | Quarto | Modern dashboard with `server: shiny` |

Example configuration (from `examples/config.json`):

```json
{
  "apps": [
    {
      "name": "sales",
      "path": "./examples/sales",
      "resident": true
    },
    {
      "name": "inventory", 
      "path": "./examples/inventory"
    },
    {
      "name": "reports",
      "path": "./examples/reports",
      "resident": false
    },
    {
      "name": "dashboard",
      "path": "./examples/dashboard",
      "resident": true
    }
  ],
  "starting_port": 3001,
  "proxy_port": 3838,
  "management_port": 3839,
  "log_dir": "./logs"
}
```

### ⚡ Auto Port Assignment

Apps are automatically assigned ports starting from `starting_port`:
- `sales` → port 3001  
- `inventory` → port 3002  
- `reports` → port 3003  
- `dashboard` → port 3004

The system skips reserved ports (`proxy_port`, `management_port`) automatically.

### 🔄 Resident vs On-Demand Apps

The server supports two application life cycle modes controlled by the `resident` configuration option:

#### **Resident Apps** (`"resident": true`)
- **Always Running**: Started when the server starts and keep running continuously
- **Immediate Response**: No startup delay when users access the app
- **Higher Resource Usage**: Consumes memory and CPU even when unused
- **Best For**: Frequently accessed apps, apps with long startup times, production apps requiring immediate availability

#### **On-Demand Apps** (`"resident": false`, default)
- **Start on Access**: Only started when a user first accesses the app (HTTP request or WebSocket connection)
- **Immediate Shutdown**: Stopped when the last WebSocket connection closes, after any in-flight HTTP requests finish
- **Resource Efficient**: Only consumes resources when actively being used
- **Startup Delay**: Users may experience a brief delay on first access while the app starts
- **Best For**: Infrequently used apps, development/testing environments, resource-constrained servers

#### **Status Indicators**
- **Resident apps**: Show as "running" (green) or "stopped" (red) in the management interface
- **On-demand apps**: Show as "dormant" (blue) when stopped normally, "running" (green) when active

**Example Configuration:**
```json
{
  "apps": [
    {
      "name": "critical-dashboard",
      "path": "./apps/dashboard",
      "resident": true  // Always running for immediate access
    },
    {
      "name": "occasional-report",
      "path": "./apps/reports",
      "resident": false  // Starts only when needed
    },
    {
      "name": "dev-prototype",
      "path": "./apps/prototype"
      // "resident" defaults to false
    }
  ]
}
```

### Configuration Options

| Option | Description | Default |
|--------|-------------|---------|
| `apps` | Array of Shiny applications to host | Required |
| `apps[].name` | Application identifier for URLs | Required |
| `apps[].path` | Relative path to app directory | Required |
| `apps[].resident` | Keep app running continuously (true) or start on-demand (false) | false |
| `apps[].appstart_timeout` | Seconds from app startup to wait for readiness before returning HTTP 503; positive, finite number (fractional seconds supported) | 2 |
| `starting_port` | Starting port for auto-assignment | Required |
| `log_dir` | Directory for log files | Required |
| `proxy_port` | Port for the proxy server | 3838 |
| `proxy_host` | Host interface for proxy server (localhost, 127.0.0.1, 0.0.0.0, ::1, ::) | "127.0.0.1" |
| `management_port` | Port for the management interface | 3839 |
| `restart_delay` | Seconds to wait before restarting failed apps | 5 |
| `health_check_interval` | Seconds between health checks | 10 |

### Network Configuration

The `proxy_host` option controls which network interface the proxy server binds to:

- `"localhost"` or `"127.0.0.1"` - Binds to localhost only (default, most secure)
- `"0.0.0.0"` - Binds to all network interfaces (allows external access)
- `"::1"` - IPv6 localhost
- `"::"` - All IPv6 interfaces

⚠️ **Security Note**: Using `"0.0.0.0"` makes the server accessible from external networks. Only use this if you understand the security implications and have proper firewall rules in place.

### SSL and Authentication with Caddy

For production deployments requiring SSL/TLS and authentication, [Caddy Server](https://caddyserver.com/) provides a simple solution:

```caddyfile
# Caddyfile
myapp.example.com {
    reverse_proxy localhost:3838
    basicauth {
        username password_hash
    }
}

manage.myapp.example.com {
    reverse_proxy localhost:3839
    basicauth {
        admin admin_password_hash
    }
}
```

This configuration automatically handles SSL certificates via Let's Encrypt and adds HTTP basic authentication.

**Important**: When using Caddy, keep `proxy_host` set to `"localhost"` or `"127.0.0.1"` in your `config.json` to ensure the server only accepts connections from Caddy, not directly from external clients.

## Landing Page

The landing page at **http://localhost:3838** provides an overview of all hosted applications with real-time status information.

### Features

#### **Real-time Status Display**
- **Live Status Badges**: Visual indicators showing if each app is running, dormant, stopped, or crashed
- **Connection Counts**: Real-time display of active connections per application
- **Auto-refresh**: Status updates automatically every 5 seconds
- **Color-coded Indicators**: Green for running, blue for dormant, red for stopped, yellow for crashed apps
- **Connection Status**: Shows server connectivity with automatic timeout detection

#### **Application Access**
- **Smart Clickable Tiles**: App accessibility based on status
  - **Running apps**: Immediately clickable ("Click to open [app name]")
  - **Dormant apps**: Clickable for on-demand starting ("Click to start and open [app name]")
  - **Stopped/Crashed apps**: Disabled with helpful tooltips explaining next steps
  - **Server down**: All tiles disabled when server is unreachable
- **Visual Feedback**: Disabled apps are dimmed with "not-allowed" cursor
- **Clean Interface**: Modern, responsive design that works on all devices
- **Dark Mode Support**: Automatically adapts to your system's theme preference

#### **Server Information**
- **System Details**: Display of R version, platform, and server configuration
- **Session Info**: Technical details about the running R environment

The landing page uses the same status API as the management interface, ensuring consistency between all monitoring views.

## Management Interface

The management interface provides a professional web-based dashboard for monitoring and controlling your Shiny server.

### Accessing the Management Interface

Visit **http://localhost:3839** to access the management dashboard.

🔒 **Security**: The management interface is restricted to localhost only for security.

### Features

#### **System Overview**
- Total applications count
- Running applications count  
- Active connections count
- Real-time auto-refresh (every 5 seconds)

#### **Application Management**
- **Enhanced Status Monitoring**: See if each app is running, dormant, stopped, or crashed
- **App Type Display**: Shows whether apps are "Resident" (always-on) or "On-Demand"
- **Connection Counts**: View active connections per application
- **Process Information**: See process IDs and ports for each app
- **Smart Restart Controls**: 
  - **Running/Stopped/Crashed apps**: Show restart button
  - **Dormant apps**: No restart button (start automatically on access)
  - **Helpful messages**: "This app will start automatically when accessed"
- **Resource Details**: Monitor app paths and configuration

#### **Connection Tracking**
- **Real-time Connections**: See all active WebSocket connections
- **Client Information**: IP addresses and user agents
- **Session Details**: Connection timestamps, duration, and last activity
- **Browser Detection**: Identify client browsers and operating systems

#### **Server Controls**
- **Graceful Shutdown**: Shutdown the entire server with confirmation dialog
- **Safety Features**: Multiple confirmation prompts prevent accidental shutdowns
- **Clean Termination**: Properly closes all connections and terminates all processes

### Management API

The management interface exposes a REST API for programmatic access. POST requests require the `X-TinyShinyServer-Request: management` header to prevent cross-origin browser requests. The dashboard sends this header automatically. Cross-origin CORS access must remain disabled on any reverse proxy:

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

### Dark Mode Support

The management interface automatically adapts to your system's theme preference:
- **Light Mode**: Clean, professional appearance for daytime use
- **Dark Mode**: Comfortable viewing for low-light environments
- **Automatic Detection**: Uses CSS `prefers-color-scheme` media query

## Application Structure

### Standard Shiny App
```
apps/myapp/
├── app.R          # Single-file Shiny app
└── [other files]
```

### Multi-file Shiny App
```
apps/myapp/
├── ui.R           # UI definition
├── server.R       # Server logic
└── [other files]
```

### R Markdown App
```
apps/myapp/
├── report.Rmd     # R Markdown with runtime: shiny
└── [other files]
```

### Quarto App
```
apps/myapp/
├── dashboard.qmd  # Quarto document with server: shiny
└── [other files]
```

## Example Applications

The project includes four example applications demonstrating different application types:

### 1. Sales Dashboard (`examples/sales/`)
- Single-file Shiny app demonstrating basic dashboard
- Simple plot with sample sales data

### 2. Inventory Management (`examples/inventory/`)
- Multi-file Shiny app (ui.R + server.R)
- Interactive table with dynamic data generation

### 3. Business Reports (`examples/reports/`)
- R Markdown flexdashboard with `runtime: shiny`
- Interactive charts, KPIs, and data tables
- Demonstrates plotly integration and responsive design

### 4. Interactive Dashboard (`examples/dashboard/`)
- Quarto dashboard with `server: shiny`
- Professional dashboard layout with sidebar controls
- Real-time filtering, interactive plots, and data tables
- Demonstrates modern dashboard design with `format: dashboard`

## Memory Management

The server includes automatic memory management features:

- **Connection Cleanup**: Removes stale connections after 30 minutes of inactivity
- **Queue Limits**: Limits pending message queues to 100 messages per connection
- **Process Cleanup**: Removes dead process objects from memory
- **File Handle Management**: Ensures proper cleanup of log file handles

Cleanup runs automatically every 5 minutes and logs activity for monitoring.

## Logging

### Log Files

- `logs/server.log` - Main server logs
- `logs/{app_name}_output.log` - Per-app stdout logs
- `logs/{app_name}_error.log` - Per-app stderr logs

### Log Levels

- `INFO` - Normal operations
- `WARN` - Warning conditions (e.g., queue limits reached)
- `ERROR` - Error conditions requiring attention

## Development

### Running Individual Apps

For development, you can run apps directly:

```bash
# Standard Shiny app
R -e "shiny::runApp('apps/sales', port = 3001)"

# R Markdown app
R -e "rmarkdown::run('apps/reports/report.Rmd', shiny_args = list(port = 3003, host = '127.0.0.1'))"

# Quarto app
R -e "quarto::quarto_serve('apps/dashboard/dashboard.qmd', port = 3004, host = '127.0.0.1')"
```

### Adding New Applications

1. Create your app in `apps/{app_name}/`
2. Add configuration entry to `config.json` (only name and path required)
3. Restart the server

The server will automatically assign the next available port to your new application.

### Health Endpoints

**Proxy Server:**
- Health check: `GET /health` - Returns `{"status": "healthy"}`
- Apps status: `GET /api/apps` - Returns detailed application status (used by landing page)

**Management Interface:**
- System status: `GET /api/status` - Returns overall system health
- Apps status: `GET /api/apps` - Returns detailed application status
- Connections: `GET /api/connections` - Returns active connection details

## 📁 Package Structure

```
tinyshinyserver/
├── R/                    # R source code
│   ├── start_tss.R       # Main exported function
│   ├── config.R          # Configuration management
│   ├── handlers.R        # HTTP request routing
│   ├── process_manager.R # Application lifecycle
│   └── ...              # Other core modules
├── inst/
│   ├── examples/         # Example apps & config
│   │   ├── sales/        # Simple Shiny app
│   │   ├── inventory/    # Multi-file Shiny app
│   │   ├── reports/      # R Markdown dashboard
│   │   ├── dashboard/    # Quarto dashboard
│   │   └── config.json   # Example configuration
│   └── templates/        # HTML templates & CSS
├── man/                  # Documentation (.Rd files)
├── DESCRIPTION           # Package metadata
└── NAMESPACE            # Exported functions
```

## Architecture

The server uses a multi-process architecture:

```
                    ┌─────────────────────┐
                    │  Management Server  │
                    │    (Port 3839)      │
                    │                     │
                    │  • System Status    │
                    │  • App Control      │
                    │  • Connection Info  │
                    │  • Graceful Shutdown│
                    └─────────────────────┘
                    
┌─────────────────────┐
│   Proxy Server      │
│   (Port 3838)       │
│                     │
│  ┌─WebSocket────────┤
│  │  Handler         │
│  │                  │
│  │  ┌─HTTP──────────┤
│  │  │  Handler      │
└──┼──┼───────────────┘
   │  │
   │  └── HTTP Requests ──┐
   │                      │
   └── WebSocket ─────────┼───┐
       Messages           │   │
                          ▼   ▼
              ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐
              │ Sales    │ │Inventory │ │ Reports  │ │Dashboard │
              │(Port     │ │(Port     │ │(Port     │ │(Port     │
              │ 3001)    │ │ 3002)    │ │ 3003)    │ │ 3004)    │
              └──────────┘ └──────────┘ └──────────┘ └──────────┘
```

### Key Components

- **Management Server**: Web-based monitoring and control interface
- **HTTP Handler**: Routes requests to appropriate backend apps
- **WebSocket Handler**: Manages real-time connections with session affinity
- **Process Manager**: Monitors and restarts failed applications
- **Memory Manager**: Cleans up stale connections and resources
- **Connection Tracker**: Records client information and session details

## 🔧 Troubleshooting

### Common Issues

<details>
<summary><strong>Apps won't start</strong></summary>

- Check that the app directory exists and contains valid Shiny code
- Verify ports aren't already in use: `netstat -an | findstr :3838`
- Check app-specific error logs in `logs/{app_name}_error.log`
- Use `?start_tss` for configuration help
</details>

<details>
<summary><strong>WebSocket connection failures</strong></summary>

- Ensure backend app is running and healthy
- Check for firewall issues blocking WebSocket connections
- Verify session affinity is working correctly
- Monitor logs for WebSocket connection messages
</details>

<details>
<summary><strong>Management interface not accessible</strong></summary>

- Verify server is running: check R console output
- Access via http://localhost:3839 (not external IP)
- Ensure no firewall is blocking localhost connections
- Check logs for management server startup messages
</details>

### 🔍 Debug Mode

```r
# Check server logs (created after starting)
list.files("logs", pattern = "\\.(log|txt)$")

# Monitor main server log
tail -f logs/server.log  # Linux/macOS
Get-Content logs/server.log -Wait  # PowerShell
```

## 🔒 Security Considerations

⚠️ **Important**: This server is designed for **development and internal use**.

**For production deployment, consider:**
- Adding authentication (see [Caddy reverse proxy example](https://caddyserver.com/))
- SSL/TLS encryption for external access
- Firewall rules and network segmentation
- Regular security updates
- Rate limiting on management endpoints

**Built-in security features:**
- Management interface restricted to localhost only
- Input validation for configuration files
- Process isolation between applications

## 🤝 Contributing

Contributions are welcome! Please see our contribution guidelines:

1. **Fork** the repository
2. **Create** a feature branch: `git checkout -b feature/amazing-feature`
3. **Make** your changes and add tests
4. **Test** thoroughly: `devtools::check()`
5. **Submit** a pull request

### Development Setup

```r
# Clone and setup for development
git clone https://github.com/lab1702/tinyshinyserver.git
cd tinyshinyserver

# Install with dependencies
devtools::install_deps()
devtools::load_all()

# Run checks
devtools::check()
```

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🆘 Support

- 📖 **Documentation**: Use `?tinyshinyserver`, `?start_tss`, `?config-format`
- 🐛 **Bug Reports**: [GitHub Issues](https://github.com/lab1702/tinyshinyserver/issues)

---

<p align="center">
  <strong>Built with ❤️ for the R and Shiny community</strong>
</p>
