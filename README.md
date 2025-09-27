# Tiny Shiny Server

A lightweight, WebSocket-enabled proxy server for hosting multiple Shiny applications with automatic health monitoring, session management, and resource cleanup.

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
- **Graceful Shutdown**: Multiple shutdown options including web-based controls
- **Dark Mode Support**: Automatic theme detection for better user experience
- **R Markdown Support**: Native support for interactive R Markdown documents with `runtime: shiny`
- **Quarto Support**: Full support for interactive Quarto dashboards with `server: shiny`
- **Binary Asset Support**: Handles images, fonts, and other binary files correctly
- **Comprehensive Logging**: Structured logging with per-app log files

## Quick Start

### Prerequisites

- R (version 4.0 or higher)
- Required R packages (automatically installed via setup script)
- **Pandoc**: Required for R Markdown apps to work
  - Linux: `sudo apt install pandoc` (Ubuntu/Debian) or equivalent for your distribution
  - Windows: `winget install JohnMacFarlane.Pandoc`
  - macOS: Available via Homebrew or direct download from pandoc.org
- **Quarto CLI** (optional): Required for Quarto document apps
  - Download from https://quarto.org/docs/get-started/
  - Includes its own Pandoc, so can be used instead of separate Pandoc installation
  - Windows: `winget install posit.quarto`

### Installation

1. **Clone or download the project:**
   ```bash
   git clone <repository-url>
   cd tiny-shiny-server
   ```

2. **Install dependencies:**
   ```bash
   Rscript install_packages.R
   ```

3. **Start the server:**

   **Linux/macOS:**
   ```bash
   Rscript main.R
   ```

   **Windows:**
   ```powershell
   .\start-server.ps1
   ```
   
   The PowerShell script automatically locates your R installation and starts the server.

4. **Access applications:**
   - Landing page with live status: http://localhost:3838
   - Management interface: http://localhost:3839
   - Individual apps: http://localhost:3838/proxy/{app_name}/

### Stopping the Server

**Recommended**: Use the management interface at http://localhost:3839 and click the "Shutdown Server" button.

**Alternative methods:**
- Press `Ctrl-C` in the terminal where the server is running
- API call: `curl -X POST http://localhost:3839/api/shutdown`

All methods will gracefully:
- Close all WebSocket connections
- Terminate all spawned Shiny app processes
- Clean up resources and exit

## Configuration

The server is configured via `config.json`:

```json
{
  "apps": [
    {
      "name": "sales",
      "path": "./apps/sales",
      "resident": true
    },
    {
      "name": "inventory", 
      "path": "./apps/inventory"
    },
    {
      "name": "reports",
      "path": "./apps/reports",
      "resident": false
    },
    {
      "name": "dashboard",
      "path": "./apps/dashboard",
      "resident": true
    }
  ],
  "starting_port": 3001,
  "proxy_port": 3838,
  "proxy_host": "localhost",
  "management_port": 3839,
  "restart_delay": 5,
  "health_check_interval": 10,
  "log_dir": "./logs"
}
```

### Auto Port Assignment

The server automatically assigns ports to applications starting from `starting_port`. In the example above:
- `sales` will be assigned port 3001
- `inventory` will be assigned port 3002
- `reports` will be assigned port 3003
- `dashboard` will be assigned port 3004

The auto-assignment algorithm:
- Starts at the `starting_port` value
- Skips any reserved ports (`proxy_port` and `management_port`)
- Assigns sequential ports to each application
- Validates that enough ports are available before starting

### Resident vs On-Demand Apps

The server supports two application lifecycle modes controlled by the `resident` configuration option:

#### **Resident Apps** (`"resident": true`)
- **Always Running**: Started when the server starts and keep running continuously
- **Immediate Response**: No startup delay when users access the app
- **Higher Resource Usage**: Consumes memory and CPU even when unused
- **Best For**: Frequently accessed apps, apps with long startup times, production apps requiring immediate availability

#### **On-Demand Apps** (`"resident": false`, default)
- **Start on Access**: Only started when a user first accesses the app (HTTP request or WebSocket connection)
- **Immediate Shutdown**: Stopped immediately when the last connection closes
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
| `starting_port` | Starting port for auto-assignment | Required |
| `proxy_port` | Port for the proxy server | 3838 |
| `proxy_host` | Host interface for proxy server (localhost, 127.0.0.1, 0.0.0.0, ::1, ::) | "127.0.0.1" |
| `management_port` | Port for the management interface | 3839 |
| `restart_delay` | Seconds to wait before restarting failed apps | 5 |
| `health_check_interval` | Seconds between health checks | 10 |
| `log_dir` | Directory for log files | "./logs" |

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
- **Live Status Badges**: Visual indicators showing if each app is running, stopped, or crashed
- **Connection Counts**: Real-time display of active connections per application
- **Auto-refresh**: Status updates automatically every 5 seconds
- **Color-coded Indicators**: Green for running, red for stopped, yellow for crashed apps

#### **Application Access**
- **Clickable App Names**: App names are clickable links for direct access to applications
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
- **Status Monitoring**: See if each app is running, stopped, or crashed
- **Connection Counts**: View active connections per application
- **Process Information**: See process IDs and ports for each app
- **Individual Restart**: Restart specific applications without affecting others
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

The management interface exposes a REST API for programmatic access:

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
curl -X POST http://localhost:3839/api/apps/sales/restart

# Shutdown server
curl -X POST http://localhost:3839/api/shutdown
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

### 1. Sales Dashboard (`apps/sales/`)
- Single-file Shiny app demonstrating basic dashboard
- Simple plot with sample sales data

### 2. Inventory Management (`apps/inventory/`)
- Multi-file Shiny app (ui.R + server.R)
- Interactive table with dynamic data generation

### 3. Business Reports (`apps/reports/`)
- R Markdown flexdashboard with `runtime: shiny`
- Interactive charts, KPIs, and data tables
- Demonstrates plotly integration and responsive design

### 4. Interactive Dashboard (`apps/dashboard/`)
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

## Code Organization

The project is organized into several key components:

- `main.R` - Main server entry point
- `R/` - Core server modules:
  - `config.R` - Configuration management
  - `connection_manager.R` - WebSocket connection handling
  - `handlers.R` - HTTP request routing
  - `management_api.R` - Management interface API
  - `process_manager.R` - Application process management
  - `template_manager.R` - HTML template rendering
  - `utils.R` - Utility functions
  - `validation.R` - Input validation
- `templates/` - HTML templates and CSS styles
- `apps/` - Shiny applications
- `logs/` - Server and application logs
- `config.json` - Server configuration

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

## Troubleshooting

### Common Issues

**Apps won't start:**
- Check that the app directory exists and contains valid Shiny code
- Verify ports aren't already in use
- Check app-specific error logs in `logs/{app_name}_error.log`

**WebSocket connection failures:**
- Ensure backend app is running and healthy
- Check for firewall issues blocking WebSocket connections
- Verify session affinity is working correctly

**Memory usage growing:**
- Monitor connection cleanup logs
- Check for apps with memory leaks
- Verify cleanup intervals are appropriate for your load

**Performance issues:**
- Monitor health check logs for slow apps
- Consider adjusting health check intervals
- Check for resource exhaustion in individual apps

**Management interface not accessible:**
- Verify server is running and listening on port 3839
- Check that you're accessing http://localhost:3839 (not external IP)
- Ensure no firewall is blocking localhost connections
- Check logs for management server startup messages

**Connection tracking not showing data:**
- Verify clients are making WebSocket connections (not just HTTP)
- Check that apps are properly proxied through the server
- Monitor logs for WebSocket connection messages

### Debug Mode

For detailed debugging, monitor the logs:

```bash
# Follow main server logs
tail -f logs/server.log

# Monitor specific app
tail -f logs/sales_error.log
```

## Security Considerations

⚠️ **Important**: This server is designed for development and internal use. For production deployment, consider:

- **Management Interface Security**: Currently restricted to localhost only
- Adding authentication and authorization for both proxy and management interfaces
- Implementing rate limiting on management API endpoints
- Setting up SSL/TLS encryption for all interfaces
- Configuring proper firewall rules
- Regular security updates of dependencies
- Network segmentation if exposing the proxy server externally

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test thoroughly
5. Submit a pull request

## Dependencies

### R Packages
- `shiny` - Core Shiny framework
- `httpuv` - HTTP and WebSocket server
- `websocket` - WebSocket client support
- `jsonlite` - JSON parsing
- `callr` - Background process management
- `later` - Asynchronous scheduling
- `httr` - HTTP client for proxying
- `digest` - Session ID generation
- `future` - Asynchronous operations
- `promises` - Promise-based async programming
- `logger` - Structured logging
- `rmarkdown` - R Markdown support
- `quarto` - Quarto document support
- `flexdashboard` - Dashboard layouts
- `DT` - Interactive data tables
- `plotly` - Interactive plots
- `dplyr` - Data manipulation

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Support

For issues and questions:
- Check the logs for error messages
- Review this README for common solutions
