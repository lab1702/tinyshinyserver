# Tiny Shiny Server

A lightweight, WebSocket-enabled proxy server for hosting multiple Shiny applications with automatic health monitoring, session management, and resource cleanup.

## Features

- **Multi-App Hosting**: Host multiple Shiny applications on different ports behind a single proxy
- **WebSocket Support**: Full WebSocket proxy with session affinity for real-time Shiny apps
- **Health Monitoring**: Automatic health checks with app restart on failure
- **Memory Management**: Built-in connection cleanup and resource management
- **R Markdown Support**: Native support for interactive R Markdown documents with `runtime: shiny`
- **Graceful Shutdown**: Clean process termination with Ctrl-C
- **Binary Asset Support**: Handles images, fonts, and other binary files correctly
- **Logging**: Comprehensive logging with per-app log files

## Quick Start

### Prerequisites

- R (version 4.0 or higher)
- Required R packages (automatically installed via setup script)
- **Pandoc**: Required for R Markdown apps to work
  - Linux: `sudo apt install pandoc` (Ubuntu/Debian) or equivalent for your distribution
  - Windows: `winget install JohnMacFarlane.Pandoc`
  - macOS: Available via Homebrew or direct download from pandoc.org

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
   Rscript tiny_shiny_server.R
   ```

   **Windows:**
   ```powershell
   .\start-server.ps1
   ```
   
   The PowerShell script automatically locates your R installation and starts the server.

4. **Access applications:**
   - Landing page: http://localhost:3838
   - Individual apps: http://localhost:3838/proxy/{app_name}/

### Stopping the Server

Press `Ctrl-C` to gracefully shutdown the server. This will:
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
      "port": 3001
    },
    {
      "name": "inventory", 
      "path": "./apps/inventory",
      "port": 3002
    },
    {
      "name": "reports",
      "path": "./apps/reports",
      "port": 3003
    }
  ],
  "restart_delay": 5,
  "health_check_interval": 10,
  "proxy_port": 3838,
  "log_dir": "./logs"
}
```

### Configuration Options

| Option | Description | Default |
|--------|-------------|---------|
| `apps` | Array of Shiny applications to host | Required |
| `apps[].name` | Application identifier for URLs | Required |
| `apps[].path` | Relative path to app directory | Required |
| `apps[].port` | Port where app process runs | Required |
| `restart_delay` | Seconds to wait before restarting failed apps | 5 |
| `health_check_interval` | Seconds between health checks | 10 |
| `proxy_port` | Port for the proxy server | 3838 |
| `log_dir` | Directory for log files | "./logs" |

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
├── app.R          # Wrapper to run R Markdown
├── report.Rmd     # R Markdown with runtime: shiny
└── [other files]
```

## Example Applications

The project includes three example applications:

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

## Memory Management

The server includes automatic memory management features:

- **Connection Cleanup**: Removes stale connections after 30 minutes of inactivity
- **Queue Limits**: Limits pending message queues to 100 messages per connection
- **Process Cleanup**: Removes dead process objects from memory
- **File Handle Management**: Ensures proper cleanup of log file handles

Cleanup runs automatically every 5 minutes and logs activity for monitoring.

## Logging

### Log Files

- `logs/websocket_proxy.log` - Main server logs
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
```

### Adding New Applications

1. Create your app in `apps/{app_name}/`
2. Add configuration entry to `config.json`
3. Restart the server

### Health Endpoints

- Health check: `GET /health` - Returns `{"status": "healthy"}`

## Architecture

The server uses a multi-process architecture:

```
┌─────────────────────┐
│   Proxy Server      │
│   (Port 3838)       │
│                     │
│  ┌─WebSocket────────┤
│  │  Handler         │
│  │                  │
│  │  ┌─HTTP─────────┤
│  │  │  Handler      │
└──┼──┼───────────────┘
   │  │
   │  └── HTTP Requests ──┐
   │                      │
   └── WebSocket ─────────┼───┐
       Messages           │   │
                          ▼   ▼
              ┌──────────┐ ┌──────────┐ ┌──────────┐
              │ Sales    │ │Inventory │ │ Reports  │
              │(Port     │ │(Port     │ │(Port     │
              │ 3001)    │ │ 3002)    │ │ 3003)    │
              └──────────┘ └──────────┘ └──────────┘
```

### Key Components

- **HTTP Handler**: Routes requests to appropriate backend apps
- **WebSocket Handler**: Manages real-time connections with session affinity
- **Process Manager**: Monitors and restarts failed applications
- **Memory Manager**: Cleans up stale connections and resources

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

### Debug Mode

For detailed debugging, monitor the logs:

```bash
# Follow main server logs
tail -f logs/websocket_proxy.log

# Monitor specific app
tail -f logs/sales_error.log
```

## Security Considerations

⚠️ **Important**: This server is designed for development and internal use. For production deployment, consider:

- Adding authentication and authorization
- Implementing input validation and sanitization
- Setting up SSL/TLS encryption
- Configuring proper firewall rules
- Regular security updates of dependencies

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
- `rmarkdown` - R Markdown support
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