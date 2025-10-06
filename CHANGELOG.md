# Changelog

All notable changes to Tiny Shiny Server will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Resident Apps Feature**: New `resident` configuration option for individual apps
  - Apps with `resident: true` start when server starts and run continuously (existing behavior)
  - Apps with `resident: false` (default) start on-demand when first accessed and stop immediately when last connection closes
  - Maximum resource efficiency - apps only run when actively being used
  - Enhanced status reporting with "dormant" state for stopped on-demand apps
  - Per-app connection tracking with automatic lifecycle management
  - Management API now exposes `resident` flag and app type information
- **Smart UI Interactions**: Conditional clickable behavior based on app status
  - Landing page tiles only clickable when apps can be accessed
  - Running and dormant apps are clickable, stopped/crashed apps are disabled
  - Helpful tooltips explain app states and required actions
  - Visual feedback (dimming, hover effects) indicates app accessibility
- **Enhanced Connection Status**: Real-time server connectivity monitoring
  - 3-second timeout for faster disconnection detection
  - Cache-busting headers prevent stale status information
  - All tiles properly disabled when server is unreachable

### Changed
- **Breaking**: All apps now default to `resident: false` (on-demand) instead of always running
- Health check system now skips stopped non-resident apps (normal behavior)
- App status API now includes `resident` flag and enhanced status information
- Enhanced logging for on-demand app lifecycle events
- Non-resident apps now stop immediately when last connection closes (no delay)
- Management interface restart buttons hidden for dormant apps (with explanatory message)
- Reduced log verbosity: HTTP forwarding and management API calls moved to debug level
- Connection tracking now only uses WebSocket connections (not HTTP requests) for app lifecycle

### Fixed
- Resolved infinite start/stop loop for non-resident apps caused by HTTP asset requests
- Fixed landing page connection status to properly detect server shutdown
- Corrected app tile clickability when server becomes unreachable
- Fixed syntax errors in connection manager class definition
- Improved connection reliability for slow-starting Shiny applications
- Added port availability checking with retry logic (up to 5 seconds)
- Better error handling with 503 Service Temporarily Unavailable responses
- Enhanced debugging information for startup issues

### Migration Guide
To maintain existing behavior where all apps start immediately and run continuously, add `"resident": true` to each app in your `config.json`:

```json
{
  "apps": [
    {
      "name": "my-app",
      "path": "./apps/my-app",
      "resident": true
    }
  ]
}
```