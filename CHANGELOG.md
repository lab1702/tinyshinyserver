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
  - Management API now exposes `resident` flag for each app

### Changed
- **Breaking**: All apps now default to `resident: false` (on-demand) instead of always running
- Health check system now skips stopped non-resident apps (normal behavior)
- App status API now includes `resident` flag and enhanced status information
- Enhanced logging for on-demand app lifecycle events
- Non-resident apps now stop immediately when last connection closes (no delay)

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