# tinyshinyserver 0.2.0

* Preserved in-flight HTTP requests during on-demand shutdown and redirected bare app URLs to their trailing-slash form.

* Fixed child-process cleanup, failed WebSocket connection cleanup, health-check failure isolation, and additional backend-local redirect forms.

* Preserved proxy query strings and directory paths, forwarded WebSocket authentication headers, kept streaming sessions active, retained processes after failed termination, and made restart delays asynchronous.

* Fixed stale startup callbacks, connection shutdown, partial startup cleanup, duplicate app names, and proxy response headers, redirects, and cookie paths.

* Isolated proxy request cookies, escaped connection metadata in the management UI, and made startup waits and backend HTTP requests asynchronous.

* Added per-app `appstart_timeout` to configure the startup grace period (default: 2 seconds).

# tinyshinyserver 0.1.0

* Initial CRAN submission
* Core features:
  - Multi-application Shiny proxy server
  - WebSocket support with session affinity
  - Automatic health monitoring and restart
  - Resident (always-running) and on-demand application modes
  - Real-time management interface
  - Support for traditional Shiny apps, R Markdown, and Quarto dashboards
  - Automatic port assignment for applications
  - Connection tracking with graceful cleanup
