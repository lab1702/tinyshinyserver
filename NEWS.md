# tinyshinyserver (development version)

* The management server now rejects requests whose `Host` header is not a loopback name (`localhost`, `127.0.0.1`, or `[::1]`), protecting it from DNS-rebinding attacks. **A reverse proxy in front of the management port must forward the upstream address as the host** (for Caddy, `header_up Host {upstream_hostport}`; see the README).

* App processes now write their stdout and stderr directly to the per-app log files. Output from child processes and native code (for example pandoc) is now logged and can no longer fill an unread pipe and hang the app.

* Starting an app keeps the previous run's logs as `{app_name}_output.prev.log` and `{app_name}_error.prev.log`, so a crash traceback survives the automatic restart.

* WebSocket messages for a client session that is no longer tracked no longer open an orphaned backend session.

* Client IP addresses shown in the management dashboard and logs are taken from `X-Forwarded-For` or `X-Real-IP` only when the request comes from a reverse proxy on the same machine, and then from the entry that proxy added.

# tinyshinyserver 0.2.1

* Fixed a race in WebSocket proxy tests that caused `invalid state` errors on some CRAN check machines: test clients are now closed before the servers they are connected to are stopped.

* Stopping or restarting an app no longer reports a failure when one of the app's child processes exits while it is being terminated.

# tinyshinyserver 0.2.0

* Process shutdown now waits briefly for exit after sending termination signals, avoiding premature failure reports on macOS.

* Help examples now use temporary directories and restore the working directory on exit. Probing an unused port no longer emits an expected connection warning.

## Configuration

* Added per-app `appstart_timeout` to set the startup grace period before requests receive HTTP 503 if the app is not ready. The timeout is measured from app startup, defaults to 2 seconds, and supports positive fractional seconds.

* Invalid configuration now fails before apps start: app names must be unique, ports must be finite whole numbers, `restart_delay` must be a non-negative finite number, and `health_check_interval` must be a positive finite number. This prevents ambiguous app routing and timer errors that could interrupt monitoring or restarts.

* Fixed port allocation near the upper port limit so valid configurations are not rejected because of reserved ports outside the allocation range.

## Security and sessions

* Management restart and shutdown requests now require the `X-TinyShinyServer-Request: management` header to block cross-origin browser requests. **API clients must add this header.** The management dashboard sends it automatically.

* Fixed cookie sharing between proxied requests that could expose one user's session to another. Cookies returned by apps are scoped to their app's proxy path.

* Connection details in the management dashboard now display as plain text, preventing client-supplied headers from injecting HTML or scripts.

## Proxy compatibility and responsiveness

* Slow app startup, HTTP responses, and restart delays no longer block unrelated requests or active sessions.

* Fixed HTTP and WebSocket forwarding of directory paths and query strings, including document-specific WebSocket URLs. App URLs without a trailing slash now redirect while preserving the request method and query string.

* Fixed forwarding of binary WebSocket messages and each browser's authentication headers. Active streaming sessions are no longer disconnected as idle when traffic comes only from the app.

* Preserved download filenames, cache headers, compressed responses, and multiple cookies. Redirects to backend-local URLs now stay on the public app route.

## App lifecycle

* On-demand apps now finish in-flight HTTP requests before stopping and allow a new page time to establish its WebSocket session. Apps started by visits that never connect, abandoned WebSocket attempts, or restarts without returning users are stopped after a grace period.

* Fixed recovery after app crashes and failed connections. Browsers are disconnected when their backend connection fails, and callbacks from an earlier app instance no longer interfere with its replacement.

* App shutdown and crash recovery now clean up surviving child processes, including before an on-demand relaunch. Failed termination no longer causes the server to lose track of a process or start a duplicate replacement. Failed server startup also cleans up apps and listeners already started.

* A health-check or app-start error no longer prevents other apps from being monitored. Failed resident apps respect `restart_delay`, and stopping the server cancels scheduled restarts.

## Examples and documentation

* The inventory example now accepts zero items as an empty table and displays a validation message for negative, missing, or fractional counts.

* Clarified example dependencies and Quarto setup, relative-path resolution from the R working directory, startup timeouts, on-demand shutdown behavior, and network access requirements. Added a minimal Shiny-only configuration to the quick start.

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
