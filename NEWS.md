# tinyshinyserver 0.3.0

## Breaking changes

* **Reverse proxies must now send a loopback `Host` header.** When `proxy_host` is a loopback address, the proxy rejects requests whose `Host` header is not `localhost`, `127.0.0.1`, or `[::1]`; the management server always does. A reverse proxy on the same machine must forward the upstream address as the host (for Caddy, `header_up Host {upstream_hostport}`) and set `X-Forwarded-Host` to the public host (Caddy does by default). A reverse proxy on another machine, with `proxy_host` set to `"0.0.0.0"` or `"::"`, must preserve the public `Host` header. See the updated Caddy example in the README.

* **The proxy now rejects HTTP request bodies larger than 100 MB, and all chunked request bodies,** with HTTP 413 before reading them. Previously the whole body was read into memory before being forwarded, so one very large upload could exhaust the server's memory and stop every app. Apps that accept larger uploads through `shiny.maxRequestSize` need the new `max_request_size_mb` option raised to match. The management server, whose requests need no body, rejects bodies larger than 64 KB.

* The proxy's public `/api/apps` endpoint now returns only each app's name, status, mode, and connection count. App paths, ports, and process IDs are available only from the loopback management API.

* On-demand apps now stop 30 seconds after their last WebSocket connection closes instead of immediately. Reloading or navigating within an on-demand app previously stopped it between pages, forcing a cold restart that could make the new page's requests fail with HTTP 503.

## Security

* The proxy and management server reject DNS-rebinding requests through the `Host` header checks above, so websites cannot read or control local apps through the proxy or use the management API. App processes still listen on their own loopback ports without these checks; see "Network access and authentication" in the README.

* The proxy rejects app WebSocket connections whose browser `Origin` does not match the request host, so other websites can no longer open app sessions with a visitor's cookies or reverse-proxy credentials.

* Management responses send `X-Frame-Options: DENY` and `Content-Security-Policy: frame-ancestors 'none'` so the dashboard's restart controls cannot be clickjacked from a framing page.

* The proxy forwards HTTP requests and WebSocket sessions, with their cookies and credentials, only when the app's own process is listening on the app's port. Previously, another program holding a dormant on-demand app's port, such as a second server instance with the same `starting_port`, received that app's traffic.

* Client IP addresses shown in the management dashboard and logs are taken from `X-Forwarded-For` or `X-Real-IP` only when the request comes from a reverse proxy on the same machine, and then from the entry that proxy added.

* App WebSocket sessions now receive the same request headers as proxied HTTP requests, so `session$request` includes the identity headers that an authenticating reverse proxy adds, such as `X-Forwarded-User`, as well as `User-Agent` and `X-Forwarded-For`. Previously only `Cookie` and `Authorization` were forwarded.

* A request header spelled with an underscore no longer takes the place of the real header. R's web server reads `Content_Length` as `Content-Length` and `X_Forwarded_User` as `X-Forwarded-User`, and the underscore spelling wins. A client could therefore send a body of any size, chunked or not, past `max_request_size_mb` by also declaring a small `Content_Length`, and a user behind an authenticating reverse proxy could replace the identity header that the reverse proxy set. The proxy now reads the body size, and the forwarding headers behind the client IP addresses it shows, under the names the client sent. It rejects a chunked body whatever length is also declared, and it does not pass a header whose name contains an underscore on to an app.

* HTTP 502 responses from the proxy no longer include the underlying connection error, which named the app's internal address and port. The error is written to `server.log`.

* Requests to apps no longer follow the `http_proxy` and `ALL_PROXY` environment variables. Where R was configured to use a proxy and `no_proxy` did not list `127.0.0.1`, app requests, with their cookies and bodies, were sent to that proxy, and every app answered HTTP 502 or 503.

* An unexpected error while answering a request is now answered with HTTP 500 and no detail, and the error is written to `server.log`. The client used to receive the R error message; on Windows, a request for `/templates/nul` returned the server's installation path that way, and now returns HTTP 404. An error reading a page's style sheet or script no longer names the file either. The management API's restart endpoint answers an unexpected error the same way; a restart that cannot start the app still reports why.

* The INFO message that records a WebSocket connection to an app no longer includes the connection's query string, which can carry a token. The full address is logged at DEBUG.

## Web pages

* The landing page and management dashboard have a new design. They follow the system light or dark theme by default and have a theme toggle; an explicit choice is remembered in the browser until it is switched back to match the system.

* A browser that opens an app before it is ready, for example an on-demand app that is still starting, now receives a page that reloads itself until the app answers, for up to two minutes. It previously showed the proxy's JSON error until the visitor reloaded by hand. The response is still HTTP 503 with `Retry-After`, and scripts and other clients still receive JSON.

* Management dashboard actions report their results in an inline notice instead of browser alert dialogs.

* Restarting an app refreshes the dashboard's status tiles and connection list at once. They previously kept their old values, such as the number of running apps, for up to 5 seconds.

* `POST /api/apps/{name}/restart` returns HTTP 404 instead of 500 for an app name that is not configured, and accepts only that path. A path with extra segments, such as `/api/apps/a/b/restart`, restarted app `a`; it now returns HTTP 400.

* The proxy's own pages and endpoints (`/`, `/health`, `/api/apps`, and the files under `/templates/`) answer methods other than `GET` and `HEAD` with HTTP 405. They answered every method as if it were a `GET`.

* Requests for a directory under `/templates/` return HTTP 404 instead of 500.

* Files under `/templates/` are sent as their bytes. Files other than images were read as lines of text, which changed line endings and dropped a final newline, and would have corrupted fonts and other binary files; web fonts and WebP images are now sent with their own types, and unknown types as `application/octet-stream` instead of `text/plain`.

* A request with a method that neither server supports on any page, such as `PROPFIND`, receives HTTP 501 (Not Implemented) instead of a 405 without the `Allow` header that a 405 requires. Method names are matched exactly, as HTTP defines them, so `get` is not taken for `GET`.

* When restarting an app or shutting down fails with an error, the management dashboard shows the server's reason instead of "undefined", and a failed restart's notice names the app once instead of repeating itself.

* The management dashboard shows the server's uptime and memory use, and each running app's memory use and uptime. `/api/status` returns `uptime_seconds`, `server_memory_bytes`, and `apps_memory_bytes` in place of the `server_uptime` and `memory_usage` fields, which were always `"N/A"`; the management `/api/apps` returns `memory_bytes` and `uptime_seconds` for running apps. Memory is the resident memory of each R process and is left out where the `ps` package cannot read it.

* The R version and operating system are now shown on the management dashboard instead of the public landing page.

* The management dashboard has a **Reload Config & Restart All** button, also available as `POST /api/reload`. It reads the configuration file again, stops every app, and starts the resident apps under the new configuration, so apps can be added, removed, or changed without restarting the server. An invalid file, a `log_dir` where the server log cannot be written, or a change to `proxy_host`, `proxy_port`, or `management_port`, is rejected before any app stops. If an app cannot be stopped, the previous configuration stays in effect. The dashboard reports the reload's outcome, which `/api/status` also returns as `last_reload`. The landing page picks up added and removed apps.

* The new `base_path` configuration option serves the landing page and apps under a URL prefix, such as `https://example.com/shiny/`, behind a reverse proxy. The proxy accepts requests with or without the prefix, so the reverse proxy may pass it through or strip it, and landing page links, redirects, and app cookie paths include it. The management dashboard now uses relative URLs, so it also works under a prefix that the reverse proxy strips.

* The new `title` configuration option sets the name shown in the browser tab and top bar of the landing and management pages (default: "Tiny Shiny Server").

* The new `idle_timeout_minutes` configuration option sets how long a session may carry no WebSocket message before it is closed (default: 30, the limit that was fixed before). `0` keeps idle sessions open, for dashboards that people leave open without interacting.

* The new `log_level` configuration option chooses the least severe messages written to `server.log` (default: "INFO"). The package's DEBUG messages, which could not be turned on before, record each request's routing and the connection bookkeeping, and the first 100 characters of every WebSocket message.

* Restarting an app from the management dashboard or with `POST /api/apps/{name}/restart` no longer waits for `restart_delay`, which left a healthy app answering HTTP 503 for 5 seconds by default; the delay now applies only to restarts after a failure. Restarting an on-demand app stops it until it is next opened, instead of starting it with nobody connected and stopping it again half a minute later.

* An on-demand app whose directory is missing, or holds no supported entry point, now has the status **unavailable** in `/api/apps`, on the landing page, and on the management dashboard, instead of **dormant**. Its landing page tile is disabled; it used to invite a click that could only fail with HTTP 502. The status returns to dormant as soon as the directory can be served.

* The management API's `/api/apps` leaves out `pid` for an app that is not running. It returned `"pid": {}`.

## Apps and logging

* App processes now write their stdout and stderr directly to the per-app log files. Output from child processes and native code (for example, Pandoc) is now logged and can no longer fill an unread pipe and hang the app.

* Starting an app keeps the previous run's logs as `{app_name}_output.prev.log` and `{app_name}_error.prev.log`, so a crash traceback survives the automatic restart.

* `start_tss()` now configures logging only for the package's own logger namespace, so the caller's `logger` threshold and appender are no longer changed for the rest of the R session.

* WebSocket messages for a client session that is no longer tracked no longer open an orphaned backend session.

* The server now opens the proxy and management ports before it starts any app. A second server started by mistake on the same ports stops with an error straight away; it previously started its own app processes and renamed the running server's app logs to `.prev.log` first.

* Configuration fields that the server does not recognize, such as a misspelled `resident`, are now reported as a warning in `server.log` at startup, and in the dashboard's notice after a reload. They were ignored without notice.

* Proxied responses no longer carry two `Date` headers, and a response that an app sends without a `Content-Type`, such as HTTP 304, is no longer labelled `text/html`.

* Apps can now be stopped and restarted on platforms where the `ps` package cannot list processes. Stopping an app failed there, which also blocked restarts, reloads, and shutdown; on those platforms only the app's own process is stopped, not processes it started.

* An on-demand app's finished HTTP requests no longer each leave a timer queued for 30 seconds; a newer idle check now replaces the pending one.

* WebSocket messages larger than 32 MB from an app to the browser, such as large htmlwidget outputs, no longer disconnect the browser session.

* Proxied HTTP requests are no longer cut off after 30 seconds. Once the app accepts the connection, a request fails only if the app sends no data for 10 minutes, so slow downloads and documents rendered on request complete.

* Proxied responses are passed to the browser as the app's bytes. The proxy previously scanned responses with other content types, such as CSV, JSON, and ZIP downloads, for zero bytes and converted text to a string, briefly using about 11 times the response size in memory; text responses over 2 GB failed.

* Proxied HTTP requests and readiness checks now close their connections to the app when they finish. The connections previously stayed open until R ran garbage collection, so bursts of requests left many idle sockets open in both the server and the app. With libcurl older than 8, each readiness check also left a connection open.

* Uploads to apps are no longer limited to about 4 MB per second, and every proxied request is faster. A transfer now continues as soon as the app's socket is ready instead of on a 10-millisecond timer, and a request to a running app no longer opens a separate connection first to check that the app is listening. This requires `later` 1.4.0 or later.

* An upload that an app refuses from its headers, such as one above `shiny.maxRequestSize`, now returns the app's HTTP 413 response. It returned HTTP 502 when the client sent `Expect: 100-continue`, as curl does for large uploads. The proxy now offers request bodies over 64 KB to the app with `Expect: 100-continue` and sends them once the app agrees.

* Stopping the server with Ctrl-C, or after an error in its event loop, no longer runs the shutdown twice, which repeated its log messages. A shutdown interrupted by a second Ctrl-C still runs again.

* Assigning app ports no longer waits about a second for each app on Windows, which delayed startup and paused all apps' traffic during a configuration reload. A port now counts as in use when the server cannot listen on it.

* The proxy no longer rejects, with HTTP 400, query strings that contain a literal `%`, such as `?discount=50%&x=1`, or paths with a name that only starts or ends with two dots, such as `/..hidden/`. Paths with a `..` segment are still rejected.

* Configuration validation now rejects an empty `log_dir`, which put the logs in the filesystem root, an app `resident` value of `null`, which stopped the server at startup, and app names that differ only in letter case, which shared log files on Windows and macOS.

* Query strings may now be up to 8,192 characters (previously 2,048), so Shiny URL bookmarks with many inputs load.

* Port assignments are now written to `server.log`, not only to the console.

* If `server.log` can no longer be written while the server runs, messages still go to the console. Previously the write error stopped health checks and interrupted shutdown, leaving the proxy, management server, and apps running. If `log_dir` is removed, starting an app creates it again, so apps can still start and `server.log` resumes. A `log_dir` where `server.log` cannot be written at startup is reported as such.

* A resident app whose process cannot be started, for example because its log files cannot be opened, is now reported and retried by health checks. It previously stopped the server at startup, and during a reload kept the remaining resident apps from starting and reported the reload as failed. An on-demand app that cannot start now returns HTTP 502 and is logged in `server.log`, instead of HTTP 500 with the internal error text.

* A shutdown through the management API now stops only that server. It previously wrote `shutdown.flag` to `log_dir`, which also stopped any other server sharing that directory, and left the file behind.

* App processes are now supervised, so they stop instead of holding their ports if the R session running the server is killed or crashes.

* Package log messages are formatted correctly even when the session's global `logger` formatter is not glue.

* Resident apps that die during startup, or are found without a process by a health check, now wait `restart_delay` before restarting instead of restarting at the next health check.

* The management dashboard shows "N/A" instead of "[object Object]" as the PID of apps that are not running.

* Startup readiness checks now continue for the full 30-second startup window (or `appstart_timeout`, if longer) instead of giving up after about 5 seconds. Slow-starting apps, such as Quarto documents that render first, no longer log a false "failed to become ready" error, and WebSocket clients keep receiving the "App is starting" reply until the app is ready.

* The management dashboard shows app paths containing characters such as `<` or `&` literally instead of interpreting them as HTML.

* The inventory example app accepts at most 10,000 items, so a visitor can no longer make it allocate unbounded memory.

* An app whose directory is missing, or contains no `app.R`, `ui.R` and `server.R`, `.Rmd`, or `.qmd` file, is no longer started. A resident app with a mistyped `path` previously started a new R process that exited, every 10 to 15 seconds, indefinitely, with the reason only in the app's error log. The reason is now written to `server.log`, for on-demand apps already when the configuration is loaded. Whether an on-demand app's directory can be served is looked up again when the directory changes, and at least every 30 seconds, instead of for every status request from every open landing page.

* R Markdown and Quarto documents are found whatever the letter case of their extension, such as `report.rmd` or `dash.QMD`, as `rmarkdown` and Quarto accept them. Such an app was reported as having nothing to serve.

* A directory with several documents serves the one that `rmarkdown` picks, instead of the first in alphabetical order. Files whose names start with `_`, which are partials that other documents include, are never served; such a file, such as `_child.Rmd`, could be served in place of the app, and a directory holding only partials now says so in `server.log`. An `index` or `ui` document is preferred, then one whose front matter sets `runtime: shiny` or `server: shiny`, so a static page no longer displaces the app. Among the rest, a document spelled `.Rmd` or `.qmd` comes before other letter cases, so a child document such as `appendix.rmd` does not displace `report.Rmd`.

* A Quarto document is rendered only when its rendered page is missing or older than another file in the app directory, instead of on every start. On-demand Quarto apps and restarts no longer wait for a full render each time. Delete the document's `.html` file to force a render. The check runs in the app's own process, so reading a large app directory does not hold up the other apps' traffic, and the app's error log records whether the document was rendered.

* A configuration in which two apps serve the same Quarto document, or the same prerendered R Markdown document (`runtime: shinyrmd` or `shiny_prerendered`, or `server: shiny`), is rejected when it is loaded or reloaded. Such documents are rendered into their own directory, and two renders at once could leave a broken page or none, which was then served at every start because it was newer than the document. The README explains how to recover, and that two server instances must not share such a directory either.

* An app receives the request path exactly as the client sent it. `.` and `..` segments were removed on the way, including their `%2e` spellings, so `/proxy/app/a/%2e%2e/c` reached the app as `/c`.

* Shutdown and a configuration reload try once more to stop an app process that outlives the first attempt, and `server.log` names any app that is still running afterwards. It reported "All applications stopped" and "Server shutdown complete" regardless.

* The HTTP 502 answer "App startup timed out" goes to the first request after an app's startup window has ended, and only if the app still does not answer. Whether it was sent used to depend on which request or check came first: a WebSocket connection could use it up.

* `server.log` records the close of a WebSocket session once. A session that the proxy closed was recorded twice, and one that was never opened was recorded as closed.

* The proxy connects to an app's WebSocket as soon as the browser's WebSocket opens, instead of at the browser's first message. Sockets on which only the app speaks, such as Shiny's autoreload socket, now work.

* A WebSocket to a path such as `/proxy//app/websocket/` is rejected, as the same HTTP path already was. It was connected to the app's default socket whatever path followed the app name.

* `base_path` values ending in `//`, such as `"/shiny//"`, are rejected. They were accepted with a trailing slash, which made requests under the prefix return HTTP 404.

* `start_tss()` reports a `config` argument that is not a single file path, instead of failing with an unrelated error.

* A WebSocket message no longer rewrites the server's records of every connected session, so busy sessions cost less as the number of sessions grows. Exited app processes are removed by the health check alone; the five-minute cleanup repeated that work.

* A configuration field whose name begins with an option's name, such as `proxy_port_old` or `title_draft`, no longer sets that option. Such a field was used, without validation, when the option itself was absent, because R matches list names by prefix. Unrecognized fields are now removed from the configuration as well as reported.

* An app response larger than 8 MB is written to a temporary file as it arrives and sent from there, instead of being held in the server's memory, so a large download no longer occupies the memory of the process that serves every app. This requires `curl` 3.3 or later.

* Proxied responses that cannot have a body, HTTP 204 and 304 and the response to a `HEAD` request, are sent without one. For a client that accepts gzip, a compressed empty body followed them, which is not allowed and could make the client drop the connection.

* The proxy now forwards `PATCH` requests to apps. It answered them with HTTP 405.

* An on-demand app that is still starting is no longer stopped as unused 30 seconds after the last request. It is left to finish, for up to 10 minutes from its start, and stops after the usual 30 seconds once it is ready if nobody has opened it. A slow start, such as a Quarto render that takes longer than the two minutes for which the visitor's page reloads, is no longer thrown away and begun again on the next visit.

* The pages' style sheet and script are sent with `Cache-Control` and `ETag` headers, so browsers reuse them instead of fetching them again for every page, including each reload of the page shown while an app starts. The management dashboard's R version and operating system are read once instead of on every page load.

* A request body that the client sent without a `Content-Type` now reaches the app without one; the proxy labelled it `application/x-www-form-urlencoded`. A request with `Content-Length: 0` now reaches the app with that header and its `Content-Type`, which were both dropped.

## Package

* The package now requires R 4.1.0 or later, as its `promises` and `quarto` dependencies already do.

* Removed the unused `future` dependency. `tools` and `utils` are now declared imports, as is `glue`, which `logger` needs to format the package's log messages. Added `ps`, already required by `callr`, to identify the process listening on an app's port.

* Removed the `digest` dependency. Session IDs are 256 random bits from `openssl`; hashing them with request details added nothing.

* The package now requires `httpuv` 1.5.2 or later, the first version whose `startServer()` has the `quiet` argument, and `ps` 1.2.0 or later, the first version with `ps_connections()`. With an older `httpuv`, every port was reported as in use and the server did not start.

* The browser tests in `tests/browser`, which need Node.js and Playwright, are no longer included in the built package.

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

* Initial CRAN submission.
* Core features:
  - Multi-application Shiny proxy server
  - WebSocket support with session affinity
  - Automatic health monitoring and restart
  - Resident (always-running) and on-demand application modes
  - Real-time management interface
  - Support for traditional Shiny apps, R Markdown, and Quarto dashboards
  - Automatic port assignment for applications
  - Connection tracking with graceful cleanup
