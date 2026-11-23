# tinyshinyserver 0.3.0

## Breaking changes

* **Reverse proxies must now send a loopback `Host` header.** When `proxy_host` is a loopback address, the proxy rejects requests whose `Host` header is not `localhost`, `127.0.0.1`, or `[::1]`; the management server always does. A reverse proxy on the same machine must forward the upstream address as the host (for Caddy, `header_up Host {upstream_hostport}`) and set `X-Forwarded-Host` to the public host (Caddy does by default). A reverse proxy on another machine, with `proxy_host` set to `"0.0.0.0"` or `"::"`, must preserve the public `Host` header. See "Reverse proxy requirements" and the updated Caddy example in the README.

* **The proxy now rejects HTTP request bodies larger than 100 MB, and all chunked request bodies,** with HTTP 413 before reading them. Previously the whole body was read into memory before being forwarded, so one very large upload could exhaust the server's memory and stop every app. Apps that accept larger uploads through `shiny.maxRequestSize` need the new `max_request_size_mb` option raised to match. The management server, whose requests need no body, rejects bodies larger than 64 KB.

* **App processes now start in their app directory and read the `.Renviron` and `.Rprofile` there,** as R does when the app is run on its own, so an app's own environment variables and profile, such as one that activates renv, now apply to it. As at R's startup, `R_ENVIRON_USER` and `R_PROFILE_USER` take precedence, and an app directory without these files falls back to the user's `~/.Renviron` or `~/.Rprofile`. Previously every app read the `.Renviron` and `.Rprofile` of the directory that `start_tss()` was called from; an app that relied on those files needs its own copy. An error in an app's profile is written to its error log, and the app still starts.

* The proxy's public `/api/apps` endpoint now returns only each app's name, status, mode, and connection count. App paths, ports, and process IDs are available only from the loopback management API.

* On-demand apps now stop 30 seconds after their last WebSocket connection closes instead of immediately. Reloading or navigating within an on-demand app previously stopped it between pages, forcing a cold restart that could make the new page's requests fail with HTTP 503.

## New features

* The management dashboard has a **Reload Config & Restart All** button, also available as `POST /api/reload`. It reads the configuration file again, stops every app, and starts the resident apps under the new configuration, so apps can be added, removed, or changed without restarting the server. An invalid file, a `log_dir` where the server log cannot be written, or a change to `proxy_host`, `proxy_port`, or `management_port` is rejected before any app stops. If an app cannot be stopped, the previous configuration stays in effect. The dashboard reports the reload's outcome, which `/api/status` also returns as `last_reload`. The landing page picks up added and removed apps.

* New `base_path` option serves the landing page and apps under a URL prefix, such as `https://example.com/shiny/`, behind a reverse proxy. The proxy accepts requests with or without the prefix, so the reverse proxy may pass it through or strip it, and landing page links, redirects, and app cookie paths include it. The management dashboard now uses relative URLs, so it also works under a prefix that the reverse proxy strips.

* New `idle_timeout_minutes` option sets how long a session may carry no WebSocket message before it is closed (default: 30, the limit that was fixed before). `0` keeps idle sessions open, for dashboards that people leave open without interacting.

* New `log_level` option chooses the least severe messages written to `server.log` (default: `"INFO"`). The package's DEBUG messages, which could not be turned on before, record each request's routing, the connection bookkeeping, and the first 100 characters of every WebSocket message.

* New `title` option sets the name shown in the browser tab and top bar of the landing and management pages (default: `"Tiny Shiny Server"`).

* The landing page and management dashboard have a new design. They follow the system light or dark theme by default and have a theme toggle; an explicit choice is remembered in the browser until it is switched back to match the system.

* A browser that opens an app before it is ready, for example an on-demand app that is still starting, now receives a page that reloads itself until the app answers, for up to two minutes. It previously showed the proxy's JSON error until the visitor reloaded by hand. The response is still HTTP 503 with `Retry-After`, and scripts and other clients still receive JSON.

* The management dashboard shows the server's uptime and memory use, and each running app's memory use and uptime. `/api/status` returns `uptime_seconds`, `server_memory_bytes`, and `apps_memory_bytes` in place of the `server_uptime` and `memory_usage` fields, which were always `"N/A"`; the management `/api/apps` returns `memory_bytes` and `uptime_seconds` for running apps. Memory is the resident memory of each R process and is left out where the `ps` package cannot read it.

* A Quarto document is rendered only when its rendered page is missing or older than another file in the app directory, instead of on every start, so on-demand Quarto apps and restarts no longer wait for a full render each time. Delete the document's `.html` file to force a render. The check runs in the app's own process, so reading a large app directory does not hold up the other apps' traffic, and the app's error log records whether the document was rendered. When `log_dir` lies in the app directory, the server's log files there do not count as changes, since they are written at every start. Neither do hidden files and directories, such as `.git`, `.Rproj.user`, and `.Rhistory`, other than a top-level `.Rprofile` or `.Renviron`, nor the packages that renv installs under `renv/`, since version control, the IDE, and renv change them without the document changing.

* Starting an app keeps the previous run's logs as `{app_name}_output.prev.log` and `{app_name}_error.prev.log`, so a crash traceback survives the automatic restart.

## Security

* The proxy and management server reject DNS-rebinding requests through the `Host` header checks above, so websites cannot read or control local apps through the proxy or use the management API. App processes still listen on their own loopback ports without these checks; see "Security model" in the README.

* The proxy rejects app WebSocket connections whose browser `Origin` does not match the request host, so other websites can no longer open app sessions with a visitor's cookies or reverse-proxy credentials.

* Management responses send `X-Frame-Options: DENY` and `Content-Security-Policy: frame-ancestors 'none'` so the dashboard's restart controls cannot be clickjacked from a framing page.

* The proxy forwards HTTP requests and WebSocket sessions, with their cookies and credentials, only when the app's own process is listening on the app's port. Previously, another program holding a dormant on-demand app's port, such as a second server instance with the same `starting_port`, received that app's traffic.

* A request header spelled with an underscore no longer takes the place of the real header. R's web server reads `Content_Length` as `Content-Length` and `X_Forwarded_User` as `X-Forwarded-User`, and the underscore spelling wins. A client could therefore send a body of any size, chunked or not, past `max_request_size_mb` by also declaring a small `Content_Length`, and a user behind an authenticating reverse proxy could replace the identity header that the reverse proxy set. The proxy now reads the body size, the forwarding headers behind the client IP addresses it shows, and every other header that a decision depends on, such as `Host`, `Origin`, and `If-None-Match`, under the names the client sent. It rejects a chunked body whatever length is also declared, and it does not pass a header whose name contains an underscore on to an app.

* Client IP addresses shown in the management dashboard and logs are taken from `X-Forwarded-For` or `X-Real-IP` only when the request comes from a reverse proxy on the same machine, and then from the entry that proxy added.

* App WebSocket sessions now receive the same request headers as proxied HTTP requests, so `session$request` includes the identity headers that an authenticating reverse proxy adds, such as `X-Forwarded-User`, as well as `User-Agent` and `X-Forwarded-For`. Previously only `Cookie` and `Authorization` were forwarded.

* Requests to apps no longer follow the `http_proxy` and `ALL_PROXY` environment variables. Where R was configured to use a proxy and `no_proxy` did not list `127.0.0.1`, app requests, with their cookies and bodies, were sent to that proxy, and every app answered HTTP 502 or 503.

* An unexpected error while answering a request is now answered with HTTP 500 and no detail, and the error is written to `server.log`. The client used to receive the R error message; on Windows, a request for `/templates/nul` returned the server's installation path that way, and now returns HTTP 404. An error reading a page's style sheet or script no longer names the file either. The management API's restart endpoint answers an unexpected error the same way; a restart that cannot start the app still reports why.

* HTTP 502 responses from the proxy no longer include the underlying connection error, which named the app's internal address and port. The error is written to `server.log`.

* The INFO message that records a WebSocket connection to an app no longer includes the connection's query string, which can carry a token. The full address is logged at DEBUG.

* An R Markdown app serves only its entry document. `rmarkdown::run()` renders, and so runs the code of, any `.Rmd` or `.qmd` document under the app directory that a request names, including partials whose names start with `_` and drafts in subdirectories; such requests, and WebSocket connections for such documents, are now refused with HTTP 404.

* When a reverse proxy reports with `X-Forwarded-Proto: https` that a WebSocket arrived over HTTPS, the proxy refuses it if the page that opens it comes from the plain-HTTP site of the same host. A page injected into unencrypted traffic could otherwise use the HTTPS site's sessions and credentials.

* Request headers that the client names in its `Connection` header, and `Proxy-Connection`, are no longer passed on to apps, as response headers named in the app's `Connection` header already were not.

* The `Host` and `Origin` of a rejected WebSocket handshake are written to `server.log` with control characters replaced and cut to 200 bytes, so a client cannot move the cursor of a terminal that follows the log or forge log lines.

* Forwarding a request's headers no longer takes time that grows with the square of their number. A request with some 13,000 empty headers, which fit within httpuv's size limit, held up every app for a second.

* The management dashboard shows app paths containing characters such as `<` or `&` literally instead of interpreting them as HTML.

## Apps

* An app whose directory is missing, or contains no `app.R`, `ui.R` and `server.R`, `.Rmd`, or `.qmd` file, is no longer started. A resident app with a mistyped `path` previously started a new R process that exited, every 10 to 15 seconds, indefinitely, with the reason only in the app's error log. The reason is now written to `server.log`, for on-demand apps already when the configuration is loaded. Such an on-demand app now has the status **unavailable** in `/api/apps`, on the landing page, and on the management dashboard, instead of **dormant**, and its landing page tile is disabled; it used to invite a click that could only fail with HTTP 502. The status returns to dormant as soon as the directory can be served. Whether a directory can be served is looked up again when the directory changes, and at least every 30 seconds, instead of for every status request from every open landing page.

* R Markdown and Quarto documents are found whatever the letter case of their extension, such as `report.rmd` or `dash.QMD`, as `rmarkdown` and Quarto accept them. Such an app was reported as having nothing to serve.

* A directory with several documents serves the one that the server picks, by the rules below, instead of the first in alphabetical order; `rmarkdown::run()` is told which document to serve, where it would otherwise choose its own, among the directory's `.qmd` files as well. Files whose names start with `_`, which are partials that other documents include, are never served; such a file, such as `_child.Rmd`, could be served in place of the app, and a directory holding only partials now says so in `server.log`. An `index` or `ui` document is preferred, then one whose front matter sets `runtime: shiny` or `server: shiny`, so a static page no longer displaces the app. Among the rest, a document spelled `.Rmd` or `.qmd` comes before other letter cases, so a child document such as `appendix.rmd` does not displace `report.Rmd`.

* A configuration in which two apps serve the same Quarto document, or the same prerendered R Markdown document (`runtime: shinyrmd` or `shiny_prerendered`, or `server: shiny`), is rejected when it is loaded or reloaded. Such documents are rendered into their own directory, and two renders at once could leave a broken page or none, which was then served at every start because it was newer than the document. The README explains how to recover, and that two server instances must not share such a directory either.

* Startup readiness checks now continue for the full 30-second startup window (or `appstart_timeout`, if longer), measured in time from the app's start, instead of giving up after about 5 seconds, and WebSocket clients receive the "App is starting" reply throughout it. An app whose process is still running after the window, such as a Quarto document that is still rendering, is checked every 2 seconds for up to 10 minutes from its start and logged as ready once it answers. Slow-starting apps no longer log a false "failed to become ready" error.

* An on-demand app that is still starting is no longer stopped as unused 30 seconds after the last request. It is left to finish, for up to 10 minutes from its start, and stops after the usual 30 seconds once it is ready if nobody has opened it. A slow start, such as a Quarto render that takes longer than the two minutes for which the visitor's page reloads, is no longer thrown away and begun again on the next visit.

* The HTTP 502 answer "App startup timed out" goes to the first request after an app's startup window has ended, and only if the app still does not answer. Whether it was sent used to depend on which request or check came first: a WebSocket connection could use it up.

* Restarting an app from the management dashboard or with `POST /api/apps/{name}/restart` no longer waits for `restart_delay`, which left a healthy app answering HTTP 503 for 5 seconds by default; the delay now applies only to restarts after a failure. Restarting an on-demand app stops it until it is next opened, instead of starting it with nobody connected and stopping it again half a minute later. A restart disconnects the app's users only once its process has stopped, so a restart that cannot stop the app leaves them connected.

* Resident apps that die during startup, or are found without a process by a health check, now wait `restart_delay` before restarting instead of restarting at the next health check.

* An on-demand app whose process failed, for example because the app stops with an error at startup, now starts again only after `restart_delay`, as a resident app does. Until then, a request for it receives HTTP 503 with a `Retry-After` header that counts down the delay, and a browser receives the page that reloads itself. Every request used to start a new R process, so the page that reloads every 2 seconds, open in each visitor's tab, kept starting the failing app. This also applies to an app that crashes while in use. The management dashboard shows how long the app waits, and its Restart button, like `POST /api/apps/{name}/restart` and a configuration reload, ends the delay; the management `/api/apps` returns the wait as `restart_in_seconds`.

* An on-demand app whose process has exited is shown as **dormant**, which is what opening it does, until the health check cleans it up. It was shown as **crashed**, its landing page tile was disabled, and the tile advised a restart from the management dashboard, which only stops an on-demand app.

* A resident app whose process cannot be started, for example because its log files cannot be opened, is now reported and retried by health checks. It previously stopped the server at startup, and during a reload kept the remaining resident apps from starting and reported the reload as failed. An on-demand app that cannot start now returns HTTP 502 and is logged in `server.log`, instead of HTTP 500 with the internal error text.

* App processes are now supervised, so they stop instead of holding their ports if the R session running the server is killed or crashes.

* The server now opens the proxy and management ports before it starts any app. A second server started by mistake on the same ports stops with an error straight away; it previously started its own app processes and renamed the running server's app logs to `.prev.log` first.

* Shutdown and a configuration reload try once more to stop an app process that outlives the first attempt, and `server.log` names any app that is still running afterwards. It reported "All applications stopped" and "Server shutdown complete" regardless.

* A shutdown through the management API now stops only that server. It previously wrote `shutdown.flag` to `log_dir`, which also stopped any other server sharing that directory, and left the file behind.

* Stopping the server with Ctrl-C, or after an error in its event loop, no longer runs the shutdown twice, which repeated its log messages. A shutdown interrupted by a second Ctrl-C still runs again.

* An unexpected error that stops the server now ends `start_tss()` with an error once the servers and apps have stopped, so a server run with `Rscript` exits with a failure status that a service manager can restart it on. `start_tss()` used to return normally. The server object that `start_tss()` returns can no longer be started again, which left its servers and apps running after its second shutdown.

* An app's previous log is copied to `.prev.log` when it cannot be renamed, as on Windows when another program holds it open. Both the log and the older `.prev.log` were lost then.

* `R_ENVIRON_USER` and `R_PROFILE_USER` set to an empty value now keep apps from reading a user environment file or profile, as they do for R itself. Apps read the app directory's files, or the user's, instead.

* Apps can now be stopped and restarted on platforms where the `ps` package cannot list processes. Stopping an app failed there, which also blocked restarts, reloads, and shutdown; on those platforms only the app's own process is stopped, not processes it started.

* Assigning app ports no longer waits about a second for each app on Windows, which delayed startup and paused all apps' traffic during a configuration reload. A port now counts as in use when the server cannot listen on it.

* An on-demand app's finished HTTP requests no longer each leave a timer queued for 30 seconds; a newer idle check now replaces the pending one.

## Proxy

* Proxied HTTP requests are no longer cut off after 30 seconds. Once the app accepts the connection, a request fails only if the app sends no data for 10 minutes, so slow downloads and documents rendered on request complete.

* Uploads to apps are no longer limited to about 4 MB per second, and every proxied request is faster. A transfer now continues as soon as the app's socket is ready instead of on a 10-millisecond timer, and a request to a running app no longer opens a separate connection first to check that the app is listening.

* An app response larger than 8 MB is written to a temporary file as it arrives and sent from there, instead of being held in the server's memory, so a large download no longer occupies the memory of the process that serves every app. If R's temporary directory has been removed, for example by a cleaner of `/tmp` on a server that runs for weeks, it is created again. The README notes that httpuv keeps a response that a client on a keep-alive connection stops downloading, in memory or as this file, until the server stops (rstudio/httpuv#448).

* Proxied responses keep their headers when the app sends a chunked response with trailer fields. The trailer fields took the place of the response's headers, such as its `Content-Type`, cookies, and redirect.

* Every `Path` attribute of an app's cookie is scoped to the app, including ones written with spaces around `=`, such as `Path =/`. Browsers use the last `Path`, so an app could scope a cookie to every app; `Domain` attributes for the loopback address are removed however they are spaced.

* When an app is too busy to complete a WebSocket handshake within the 5 seconds that the `websocket` package allows, as while another visitor's long calculation runs, the proxy tries again for up to 2 minutes, with the browser's messages queued. The browser's session was closed, and its page greyed out.

* Proxied responses are passed to the browser as the app's bytes. The proxy previously scanned responses with other content types, such as CSV, JSON, and ZIP downloads, for zero bytes and converted text to a string, briefly using about 11 times the response size in memory; text responses over 2 GB failed.

* Proxied HTTP requests and readiness checks now close their connections to the app when they finish. The connections previously stayed open until R ran garbage collection, so bursts of requests left many idle sockets open in both the server and the app. With libcurl older than 8, each readiness check also left a connection open.

* An upload that an app refuses from its headers, such as one above `shiny.maxRequestSize`, now returns the app's HTTP 413 response. It returned HTTP 502 when the client sent `Expect: 100-continue`, as curl does for large uploads. The proxy now offers request bodies over 64 KB to the app with `Expect: 100-continue` and sends them once the app agrees.

* The proxy now forwards `PATCH` requests to apps. It answered them with HTTP 405.

* Query strings may now be up to 8,192 characters (previously 2,048), so Shiny URL bookmarks with many inputs load.

* The proxy no longer rejects, with HTTP 400, query strings that contain a literal `%`, such as `?discount=50%&x=1`, or paths with a name that only starts or ends with two dots, such as `/..hidden/`. Paths with a `..` segment are still rejected.

* An app receives the request path exactly as the client sent it. `.` and `..` segments were removed on the way, including their `%2e` spellings, so `/proxy/app/a/%2e%2e/c` reached the app as `/c`.

* A request body that the client sent without a `Content-Type` now reaches the app without one; the proxy labelled it `application/x-www-form-urlencoded`. A request with `Content-Length: 0` now reaches the app with that header and its `Content-Type`, which were both dropped.

* Proxied responses no longer carry two `Date` headers, and a response that an app sends without a `Content-Type`, such as HTTP 304, is no longer labelled `text/html`.

* Proxied responses keep their headers when one of them holds a byte that is not valid UTF-8, such as a Latin-1 file name in `Content-Disposition`; such bytes are passed on unchanged. Every header was dropped, so the browser received the body without its `Content-Type`, cookies, or redirect `Location`.

* Proxied responses that cannot have a body, HTTP 204 and 304 and the response to a `HEAD` request, are sent without one. For a client that accepts gzip, a compressed empty body followed them, which is not allowed and could make the client drop the connection.

* The proxy connects to an app's WebSocket as soon as the browser's WebSocket opens, instead of at the browser's first message. Sockets on which only the app speaks, such as Shiny's autoreload socket, now work.

* WebSocket messages larger than 32 MB from an app to the browser, such as large htmlwidget outputs, no longer disconnect the browser session.

* A WebSocket to a path such as `/proxy//app/websocket/` is rejected, as the same HTTP path already was. It was connected to the app's default socket whatever path followed the app name.

* WebSocket messages for a client session that is no longer tracked no longer open an orphaned backend session.

* A WebSocket message no longer rewrites the server's records of every connected session, so busy sessions cost less as the number of sessions grows. Exited app processes are removed by the health check alone; the five-minute cleanup repeated that work.

## Server pages and management API

* Every answer to a `HEAD` request that the proxy or the management server makes itself is sent without a body, such as the landing page, `/health`, the pages' style sheet, and the 503 for an app that is starting. The body was sent after the headers, so a client that sent another request on the same connection, as uptime monitors and HTTP libraries do, read it as the start of the next answer and failed.

* The proxy's own pages and endpoints (`/`, `/health`, `/api/apps`, and the files under `/templates/`) answer methods other than `GET` and `HEAD` with HTTP 405. They answered every method as if it were a `GET`. The management server's dashboard and `GET` endpoints also answer `HEAD` now, where they returned HTTP 404, and its pages and actions answer a method they do not serve with HTTP 405 instead of 404.

* A request with a method that neither server supports on any page, such as `PROPFIND`, receives HTTP 501 (Not Implemented) instead of a 405 without the `Allow` header that a 405 requires. Method names are matched exactly, as HTTP defines them, so `get` is not taken for `GET`.

* `POST /api/apps/{name}/restart` returns HTTP 404 instead of 500 for an app name that is not configured, and accepts only that path. A path with extra segments, such as `/api/apps/a/b/restart`, restarted app `a`; it now returns HTTP 400.

* The management API's `/api/apps` leaves out `pid` for an app that is not running. It returned `"pid": {}`, which the dashboard showed as "[object Object]"; it now shows "N/A".

* Management dashboard actions report their results in an inline notice instead of browser alert dialogs. When restarting an app or shutting down fails with an error, the notice shows the server's reason instead of "undefined", and a failed restart's notice names the app once instead of repeating itself.

* Restarting an app refreshes the dashboard's status tiles and connection list at once. They previously kept their old values, such as the number of running apps, for up to 5 seconds.

* The dashboard updates its app cards and connection list in place when it refreshes, instead of rebuilding them every 5 seconds, so the keyboard focus stays on a Restart button, a click that spans a refresh still lands, and selected text, such as a client's IP address, stays selected.

* The page shown while an app starts stops reloading after two minutes in every case. A reload continued the same wait only if it came soon enough after the page before it, judged by this page's own retry time, so a page that waited out a long `restart_delay`, a slow answer, or a browser that delays the timers of background tabs began the two minutes again, and a tab left open restarted a failing app forever.

* An open landing page reloads itself when a configuration reload changes `base_path`. Its status requests went to an address that no longer exists, and it showed every app as "server down".

* The R version and operating system are now shown on the management dashboard instead of the public landing page, and are read once instead of on every page load.

* The pages' style sheet and script are sent with `Cache-Control` and `ETag` headers, so browsers reuse them instead of fetching them again for every page, including each reload of the page shown while an app starts.

* Files under `/templates/` are sent as their bytes. Files other than images were read as lines of text, which changed line endings and dropped a final newline, and would have corrupted fonts and other binary files; web fonts and WebP images are now sent with their own types, and unknown types as `application/octet-stream` instead of `text/plain`.

* Requests for a directory under `/templates/` return HTTP 404 instead of 500.

## Configuration

* Configuration fields that the server does not recognize, such as a misspelled `resident`, are now reported as a warning in `server.log` at startup, and in the dashboard's notice after a reload. They were ignored without notice. They are now also removed from the configuration: a field whose name begins with an option's name, such as `proxy_port_old` or `title_draft`, no longer sets that option when the option itself is absent, as it did, without validation, because R matches list names by prefix.

* Configuration validation now rejects an empty `log_dir`, which put the logs in the filesystem root, an app `resident` value of `null`, which stopped the server at startup, and app names that differ only in letter case, which shared log files on Windows and macOS.

* `base_path` values ending in `//`, such as `"/shiny//"`, are rejected. They were accepted with a trailing slash, which made requests under the prefix return HTTP 404.

* `start_tss()` reports a `config` argument that is not a single file path, instead of failing with an unrelated error.

* `restart_delay`, `health_check_interval`, and `appstart_timeout` may be at most 86400 seconds (one day). A value of about 1e12 or more, such as a large `restart_delay` meant as "never", was accepted, and then made the server stop answering requests.

* A configuration file that gives a field twice, at the top level or in one app, is rejected. The first copy was used without notice, where most JSON readers use the last, and the other copy was not validated.

* A configuration reload checks that the server log can be written in `log_dir` even when `log_dir` has not changed, and stops no app if it cannot. Such a reload stopped every app, and resident apps could then not open their log files. When the server log cannot be moved to a changed `log_dir`, a changed `log_level` is now applied anyway, and the reload's outcome says that the previous server log was kept.

## Logging

* App processes now write their stdout and stderr directly to the per-app log files. Output from child processes and native code (for example, Pandoc) is now logged and can no longer fill an unread pipe and hang the app.

* `start_tss()` now configures logging only for the package's own logger namespace, so the caller's `logger` threshold and appender are no longer changed for the rest of the R session. Package log messages are formatted correctly even when the session's global `logger` formatter is not glue.

* If `server.log` can no longer be written while the server runs, messages still go to the console. Previously the write error stopped health checks and interrupted shutdown, leaving the proxy, management server, and apps running. If `log_dir` is removed, starting an app creates it again, so apps can still start and `server.log` resumes. A `log_dir` where `server.log` cannot be written at startup is reported as such.

* `server.log` records the close of a WebSocket session once. A session that the proxy closed was recorded twice, and one that was never opened was recorded as closed.

* Port assignments are now written to `server.log`, not only to the console.

## Examples

* The dashboard example's data cover fixed dates. Its date range was taken from the day the page was rendered and its data from the day the app started, so once its rendered page was reused, the newest data could not be selected, and after three months the dashboard showed no data at all.

* The inventory example app accepts at most 10,000 items, so a visitor can no longer make it allocate unbounded memory.

## Package

* The package now requires R 4.1.0 or later, as its `promises` and `quarto` dependencies already do.

* The package now requires `httpuv` 1.5.2 or later, the first version whose `startServer()` has the `quiet` argument; with an older `httpuv`, every port was reported as in use and the server did not start. It also requires `later` 1.4.0 (for `later_fd()`, which the faster transfers use), `curl` 3.3 (for `file_writer()`, which writes large responses to disk), `websocket` 1.1.0 (whose client accepts the `maxMessageSize` that large app messages need), and `ps` 1.2.0 (for `ps_connections()`, which identifies the process listening on an app's port).

* Removed the unused `future` dependency, and `digest` and `openssl`: session IDs only key the server's own connection tables and are never read from a request, so they are now numbered in order instead of hashed from random bits and request details. `tools` and `utils` are now declared imports, as is `glue`, which `logger` needs to format the package's log messages. Added `ps`, already required by `callr`.

* The browser tests in `tests/browser`, which need Node.js and Playwright, are no longer included in the built package.

# tinyshinyserver 0.2.1

* Fixed a race in WebSocket proxy tests that caused `invalid state` errors on some CRAN check machines: test clients are now closed before the servers they are connected to are stopped.

* Stopping or restarting an app no longer reports a failure when one of the app's child processes exits while it is being terminated.

# tinyshinyserver 0.2.0

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

* Process shutdown now waits briefly for exit after sending termination signals, avoiding premature failure reports on macOS.

* A health-check or app-start error no longer prevents other apps from being monitored. Failed resident apps respect `restart_delay`, and stopping the server cancels scheduled restarts.

* Probing an unused port no longer emits an expected connection warning.

## Examples and documentation

* The inventory example now accepts zero items as an empty table and displays a validation message for negative, missing, or fractional counts.

* Help examples now use temporary directories and restore the working directory on exit.

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
