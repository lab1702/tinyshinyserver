# tinyshinyserver 0.3.0

## Breaking changes

* **Reverse proxies must now send a loopback `Host` header.** When `proxy_host` is a loopback address, the proxy rejects requests with a `Host` header other than `localhost`, `127.0.0.1`, or `[::1]`; the management server always does. A reverse proxy on the same machine must forward the upstream address as the host (for Caddy, `header_up Host {upstream_hostport}`) and set `X-Forwarded-Host` to the public host (Caddy does by default). A reverse proxy on another machine, with `proxy_host` set to `"0.0.0.0"` or `"::"`, must preserve the public `Host` header. See "Reverse proxy requirements" and the updated Caddy example in the README.

* **The proxy now rejects HTTP request bodies larger than 100 MB, and all chunked request bodies,** with HTTP 413 before reading them. Previously the whole body was read into memory before being forwarded, so one very large upload could exhaust the server's memory and stop every app. Apps that accept larger uploads through `shiny.maxRequestSize` need the new `max_request_size_mb` option raised to match. The management server, whose requests need no body, rejects bodies larger than 64 KB.

* **App processes now start in their app directory and read the `.Renviron` and `.Rprofile` there,** as R does when the app is run on its own, so an app's own environment variables and profile, such as one that activates renv, now apply to it. As at R's startup, `R_ENVIRON_USER` and `R_PROFILE_USER` take precedence, including an `R_PROFILE_USER` that the app's own `.Renviron` sets, and an app directory without these files falls back to the user's `~/.Renviron` or `~/.Rprofile`. Previously every app read the `.Renviron` and `.Rprofile` of the directory that `start_tss()` was called from; an app that relied on those files needs its own copy. An error in an app's profile is written to its error log, and the app still starts. A `.First()` function that the profile defines runs after it, as at R's startup. The site profile, `Rprofile.site`, also runs in apps, and an app's own `.Renviron` that sets `R_LIBS` or `R_LIBS_USER` gives the app the library paths that R makes from them, even when another of its lines cannot be read; apps previously always had the server's library paths unless a profile changed them.

* **The proxy no longer forwards app responses larger than 1024 MB.** It stops receiving such a response and answers HTTP 502. The proxy sends nothing before it has a whole response and cannot tell when a visitor stops waiting, so a response that an app streams without end filled the disk and kept an on-demand app from stopping. Apps that serve larger downloads need the new `max_response_size_mb` option raised to match. A response that never ends but stays below the limit still keeps its request, and an on-demand app, in use.

* The proxy's public `/api/apps` endpoint now returns only each app's name, status, mode, and connection count. App paths, ports, and process IDs are available only from the loopback management API.

* On-demand apps now stop 30 seconds after their last WebSocket connection closes instead of immediately; after a large response, the wait also allows for a slow browser to receive it, at 128 KB per second for up to 10 more minutes, since the page's WebSocket opens only then. Reloading or navigating within an on-demand app previously stopped it between pages, forcing a cold restart that could make the new page's requests fail with HTTP 503.

## New features

* The management dashboard has a **Reload Config & Restart All** button, also available as `POST /api/reload`. It reads the configuration file again, stops every app, and starts the resident apps under the new configuration, so apps can be added, removed, or changed without restarting the server. An invalid file, a `log_dir` where the server log cannot be written, apps that cannot all be given a port because other programs hold the ports after `starting_port`, or a change to `proxy_host`, `proxy_port`, or `management_port` is rejected before any app stops. If an app cannot be stopped, the previous configuration stays in effect, and the app that is still running keeps its users connected and its state, such as the document that an R Markdown app serves. If anything else fails once the apps have stopped, the previous configuration is put back in effect as a whole and the resident apps start again at once. A changed `health_check_interval` applies at once. The dashboard reports the reload's outcome, which `/api/status` also returns as `last_reload`. The landing page picks up added and removed apps.

* New `base_path` option serves the landing page and apps under a URL prefix, such as `https://example.com/shiny/`, behind a reverse proxy. The proxy accepts requests with or without the prefix, so the reverse proxy may pass it through or strip it, and landing page links, redirects, and app cookie paths include it. The management dashboard now uses relative URLs, so it also works under a prefix that the reverse proxy strips.

* New `max_response_size_mb` option sets the largest app response that the proxy forwards (default: 1024); see "Breaking changes".

* New `idle_timeout_minutes` option sets how long a session may carry no WebSocket message before it is closed (default: 30, the limit that was fixed before). `0` keeps idle sessions open, for dashboards that people leave open without interacting.

* New `max_unanswered_mb` option (default: 256) closes a WebSocket session once the app has sent it that many megabytes without a message from the browser in between; `0` sets no limit. The proxy cannot tell how much of what it sends a browser has been read, and the app's traffic kept such a session from counting as idle, so one client that opened a session and stopped reading could make the server hold an app's output in memory without end, until it stopped every app. A dashboard left open without interaction that receives more than this is disconnected too, and needs a larger value. Each message from the browser starts the count again, so a client that keeps sending messages while it does not read is not limited.

* New `log_level` option chooses the least severe messages written to `server.log` (default: `"INFO"`). The package's DEBUG messages, which could not be turned on before, record each request's routing, the connection bookkeeping, and the first 100 bytes of every WebSocket message.

* New `title` option sets the name shown in the browser tab and top bar of the landing and management pages (default: `"Tiny Shiny Server"`).

* The landing page and management dashboard have a new design. They follow the system light or dark theme by default and have a theme toggle; an explicit choice is remembered in the browser until it is switched back to match the system, or, in a browser that blocks site data, for as long as the page is open. Browsers without the CSS `light-dark()` function, such as Safari before 17.5, Chrome before 123, and Firefox before 120, show the same colors. On narrow screens, where the connection status shows only its dot, screen readers still announce a lost connection.

* A browser that opens an app before it is ready, for example an on-demand app that is still starting, now receives a page that reloads itself until the app answers, for up to two minutes. It previously showed the proxy's JSON error until the visitor reloaded by hand. The response is still HTTP 503 with `Retry-After`, and scripts and other clients still receive JSON. A browser that blocks site data keeps count of the two minutes in the tab's `window.name`, unless something else uses it. The name holds the times of the wait and a hash of the page's address, not the address, as it stays on the app's page once the app answers, and on the pages that the tab shows after it.

* The management dashboard shows the server's uptime and memory use, and each running app's memory use and uptime. `/api/status` returns `uptime_seconds`, `server_memory_bytes`, and `apps_memory_bytes` in place of the `server_uptime` and `memory_usage` fields, which were always `"N/A"`; the management `/api/apps` returns `memory_bytes` and `uptime_seconds` for running apps. Memory is the resident memory of each R process and is left out where the `ps` package cannot read it.

* A Quarto document is rendered only when its rendered page may be out of date, instead of on every start, so on-demand Quarto apps and restarts no longer wait for a full render each time. The server records each render it makes in `{log_dir}/{app_name}_render.rds`, and renders again when the page is missing, when its last render did not finish, which can leave an incomplete page, or when a file in the app directory has been added, removed, or changed since that render, including one copied in with an older modification time, or when R, or a package that the page was rendered with, has another version or is no longer installed, as the page records them, or when the page's `{name}_files` directory, which holds its scripts and styles, is missing. Files that the document's own code writes into the app directory as it renders, such as a plot or data that it saves, do not count as changes, and neither do the intermediate files that a render stopped part-way leaves. A page that the server did not render, such as one deployed with the app or one that has changed since the server rendered it, is served while it is newer than every other file in the app directory. The CLI is the one that `QUARTO_PATH` names or that `PATH` finds in the app's environment. Delete the document's `.html` file to force a render. The document is rendered with the Quarto CLI and served with `rmarkdown::run()`, as `quarto::quarto_serve()` does, so the app's own R process does not need the quarto R package, which a renv project library for a Quarto document usually lacks. The R session that Quarto starts to render the document has the app's library paths. The check runs in the app's own process, so reading a large app directory does not hold up the other apps' traffic, and the app's error log records whether the document was rendered, and the render's output. What rendering writes beside the page, `{name}_files`, `{name}_data`, and `{name}_cache`, does not count as a change. When `log_dir` lies in the app directory, the server's log files and render records there do not count either, since they are written at every start. Neither do hidden files and directories, such as `.git`, `.Rproj.user`, and `.Rhistory`, other than a top-level `.Rprofile` or `.Renviron`, nor the packages that renv installs under `renv/`, since version control, the IDE, and renv change them without the document changing.

* Starting an app keeps the previous run's logs as `{app_name}_output.prev.log` and `{app_name}_error.prev.log`, so a crash traceback survives the automatic restart.

* An app whose own `.Renviron` sets `R_PROFILE_USER` is now set up as other apps are. The profile that the file named replaced the one in which callr finishes setting up an app's process, so an error that ended the app reached its error log only as `no item called "tools:callr"`, and R processes that the app started, among them the R session that renders a Quarto document, read callr's temporary startup files and got the server's library paths. The profile that the app's `.Renviron` names now runs in the app's process after callr's own, and in the R session that renders its Quarto document.

## Security

* The proxy and management server reject DNS-rebinding requests through the `Host` header checks above, so websites cannot read or control local apps through the proxy or use the management API. App processes still listen on their own loopback ports without these checks; see "Security model" in the README.

* The proxy rejects app WebSocket connections whose browser `Origin` does not match the request host, so other websites can no longer open app sessions with a visitor's cookies or reverse-proxy credentials. Behind a reverse proxy on the same machine that reports the public host in `X-Forwarded-Host`, the `Origin` must match that host: the `Host` header then names the proxy's own address, such as `127.0.0.1:3838`, where anything on a visitor's own machine could serve a page.

* Management responses send `X-Frame-Options: DENY` and `Content-Security-Policy: frame-ancestors 'none'` so the dashboard's restart controls cannot be clickjacked from a framing page.

* The proxy forwards HTTP requests and WebSocket sessions, with their cookies and credentials, only when the app's own process is listening on the app's port. Previously, another program holding a dormant on-demand app's port, such as a second server instance with the same `starting_port`, received that app's traffic.

* A request header spelled with an underscore no longer takes the place of the real header. R's web server reads `Content_Length` as `Content-Length` and `X_Forwarded_User` as `X-Forwarded-User`, and the underscore spelling wins. A client could therefore send a body of any size, chunked or not, past `max_request_size_mb` by also declaring a small `Content_Length`, and a user behind an authenticating reverse proxy could replace the identity header that the reverse proxy set. The proxy now reads the body size, the forwarding headers behind the client IP addresses it shows, and every other header that a decision depends on, such as `Host`, `Origin`, and `If-None-Match`, under the names the client sent. It rejects a chunked body whatever length is also declared, and it does not pass a header whose name contains an underscore on to an app.

* Client IP addresses shown in the management dashboard and logs are taken from `X-Forwarded-For` or `X-Real-IP` only when the request comes from a reverse proxy on the same machine, then from the last entry, which that proxy adds, and only when the value is an IP address, so that a forwarded value cannot write other text, such as terminal escape codes, into the log. The README now says that nginx must add the client's address with `proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;`: otherwise it passes on the address that the client chose.

* The README now says that `proxy_host: "::"` also listens on all IPv4 interfaces on most systems.

* App WebSocket sessions now receive the same request headers as proxied HTTP requests, so `session$request` includes the identity headers that an authenticating reverse proxy adds, such as `X-Forwarded-User`, as well as `User-Agent` and `X-Forwarded-For`. Previously only `Cookie` and `Authorization` were forwarded.

* Requests to apps no longer follow the `http_proxy` and `ALL_PROXY` environment variables. Where R was configured to use a proxy and `no_proxy` did not list `127.0.0.1`, app requests, with their cookies and bodies, were sent to that proxy, and every app answered HTTP 502 or 503.

* An unexpected error while answering a request is now answered with HTTP 500 and no detail, and the error is written to `server.log`. The client used to receive the R error message; on Windows, a request for `/templates/nul` returned the server's installation path that way, and now returns HTTP 404. An error reading a page's style sheet or script no longer names the file either. The management API's restart endpoint answers an unexpected error the same way; a restart that cannot start the app still reports why.

* HTTP 502 responses from the proxy no longer include the underlying connection error, which named the app's internal address and port. The error is written to `server.log`.

* The INFO message that records a WebSocket connection to an app no longer includes the connection's query string, which can carry a token. The full address is logged at DEBUG.

* An R Markdown or Quarto app serves only its entry document. `rmarkdown::run()`, which serves both, renders, and so runs the code of, any file under the app directory that a page or its WebSocket names, unless the document is prerendered, including partials whose names start with `_`, drafts in subdirectories, and R scripts, which it spins into reports. A request for another document's page is now refused with HTTP 404, and a WebSocket for any other file is closed. A WebSocket path that leaves the document's directory is refused without asking the file system about it, so that the refusal does not tell a client which files exist elsewhere on the server. The path is read as `rmarkdown` reads it, so a percent-encoded path, such as one ending in `%2Fwebsocket%2F`, names the same file. Such a request does not start an on-demand app, which nothing would then stop.

* When a reverse proxy reports with `X-Forwarded-Proto: https` that a WebSocket arrived over HTTPS, the proxy refuses it if the page that opens it comes from the plain-HTTP site of the same host. A page injected into unencrypted traffic could otherwise use the HTTPS site's sessions and credentials.

* A request or WebSocket handshake whose path or query string holds a `#` is refused with HTTP 400. Browsers keep a fragment to themselves, but the server passed one that a client did send on to the app without it, so the app received another path than the one that the proxy had checked: `/proxy/reports/_draft.Rmd#x` reached an R Markdown app as a request for the page of `_draft.Rmd`, which is otherwise answered with HTTP 404.

* No more than 64 requests with a body are received or awaited at a time, on the proxy and the management server together; further ones receive HTTP 503 with `Retry-After` from their headers, and the first is logged as a warning. R's web server keeps one of the R session's 128 connections open for each request body that it is receiving and, for a request to an app, until the app has answered. A client that began some 125 uploads and never finished them took them all, and until it left, pages' styles and scripts answered HTTP 500, on-demand apps could not start, the dashboard did not load, and nothing reached `server.log`. A request with a body that no app receives, one under another host's name, for an unknown path or app, or in a method that the proxy's own pages do not serve, is now refused from its headers, before its body, of up to `max_request_size_mb`, is received and written to disk.

* Request headers that the client names in its `Connection` header, and `Proxy-Connection`, are no longer passed on to apps, as response headers named in the app's `Connection` header already were not.

* The `Host` and `Origin` of a rejected WebSocket handshake are written to `server.log` with control characters replaced, including C1 controls such as U+009B and bytes that are not valid UTF-8, and cut to 200 bytes between characters, so a client cannot move the cursor of a terminal that follows the log or forge log lines.

* WebSocket handshakes with a query string longer than 8,192 characters are refused, as HTTP requests with one are.

* A cookie that an app sets without a `Path` is no longer given attributes from the request's path. Its default path, taken from the request path, ended at the first `;`, so a link to a crafted address such as `/proxy/app/x;Domain=example.com;Max-Age=31536000;y/page` could add a `Domain`, `SameSite`, or lifetime of its choosing to the cookie. The default path now stops at the last directory before any such character.

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

* An on-demand app whose process failed, for example because the app stops with an error at startup, now starts again only after `restart_delay`, as a resident app does. Until then, a request for it receives HTTP 503 with a `Retry-After` header that counts down the delay, and a browser receives the page that reloads itself. Every request used to start a new R process, so the page that reloads every 2 seconds, open in each visitor's tab, kept starting the failing app. This also applies to an app that crashes while in use. The management dashboard shows how long the app waits, and its Restart button, like `POST /api/apps/{name}/restart` and a configuration reload, ends the delay; the management `/api/apps` returns the wait as `restart_in_seconds`. A request that waits for an app that is starting is answered as soon as the app's process fails, with the time left of this delay, rather than after `appstart_timeout`, also when the request notices the failure before the server's own check does. An app that ends itself without an error, as one that calls `shiny::stopApp()` does, has not failed: `server.log` says that it exited rather than died, and an on-demand one starts again at the next request.

* An on-demand app whose process has exited is shown as **dormant**, which is what opening it does, until the health check cleans it up. It was shown as **crashed**, its landing page tile was disabled, and the tile advised a restart from the management dashboard, which only stops an on-demand app.

* A resident app whose process cannot be started, for example because its log files cannot be opened, is now reported and retried by health checks. It previously stopped the server at startup, and during a reload kept the remaining resident apps from starting and reported the reload as failed. An on-demand app that cannot start now returns HTTP 502 and is logged in `server.log`, instead of HTTP 500 with the internal error text.

* App processes are now supervised, so they stop instead of holding their ports if the R session running the server is killed or crashes.

* The server now opens the proxy and management ports before it starts any app. A second server started by mistake on the same ports stops with an error straight away, before it writes to the running server's log; it previously started its own app processes and renamed the running server's app logs to `.prev.log` first, and then wrote its own port assignments and shutdown to that log. A port that cannot be opened is reported where `start_tss()` runs, before anything is written to `server.log`.

* Shutdown and a configuration reload try once more to stop an app process that outlives the first attempt, and `server.log` names any app that is still running afterwards. It reported "All applications stopped" and "Server shutdown complete" regardless.

* A shutdown through the management API now stops only that server. It previously wrote `shutdown.flag` to `log_dir`, which also stopped any other server sharing that directory, and left the file behind.

* The answer to `POST /api/shutdown` now arrives whole before the server stops. The management server was stopped as soon as the request was handled, before it had sent the answer, so with `log_level` set to `"WARN"` or `"ERROR"` the client received only the answer's headers, `curl` reported an error, and the dashboard said that the shutdown had failed although the server stopped. Requests that arrive while the server shuts down are answered with HTTP 503, and requests that reached the server just before it stopped no longer run after `start_tss()` returns, where one could start an on-demand app that nothing then stopped.

* Ctrl-C now stops the server also when R notices it while the proxy or the management server handles a request or its headers, a WebSocket handshake, or a WebSocket message, or while the proxy receives an app's response, as it may while a request starts an app, an app streams updates, or a large download passes through. httpuv, the websocket package, and the curl package caught the interrupt there, and the server kept running.

* The server no longer sometimes takes a free port for one in use, which stopped `start_tss()` with an error that the port could not be opened, or gave an app another port than the one after its predecessor's. The check listened on the port briefly and then connected to it, and could reach its own listener, which httpuv closes a moment after it is stopped.

* An app whose `path` names a file, such as its `app.R`, rather than its directory, is reported as such. It was reported as a directory that does not exist.

* Stopping the server with Ctrl-C, or after an error in its event loop, no longer runs the shutdown twice, which repeated its log messages. A shutdown interrupted by a second Ctrl-C still runs again.

* An unexpected error that stops the server now ends `start_tss()` with an error once the servers and apps have stopped, so a server run with `Rscript` exits with a failure status that a service manager can restart it on. `start_tss()` used to return normally. The server object that `start_tss()` returns can no longer be started again, which left its servers and apps running after its second shutdown.

* An app's previous log is copied to `.prev.log` when it cannot be renamed, as on Windows when another program holds it open. Both the log and the older `.prev.log` were lost then.

* A system clock set back, as after a virtual machine snapshot is restored, no longer keeps an app starting, a failed on-demand app waiting, or a request waiting for an app, for as long as the clock was set back.

* `R_ENVIRON_USER` and `R_PROFILE_USER` set to an empty value now keep apps from reading a user environment file or profile, as they do for R itself. Apps read the app directory's files, or the user's, instead. On Windows, which cannot hold an empty environment variable, such a value counts as not set, as it does for R there.

* An app starts without the `RENV_PROJECT` variable of the server's session, unless the app's own environment file sets it. renv sets the variable in a session that has loaded a project, and the script that activates a project reads it before the working directory. Under a server that runs in a renv project, an app whose own profile activates renv would otherwise get the server's project library in place of its own.

* A line of an app's `.Renviron` that gives `R_PROFILE_USER`, `R_LIBS`, or `R_LIBS_USER` no value, such as `R_LIBS_USER=` or one whose value expands to nothing, sets nothing, as when R reads the file: the app keeps the profile and the library paths that it has without the line. `R_PROFILE_USER=""` still names no profile.

* An unused on-demand app whose process outlives the attempt to stop it is tried again after 30 seconds. It kept running until someone opened it and left it again.

* Apps can now be stopped and restarted on platforms where the `ps` package cannot list processes. Stopping an app failed there, which also blocked restarts, reloads, and shutdown; on those platforms only the app's own process is stopped, not processes it started.

* Assigning app ports no longer waits about a second for each app on Windows, which delayed startup and paused all apps' traffic during a configuration reload. A port now counts as in use when the server cannot listen on it, or, on Windows and macOS, when another program that listens on every interface accepts a connection on it within 100 milliseconds. Such a program did not keep an app from listening on `127.0.0.1` at its port, and then lost its loopback connections to the app. On Windows, where a connection to a free port waits for its timeout, this adds about a tenth of a second for each app that is not running to startup, and twice that to a reload, which looks for the apps' ports once before it stops any app and once when it assigns them.

* An on-demand app's finished HTTP requests no longer each leave a timer queued for 30 seconds; a newer idle check now replaces the pending one.

## Proxy

* Proxied HTTP requests are no longer cut off after 30 seconds. Once the app accepts the connection, a request fails only if the app sends no data for 10 minutes, so slow downloads and documents rendered on request complete.

* Uploads to apps are no longer limited to about 4 MB per second, and every proxied request is faster. A transfer now continues as soon as the app's socket is ready instead of on a 10-millisecond timer, and a request to a running app no longer opens a separate connection first to check that the app is listening.

* An app response larger than 8 MB is written to a temporary file as it arrives and sent from there, instead of being held in the server's memory, so a large download no longer occupies the memory of the process that serves every app. If R's temporary directory has been removed, for example by a cleaner of `/tmp` on a server that runs for weeks, it is created again. If the file cannot be written, for example because the disk is full, the request fails with HTTP 502, and `server.log` names the cause. The README notes that httpuv keeps a response that a client on a keep-alive connection stops downloading, in memory or as this file, until the server stops (rstudio/httpuv#448).

* Proxied responses keep their headers when the app sends a chunked response with trailer fields. The trailer fields took the place of the response's headers, such as its `Content-Type`, cookies, and redirect.

* Every `Path` attribute of an app's cookie is scoped to the app, including ones written with spaces around `=`, such as `Path =/`. Browsers use the last `Path`, so an app could scope a cookie to every app; `Domain` attributes for the loopback address are removed however they are spaced.

* When an app is too busy to complete a WebSocket handshake within the 5 seconds that the `websocket` package allows, as while another visitor's long calculation runs, the proxy tries again for up to 2 minutes, with the browser's messages queued. The browser's session was closed, and its page greyed out.

* Proxied responses are passed to the browser as the app's bytes. The proxy previously scanned responses with other content types, such as CSV, JSON, and ZIP downloads, for zero bytes and converted text to a string, briefly using about 11 times the response size in memory; text responses over 2 GB failed.

* Proxied HTTP requests and readiness checks now close their connections to the app when they finish. The connections previously stayed open until R ran garbage collection, so bursts of requests left many idle sockets open in both the server and the app. With libcurl older than 8, each readiness check also left a connection open.

* An upload that an app refuses from its headers, such as one above `shiny.maxRequestSize`, now returns the app's HTTP 413 response. It returned HTTP 502 when the client sent `Expect: 100-continue`, as curl does for large uploads. The proxy now offers request bodies over 64 KB to the app with `Expect: 100-continue` and sends them once the app agrees.

* The proxy now forwards `PATCH` requests to apps. It answered them with HTTP 405.

* Query strings may now be up to 8,192 characters (previously 2,048), so Shiny URL bookmarks with many inputs load.

* The proxy no longer rejects, with HTTP 400, query strings that contain a literal `%`, such as `?discount=50%&x=1`, or paths with a name that only starts or ends with two dots, such as `/..hidden/`. Paths with a `..` segment are still rejected.

* An app receives the request path as the client sent it, except that with libcurl 8.20.0 or later the hex digits of a percent-encoded character arrive in upper case, so `%2e` arrives as `%2E`. `.` and `..` segments were removed on the way, including their `%2e` spellings, so `/proxy/app/a/%2e%2e/c` reached the app as `/c`.

* A request body that the client sent without a `Content-Type` now reaches the app without one; the proxy labelled it `application/x-www-form-urlencoded`. A request with `Content-Length: 0` now reaches the app with that header and its `Content-Type`, which were both dropped.

* Proxied responses no longer carry two `Date` headers, and a response that an app sends without a `Content-Type`, such as HTTP 304, is no longer labelled `text/html`.

* Proxied responses keep their headers when one of them holds a byte that is not valid UTF-8, such as a Latin-1 file name in `Content-Disposition`; such bytes are passed on unchanged. Every header was dropped, so the browser received the body without its `Content-Type`, cookies, or redirect `Location`.

* Proxied responses that cannot have a body, HTTP 204 and 304 and the response to a `HEAD` request, are sent without one. For a client that accepts gzip, a compressed empty body followed them, which is not allowed and could make the client drop the connection.

* The proxy connects to an app's WebSocket as soon as the browser's WebSocket opens, instead of at the browser's first message. Sockets on which only the app speaks, such as Shiny's autoreload socket, now work.

* WebSocket messages larger than 32 MB from an app to the browser, such as large htmlwidget outputs, no longer disconnect the browser session, as long as the app has not sent more than `max_unanswered_mb` (default: 256 MB) since the browser's last message.

* A browser message that arrives just as the app closes its WebSocket ends the session without the browser being told that its message was invalid, and is logged as a warning rather than an error.

* A WebSocket to a path such as `/proxy//app/websocket/` is rejected, as the same HTTP path already was. It was connected to the app's default socket whatever path followed the app name.

* WebSocket messages for a client session that is no longer tracked no longer open an orphaned backend session.

* A WebSocket message no longer rewrites the server's records of every connected session, so busy sessions cost less as the number of sessions grows. Exited app processes are removed by the health check alone; the five-minute cleanup repeated that work.

* A WebSocket message that the proxy refuses, one larger than 1 MB or text that is not valid UTF-8, ends its session at once, with the app's socket, whether or not the browser answers the close. The session stayed open until the browser answered, so a client that did not answer kept it, and an on-demand app with it, running, and its later messages still reached the app.

* A text WebSocket message that is not valid UTF-8 closes its session whether or not the app's socket has opened yet. Sent while it was opening, such a message silently dropped the messages queued after it and left the session half started.

* A request header whose value is not valid UTF-8 in a field that the proxy reads, such as `Connection`, `Host`, `Origin`, or `If-None-Match`, is ignored or refused as an invalid value. It made the proxy answer HTTP 502, or HTTP 500 instead of 403 for `Host`.

* The proxy's own redirects, from `/proxy/{name}` to `/proxy/{name}/` and from `base_path` to `base_path/`, are now temporary (HTTP 307) instead of permanent (308); both keep the request's method and body. Browsers keep a permanent redirect, so after a configuration reload that changed `base_path`, they went on to the old prefix's address, which then answered HTTP 404.

* Browser messages that arrive while an app's WebSocket opens are no longer dropped when more than 100 are waiting; the session is closed instead, as it is when they reach 16 MB. Dropping the oldest threw away the message with which Shiny starts a session, so a browser that sent many messages to a busy app stayed connected to a session that never showed any output.

* At startup, the proxy and management ports count as in use when another program listens on 127.0.0.1 or `[::1]` at them, whatever `proxy_host` is, since a browser may open `localhost` at either address. The server started, and `http://localhost:3838` could then reach the other program. A port that cannot be opened for another reason, such as a port below 1024 without the right to use it, or `"::1"` on a machine without IPv6, is now reported as such instead of as already in use.

* A session is found idle by the time that has passed since its last message, which setting the system clock does not change. A clock set forward, as when a virtual machine's clock is corrected after a snapshot is restored, closed every open session at the next check, and a clock set back kept idle sessions open for as long. R has no clock that cannot be set, so the server keeps its own: it adds up the steps of R's elapsed time, counting a step back as none and a step of more than a minute as one minute. The time allowed for a slow browser to receive a large response, and the intervals at which repeated warnings are logged, are counted on the same clock. On Windows, R's elapsed time starts again from zero once a session has run for 49.7 days, which no longer keeps on-demand apps from stopping or sessions from being found idle.

* An app that sets a cookie whose name starts with `__Host-` is reported once in `server.log`. Browsers keep such a cookie only with `Path=/`, and the proxy gives app cookies the app's path, so that apps do not share them; the app lost the cookie, often one that holds its session or its forms' token, without notice. See "Security model" in the README.

* A request without an `Accept` or `User-Agent` header reaches the app without one. The proxy added `Accept: */*` and a `User-Agent` that named the server's R version and platform.

* Header names, host names, log levels, and the other words that the server compares are matched the same way in every locale. R changes letter case by the locale's rules, and under a Turkish locale, where R uses the system's rules as on Linux, the capital of `i` is a dotted letter, so that `connection` is no longer `CONNECTION`.

## Server pages and management API

* Every answer to a `HEAD` request that the proxy or the management server makes itself is sent without a body, such as the landing page, `/health`, the pages' style sheet, and the 503 for an app that is starting. The body was sent after the headers, so a client that sent another request on the same connection, as uptime monitors and HTTP libraries do, read it as the start of the next answer and failed.

* The proxy's own pages and endpoints (`/`, `/health`, `/api/apps`, and the files under `/templates/`) answer methods other than `GET` and `HEAD` with HTTP 405. They answered every method as if it were a `GET`. The management server's dashboard and `GET` endpoints also answer `HEAD` now, where they returned HTTP 404, and its pages and actions answer a method they do not serve with HTTP 405 instead of 404.

* A request with a method that neither server supports on any page, such as `PROPFIND`, receives HTTP 501 (Not Implemented) instead of a 405 without the `Allow` header that a 405 requires. Method names are matched exactly, as HTTP defines them, so `get` is not taken for `GET`.

* `POST /api/apps/{name}/restart` returns HTTP 404 instead of 500 for an app name that is not configured, and accepts only that path. A path with extra segments, such as `/api/apps/a/b/restart`, restarted app `a`; it now returns HTTP 400.

* `POST /api/apps/{name}/restart` for an on-demand app that is not running answers `"success": true` and says that the app starts when it is next opened, which is what a restart is for. It answered `"success": false`, which the dashboard showed as an error when its Restart button was used on an app that had stopped since the page last refreshed.

* The management API's `/api/apps` leaves out `pid` for an app that is not running. It returned `"pid": {}`, which the dashboard showed as "[object Object]"; it now shows "N/A".

* Management dashboard actions report their results in an inline notice instead of browser alert dialogs. When restarting an app or shutting down fails with an error, the notice shows the server's reason instead of "undefined", and a failed restart's notice names the app once instead of repeating itself.

* Restarting an app refreshes the dashboard's status tiles and connection list at once. They previously kept their old values, such as the number of running apps, for up to 5 seconds.

* The dashboard updates its app cards and connection list in place when it refreshes, instead of rebuilding them every 5 seconds, so the keyboard focus stays on a Restart button, a click that spans a refresh still lands, and selected text, such as a client's IP address, stays selected.

* The management dashboard works better from the keyboard and with screen readers. A button keeps the keyboard focus while its restart, reload, or shutdown runs, instead of losing it to the page; each Restart button names its app, as in "Restart sales", and app names are headings. The focus outline also shows in forced-colors modes such as Windows High Contrast, which removed the focus ring, and small labels and the Shutdown button's hover colors meet the WCAG AA contrast ratio. A notice that reports an error stays until it is dismissed or replaced, instead of disappearing after 6 seconds, and screen readers announce the first notice too.

* The page shown while an app starts stops reloading after two minutes in every case. A reload continued the same wait only if it came soon enough after the page before it, judged by this page's own retry time, so a page that waited out a long `restart_delay`, a slow answer, or a browser that delays the timers of background tabs began the two minutes again, and a tab left open restarted a failing app forever. A page that waits out the restart delay of an app that failed, opened before the app can start again, says so, and counts its two minutes from the end of that delay, when the app starts. A page continues a wait only if its own navigation began when the page before it reloaded: a page that the visitor opens after the reload reached the app, as when the app stops again soon after, starts a new wait.

* An open landing page reloads itself when a configuration reload changes `base_path`, including from no prefix to one, which the status requests, answered with and without the prefix, did not show: `/api/apps` now names the current prefix in an `X-TinyShinyServer-Base-Path` header. Its status requests went to an address that no longer exists, and it showed every app as "server down". The page reloads for this at most once a minute, so a page whose status address is not found for another reason, such as a reverse proxy that does not pass it on, shows the server as unreachable rather than reloading without pause. A browser that blocks site data keeps the time of that reload in the tab's `window.name`, as the page shown while an app starts does, unless something else uses it.

* The landing page keeps its status current after the visitor returns to it with Back. A browser that keeps the page in its back-forward cache, as Chrome does, showed it again without the timer that refreshes it, so app statuses, connection counts, and the connection indicator stopped changing.

* `/api/connections` on the management server returns `{}` when there are no sessions, an object as when there are some; it returned an empty array. A session's user agent is shown with control characters and bytes that are not valid UTF-8 replaced, and cut to 1 KB, so that the JSON is valid UTF-8, and its `duration_seconds` is 0 rather than negative after the system clock is set back.

* The management server closes any WebSocket connection at once. It accepted a handshake whatever its `Host` or `Origin`, and wrote an error to the console for each one.

* The top bar of the landing page and the dashboard has a background in browsers without the CSS `color-mix()` function, such as Safari before 16.2, Chrome before 111, and Firefox before 113, where it was transparent.

* Status badges meet the WCAG AA contrast ratio in both themes: green and amber are a little darker in the light theme, and red is a little lighter in the dark theme.

* The keyboard focus moves to an app's name on the management dashboard when the Restart button that holds it is removed, as when a restarted on-demand app becomes dormant, and to the heading of the screen that a shutdown shows, which screen readers then read. It dropped to the page itself.

* The R version and operating system are now shown on the management dashboard instead of the public landing page, and are read once instead of on every page load.

* The pages' style sheet and script are sent with `Cache-Control` and `ETag` headers, so browsers reuse them instead of fetching them again for every page, including each reload of the page shown while an app starts. The tag is weak and the responses carry `Vary: Accept-Encoding`, since R's web server compresses them for clients that accept gzip, so a shared cache does not give the compressed file to a client that did not ask for it.

* Files under `/templates/` are sent as their bytes. Files other than images were read as lines of text, which changed line endings and dropped a final newline, and would have corrupted fonts and other binary files; web fonts and WebP images are now sent with their own types, and unknown types as `application/octet-stream` instead of `text/plain`.

* Requests for a directory under `/templates/` return HTTP 404 instead of 500.

## Configuration

* Configuration fields that the server does not recognize, such as a misspelled `resident`, are now reported as a warning in `server.log` at startup, and in the dashboard's notice after a reload. They were ignored without notice. They are now also removed from the configuration: a field whose name begins with an option's name, such as `proxy_port_old` or `title_draft`, no longer sets that option when the option itself is absent, as it did, without validation, because R matches list names by prefix.

* Configuration validation now rejects an empty `log_dir`, which put the logs in the filesystem root, an app `resident` value of `null`, which stopped the server at startup, and app names that differ only in letter case, which shared log files on Windows and macOS.

* `base_path` values ending in `//`, such as `"/shiny//"`, are rejected. They were accepted with a trailing slash, which made requests under the prefix return HTTP 404.

* `start_tss()` reports a `config` argument that is not a single file path, instead of failing with an unrelated error.

* `restart_delay`, `health_check_interval`, and `appstart_timeout` may be at most 86400 seconds (one day). A value of about 1e12 or more, such as a large `restart_delay` meant as "never", was accepted, and then made the server stop answering requests.

* A configuration file that gives a field twice, at the top level or in one app, is rejected. The first copy was used without notice, where most JSON readers use the last, and the other copy was not validated.

* A configuration reload checks that the server log can be written in `log_dir` even when `log_dir` has not changed, and stops no app if it cannot. Such a reload stopped every app, and resident apps could then not open their log files. When the server log cannot be moved to a changed `log_dir`, a changed `log_level` is now applied anyway, and the reload's outcome says that the previous `log_dir` was kept, for the app logs and render records as well as the server log.

* A configuration file that is not UTF-8 text, such as one that Windows PowerShell 5.1 saved as UTF-16 or one with a Latin-1 character, is reported as such. A small file failed with "file name conversion problem", and a file holding only the name of another file, or a URL, loaded that instead.

* A configuration file that holds a NUL character, as a byte or as `\u0000` in a string, is rejected. It was read only up to the character: the rest of a line after a NUL byte was dropped, and a string ended at `\u0000`, so that `"apps/a\u0000x"` named the directory `apps/a`.

* A reload reads the configuration file through the path that it was loaded from, without resolving links, so it reads a file that a link has since been pointed at, such as a mounted Kubernetes ConfigMap or a `current` release link. It read the previous target, and failed once that was removed.

* An app whose `path` is empty or blank is rejected when the configuration is read. It was accepted, and failed at every start with "App directory not found: " and no path.

* A configuration file that is empty, is not valid JSON, or is a directory is reported with its name and what is wrong with it. Only the JSON parser's message, such as "premature EOF", was shown for the first two, and "Permission denied" for a directory.

* A configuration file that holds a `\u` escape of half a UTF-16 surrogate pair, which is no character, is rejected. A first half alone was read as "?", which changed a path or title without notice, and a second half alone stopped the server with an error that named neither the file nor the field.

* A setting given as an array inside an array, such as `"title": [["Reports"]]`, is rejected when the configuration is read. It passed for a single value, stopped the server at startup with an unrelated error, and failed a reload only after the apps had stopped.

## Logging

* App processes now write their stdout and stderr directly to the per-app log files. Output from child processes and native code (for example, Pandoc) is now logged and can no longer fill an unread pipe and hang the app.

* `start_tss()` now configures logging only for the package's own logger namespace, so the caller's `logger` threshold and appender are no longer changed for the rest of the R session. Package log messages are formatted correctly even when the session's global `logger` formatter is not glue.

* If `server.log` can no longer be written while the server runs, messages still go to the console. Previously the write error stopped health checks and interrupted shutdown, leaving the proxy, management server, and apps running. If `log_dir` is removed, starting an app creates it again, so apps can still start and `server.log` resumes. A `log_dir` where `server.log` cannot be written at startup is reported as such.

* `server.log` records the close of a WebSocket session once. A session that the proxy closed was recorded twice, and one that was never opened was recorded as closed. Sessions closed because their app restarted, crashed, or stopped at shutdown were not recorded.

* Port assignments are now written to `server.log`, not only to the console.

* A request for an on-demand app whose directory cannot be served no longer starts the app, reads its directory again, and writes four lines, two of them errors, to `server.log`. The reason is logged as a warning at most once a minute for each app. A page in any visitor's browser, on any website, could otherwise fill the log with requests for such an app. Requests for an on-demand app that waits out `restart_delay` after a failure are likewise logged once a minute and not once each, and a request that finds such an app's process gone logs that the app died, as a health check does.

* A refused WebSocket handshake is logged at WARN at most once every 10 seconds for each reason, with the number of others since then, which are logged at DEBUG, and handshakes are no longer each logged at INFO before they are checked. A handshake for an app that cannot start, that is still starting, or whose process is not listening on its port is refused the same way, for each app: such handshakes wrote an error, or a warning that named a port conflict and two more lines, at every attempt. A page that opened handshakes in a loop, from any visitor's browser, filled `server.log` with two lines for each. The log of a browser message that is too large or not valid UTF-8 names its app and session, and WebSocket messages in DEBUG logs have control characters replaced, as other logged request values do.

* Errors on the server's WebSocket connections to apps are written to `server.log` only. The websocket package also printed each to the console, from its own thread, as when an app restarted or stopped with sessions open. A handshake that a busy app did not complete in time, and that the server tries again, is logged at INFO and no longer as an error, and an error on a connection that its session no longer uses is logged at DEBUG.

## Examples

* The dashboard example's data cover fixed dates. Its date range was taken from the day the page was rendered and its data from the day the app started, so once its rendered page was reused, the newest data could not be selected, and after three months the dashboard showed no data at all.

* The inventory example app accepts at most 10,000 items, so a visitor can no longer make it allocate unbounded memory.

* The reports example shows its charts and table in two columns of one page. It showed two pages, both named "Column", with the table on the second. The dashboard example's cards show their titles, which Quarto dropped.

## Package

* The package now requires R 4.1.0 or later, as its `promises` and `quarto` dependencies already do.

* The package now requires `httpuv` 1.5.5 or later, the first version that sends a response without a body, as the proxy sends answers to `HEAD` requests and 204 and 304 responses. It also requires `later` 1.4.0 (for `later_fd()`, which the faster transfers use), `curl` 3.3 (for `file_writer()`, which writes large responses to disk), `websocket` 1.1.0 (whose client accepts the `maxMessageSize` that large app messages need), `ps` 1.7.0 (for `ps_connections()`, which identifies the process listening on an app's port, and for `ps_memory_info()`, which before 1.7.0 misreported memory on Linux), `callr` 3.7.0 (which passes the working directory on, so that apps start in their own directory, restores in an app's process the library and startup-file variables that it changes, and accepts the `R_ENVIRON_USER` that names the environment file an app's process reads), `jsonlite` 1.6 (for `parse_json()`, which reads the configuration file's text and never takes it for the name of a file or a URL), and `logger` 0.2.0 (the first that accepts `log_level` as a name).

* Removed the unused `future` dependency, and `digest` and `openssl`: session IDs only key the server's own connection tables and are never read from a request, so they are now numbered in order instead of hashed from random bits and request details. `tools` and `utils` are now declared imports, as is `glue`, which `logger` needs to format the package's log messages. Added `ps`, already required by `callr`.

* `roxygen2` and `devtools` are no longer suggested packages: no test or example uses them.

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
