## Submission

This is an update to version 0.3.0. The current CRAN version is 0.2.1,
published on 2026-09-24 (CRAN package index checked on 2026-11-22).

## CRAN check problems in 0.2.1

The CRAN check results for 0.2.1 showed an ERROR on
r-devel-linux-x86_64-debian-clang (as seen on 2026-11-20), and the noLD
additional check still shows one (its output is from 2026-11-17). On
2026-11-22, all 13 CRAN check flavors report OK for 0.2.1, including
r-devel-linux-x86_64-debian-clang. Both errors are the same failure of one
test in `tests/testthat/test-utils.R`, "is_port_in_use detects open and
closed ports without warnings". The test stopped an httpuv server and
expected its port to be closed straight away, but `httpuv::stopServer()`
closes the listening socket on a background thread after it returns, so the
port can still accept a connection for a moment. This was a timing fault in
the test, not in the package, and it is fixed in this release: the test now
waits up to 5 seconds for the port to close.

## Changes in this release

This release hardens the proxy and management server for local and
reverse-proxy deployments and fixes app lifecycle and logging problems. It
includes changes that require reverse-proxy configuration updates, which are
documented in NEWS.md and README.md.

* The management server, and the proxy when bound to a loopback address,
  reject requests whose `Host` header is not a loopback name (DNS-rebinding
  protection; app processes' own loopback ports are not covered). App
  WebSocket connections must come from a page on the same host, and
  management pages cannot be framed.
* The proxy rejects request bodies larger than the new `max_request_size_mb`
  option (default 100 MB), and chunked request bodies, with HTTP 413 before
  reading them, instead of buffering them in memory.
* Request headers are read under the names the client sent. R's web server
  stores a header spelled with an underscore, such as `Content_Length` or
  `X_Forwarded_User`, in place of the real one, which let a client understate
  a body's size to pass that limit, or replace the identity header set by an
  authenticating reverse proxy. Headers whose names contain an underscore are
  not passed on to apps.
* The public `/api/apps` endpoint no longer exposes app paths, ports, or
  process IDs.
* An unexpected error while answering a request is answered with HTTP 500
  and no detail, and is written to `server.log`. The client used to receive
  the R error message; on Windows, a request for `/templates/nul` returned
  the server's installation path that way, and now gets HTTP 404. The
  management API's restart endpoint answers an unexpected error the same
  way.
* The INFO message for an app's WebSocket connection no longer includes the
  connection's query string, which can carry a token.
* App processes write stdout and stderr directly to their log files instead
  of unread pipes that could fill and hang the app. The previous run's logs
  are kept.
* App processes start in their app directory and read the `.Renviron` and
  `.Rprofile` there, as R does when the app is run on its own, so an app's
  own environment variables and profile, such as one that activates renv,
  apply to it; without them, the user's `~/.Renviron` and `~/.Rprofile`
  apply. Every app read the `.Renviron` and `.Rprofile` of the directory
  that `start_tss()` was called from instead.
* On-demand apps stop 30 seconds after their last WebSocket closes, so
  reloading a page no longer kills and cold-starts the app.
* The proxy forwards requests and WebSocket sessions only when the app's own
  process is listening on the app's port, so another program holding that
  port never receives users' cookies or credentials.
* App WebSocket sessions receive the same request headers as proxied HTTP
  requests, including the identity headers an authenticating reverse proxy
  adds. Previously only `Cookie` and `Authorization` were forwarded.
* WebSocket messages larger than 32 MB from an app to the browser no longer
  close the browser session.
* Proxied HTTP requests are no longer cut off after 30 seconds. Query
  strings may be up to 8,192 characters and may contain a literal `%`.
* Requests from the proxy to apps no longer follow the `http_proxy` and
  `ALL_PROXY` environment variables, which sent app traffic to a configured
  proxy and left every app unreachable.
* Transfers between the proxy and apps wait on their sockets instead of
  polling on a timer, which limited uploads to about 4 MB per second, and a
  request to a running app no longer opens a separate connection first. An
  upload that an app refuses from its headers returns the app's response.
* A port is tested for use by listening on it briefly instead of connecting
  to it, which took about a second for each app on Windows.
* A browser that opens an app before it is ready receives a page that
  reloads itself until the app answers, for up to two minutes, instead of the
  proxy's JSON error. The response status and headers are unchanged, and
  other clients still receive JSON.
* Stopping the server with Ctrl-C no longer runs the shutdown twice.
* The server opens the proxy and management ports before it starts any app,
  so a second server started by mistake on the same ports stops with an
  error instead of first starting its own apps and renaming the running
  server's app logs.
* Configuration fields that the server does not recognize, such as a
  misspelled option name, are reported as a warning in `server.log` and in
  the outcome of a reload instead of being ignored without notice.
* A configuration field whose name begins with an option's name, such as
  `proxy_port_old`, no longer sets that option: R matched it by prefix when
  the option itself was absent. Unrecognized fields are now removed from the
  configuration as well as reported.
* Proxied responses no longer carry two `Date` headers, and a response that
  an app sends without a `Content-Type` is no longer labelled `text/html`.
  A response keeps its headers when one of them holds a byte that is not
  valid UTF-8, such as a Latin-1 file name; every header was dropped.
* A request body that the client sent without a `Content-Type` reaches the
  app without one; the proxy labelled it
  `application/x-www-form-urlencoded`. A request with `Content-Length: 0`
  keeps that header and its `Content-Type`, which were both dropped.
* App responses larger than 8 MB are written to a temporary file as they
  arrive and sent from there, instead of being held in the server's memory.
  Responses that cannot have a body (HTTP 204 and 304, and the answer to a
  `HEAD` request) are sent without one; for clients that accept gzip, a
  compressed empty body followed them.
* Every answer to a `HEAD` request that the proxy or the management server
  makes itself, such as the landing page, `/health`, or the 503 for an app
  that is starting, is sent without a body. The body followed the headers,
  so a client that sent another request on the same connection read it as
  the start of the next answer.
* The proxy forwards `PATCH` requests, which it answered with HTTP 405.
* An app receives the request path as the client sent it. `.` and `..`
  segments, including their `%2e` spellings, were removed on the way.
* The proxy's own pages and endpoints answer methods other than `GET` and
  `HEAD` with HTTP 405; they answered every method as a `GET`. A method that
  neither server supports on any page, such as `PROPFIND`, gets HTTP 501
  instead of a 405 without the `Allow` header that a 405 requires. The
  management dashboard and its `GET` endpoints answer `HEAD`, which returned
  HTTP 404, and a management page or action asked with a method it does not
  serve gets HTTP 405 instead of 404. The restart endpoint accepts only
  `/api/apps/{name}/restart`; a path with extra segments restarted an app.
* Shutdown and a configuration reload try once more to stop an app process
  that outlives the first attempt, and `server.log` names any app that is
  still running; it reported that every app had stopped. The log records the
  close of a WebSocket session once instead of twice.
* The HTTP 502 answer "App startup timed out" goes to the first request
  after an app's startup window, and only if the app still does not answer;
  whether it was sent depended on which request or check came first.
* Apps can be stopped on platforms where the `ps` package cannot list
  processes; stopping an app failed there.
* An app whose directory is missing, or holds no app entry point, is no
  longer started. A resident app with a mistyped path used to start a new R
  process that exited, every 10 to 15 seconds; the reason is now written to
  `server.log`. An on-demand app in that state has the new status
  "unavailable" instead of "dormant", and its landing page tile is disabled;
  opening it could only fail with HTTP 502. That status is looked up again
  when the directory changes, and at least every 30 seconds, instead of for
  every status request from every open landing page.
* A directory with several documents serves the one that `rmarkdown` picks.
  Partials, whose names start with `_`, are never served, and an `index` or
  `ui` document comes first, then one whose front matter asks for a Shiny
  runtime; the first document in alphabetical order was served, which could
  be a partial or a static page. Document extensions may be in any letter
  case, such as `report.rmd`, as `rmarkdown` and Quarto accept them.
* A restart requested from the dashboard or API no longer waits for
  `restart_delay`, which applies to restarts after a failure. Restarting an
  on-demand app stops it until it is next opened.
* The proxy connects to an app's WebSocket when the browser's socket opens
  instead of at the browser's first message, so sockets on which only the
  app speaks work. A WebSocket path is checked in the same way as an HTTP
  path.
* A Quarto document is rendered only when its rendered page is missing or
  older than another file in the app directory, instead of on every start.
  The app's own process makes that check, so reading a large app directory
  does not hold up the other apps' traffic. When `log_dir` lies in the app
  directory, the server's log files there do not count as changes. The
  example dashboard's data now cover fixed dates, since its rendered date
  range drifted away from data generated at every start.
* A configuration in which two apps serve the same Quarto document, or the
  same prerendered R Markdown document, is rejected when it is loaded or
  reloaded. Such documents are rendered into their own directory, and two
  renders at once could leave a broken page, which was then served at every
  start.
* An on-demand app that is still starting is no longer stopped as unused 30
  seconds after the last request. It is left to finish, for up to 10 minutes
  from its start. A start slower than the two minutes for which the
  visitor's page reloads, such as a Quarto render, used to be thrown away
  and begun again on the next visit. Readiness checks keep watching an app
  whose process is still running after the startup window, for up to 10
  minutes, and log it as ready when it answers instead of logging that it
  failed to become ready.
* New `idle_timeout_minutes` and `log_level` options set how long a session
  may carry no WebSocket message before it is closed (30 minutes by
  default, as before; 0 keeps idle sessions open) and the threshold of the
  server log.
* `base_path` values ending in `//` are rejected instead of being accepted
  with a trailing slash, and `start_tss()` reports a `config` argument that
  is not a single file path.
* The management dashboard has a "Reload Config & Restart All" button, also
  available as `POST /api/reload`, that reads the configuration file again
  and restarts all apps without restarting the server. Like the other
  management actions, it requires the custom request header that blocks
  cross-origin browser requests.
* A new `base_path` option serves the landing page and apps under a URL
  prefix on an existing site behind a reverse proxy.
* The management dashboard shows the server's and each running app's uptime
  and memory use. `/api/status` returns them in place of the `server_uptime`
  and `memory_usage` fields, which were always "N/A". The management
  `/api/apps` leaves out `pid` for an app that is not running; it returned
  `"pid": {}`.
* The landing page and management dashboard have a new design that follows
  the system light or dark theme and has a theme toggle. A new `title`
  option sets the name shown on both pages.
* The pages' style sheet and script are sent with `Cache-Control` and `ETag`
  headers, so browsers reuse them instead of fetching them again for every
  page. Files under `/templates/` are sent as their bytes; files other than
  images were read as lines of text, which changed their line endings and
  would have corrupted fonts.
* When restarting an app or shutting down fails, the management dashboard
  shows the server's reason instead of "undefined".
* `start_tss()` configures logging only in the package's own logger
  namespace, leaving the caller's logger settings unchanged. Port assignments
  are written to `server.log`.
* The package now requires R >= 4.1.0, as its `promises` and `quarto`
  dependencies already do.
* Removed the unused `future` dependency, and `digest`, which only hashed
  session IDs that are already random. `tools` and `utils` are now declared
  imports, as is `glue`, which `logger` needs to format the package's log
  messages. Added `ps`, which `callr` already requires. The package now
  requires `websocket` >= 1.1.0, the first version whose client accepts a
  `maxMessageSize` argument, `later` >= 1.4.0, the first version with
  `later_fd()`, `curl` >= 3.3, the first version with `file_writer()`,
  `httpuv` >= 1.5.2, the first version whose `startServer()` has the `quiet`
  argument, and `ps` >= 1.2.0, the first version with `ps_connections()`.

## Test environments

* Local: Windows 11 x64 (build 26300), x86_64-w64-mingw32,
  R 4.6.2 (2026-10-29 ucrt).
  Checked with `R CMD check --as-cran` on the source tarball built from
  commit 23d2697, including the PDF manual and the CRAN incoming
  feasibility check.
* R-hub (commit 23d2697, run
  https://github.com/lab1702/tinyshinyserver/actions/runs/45071314610):
  linux (R-devel), windows (R-devel), macos-arm64 (R-devel), gcc16, and
  ubuntu-gcc12.

The commit recording these results changes only this file, which is excluded
from the build, so the package checked is identical.

## R CMD check results

Local: 0 errors | 0 warnings | 0 notes.

R-hub at commit 23d2697: 0 errors | 0 warnings | 0 notes on all five platforms.

Test results: 2311 passes locally and on all five R-hub platforms, with 9
intentional skips on CRAN (a timing check of the port test and eight tests
that start real R processes). All runs had 0 failures and 0 warnings. One
test's expectation count depends on timing.

## Downstream dependencies

The CRAN package index checked on 2026-11-22 (UTC) lists no reverse dependencies.
