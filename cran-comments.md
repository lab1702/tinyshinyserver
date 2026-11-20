## Submission

This is an update to version 0.3.0. The current CRAN version is 0.2.1,
published on 2026-09-24 (CRAN package index checked on 2026-11-20).

## CRAN check problems in 0.2.1

The CRAN check results for 0.2.1 show an ERROR on
r-devel-linux-x86_64-debian-clang and in the noLD additional check (as seen
on 2026-11-20). Both are the same failure of one test in
`tests/testthat/test-utils.R`, "is_port_in_use detects open and closed ports
without warnings". The test stopped an httpuv server and expected its port to
be closed straight away, but `httpuv::stopServer()` closes the listening
socket on a background thread after it returns, so the port can still accept
a connection for a moment. This was a timing fault in the test, not in the
package, and it is fixed in this release: the test now waits up to 5 seconds
for the port to close.

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
* The public `/api/apps` endpoint no longer exposes app paths, ports, or
  process IDs.
* App processes write stdout and stderr directly to their log files instead
  of unread pipes that could fill and hang the app. The previous run's logs
  are kept.
* On-demand apps stop 30 seconds after their last WebSocket closes, so
  reloading a page no longer kills and cold-starts the app.
* The proxy forwards requests and WebSocket sessions only when the app's own
  process is listening on the app's port, so another program holding that
  port never receives users' cookies or credentials.
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
* The management dashboard has a "Reload Config & Restart All" button, also
  available as `POST /api/reload`, that reads the configuration file again
  and restarts all apps without restarting the server. Like the other
  management actions, it requires the custom request header that blocks
  cross-origin browser requests.
* A new `base_path` option serves the landing page and apps under a URL
  prefix on an existing site behind a reverse proxy.
* The management dashboard shows the server's and each running app's uptime
  and memory use. `/api/status` returns them in place of the `server_uptime`
  and `memory_usage` fields, which were always "N/A".
* The landing page and management dashboard have a new design that follows
  the system light or dark theme and has a theme toggle. A new `title`
  option sets the name shown on both pages.
* `start_tss()` configures logging only in the package's own logger
  namespace, leaving the caller's logger settings unchanged. Port assignments
  are written to `server.log`.
* The package now requires R >= 4.1.0, as its `promises` and `quarto`
  dependencies already do.
* Removed the unused `future` dependency; `tools` and `utils` are now declared
  imports, as is `glue`, which `logger` needs to format the package's log
  messages. Added `ps`, which `callr` already requires. The package now
  requires `websocket` >= 1.1.0, the first version whose client accepts a
  `maxMessageSize` argument, and `later` >= 1.4.0, the first version with
  `later_fd()`.

## Test environments

* Local: Windows 11 x64 (build 26300), x86_64-w64-mingw32,
  R 4.6.2 (2026-10-29 ucrt).
  Checked with `R CMD check --as-cran` on the source tarball built from
  commit a7d9069, including the PDF manual and the CRAN incoming
  feasibility check.
* R-hub (commit a7d9069, run
  https://github.com/lab1702/tinyshinyserver/actions/runs/44785636730):
  linux (R-devel), windows (R-devel), macos-arm64 (R-devel), gcc16, and
  ubuntu-gcc12.

The commit recording these results changes only this file, which is excluded
from the build, so the package checked is identical.

## R CMD check results

Local: 0 errors | 0 warnings | 0 notes.

R-hub at commit a7d9069: 0 errors | 0 warnings | 0 notes on all five platforms.

Test results: 1810 passes locally and on all five R-hub platforms, with 4
intentional skips on CRAN (the invalid-device-path write test, a timing check
of the port test, and two tests that start real R processes). All runs had
0 failures and 0 warnings. One test's expectation count depends on timing.

## Downstream dependencies

The CRAN package index checked on 2026-09-24 (UTC) lists no reverse dependencies.
