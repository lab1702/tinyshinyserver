## Submission

Update to version 0.3.0. Version 0.2.1 was submitted on 2026-09-24; the CRAN
package index checked on 2026-09-24 (UTC) still lists 0.2.0.

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
* Proxied HTTP requests are no longer cut off after 30 seconds, and query
  strings may be up to 8,192 characters.
* `start_tss()` configures logging only in the package's own logger
  namespace, leaving the caller's logger settings unchanged. Port assignments
  are written to `server.log`.
* The package now requires R >= 4.1.0, as its `promises` and `quarto`
  dependencies already do.
* Removed the unused `future` dependency; `tools` and `utils` are now declared
  imports. Added `ps`, already required by `callr`.

## Test environments

* Local: Windows 11 x64 (build 28000), x86_64-w64-mingw32,
  R 4.6.1 (2026-06-24 ucrt).
  Checked with `R CMD check --as-cran` on the source tarball, including the
  PDF manual, with remote incoming checks disabled and OpenMP thread limit set
  to one.
* R-hub (commit 6033be6, run
  https://github.com/lab1702/tinyshinyserver/actions/runs/36076979816):
  linux (R-devel), windows (R-devel), macos-arm64 (R-devel), gcc16, and
  ubuntu-gcc12.

## R CMD check results

Local: 0 errors | 0 warnings | 0 notes.

R-hub: 0 errors | 0 warnings | 0 notes on all five platforms.

Test results: 1480 passes locally and 1479 or 1480 on R-hub, with 0 failures,
0 warnings, and 2 intentional skips on CRAN (the invalid-device-path write test
and a test that launches a real Shiny app). One test's expectation count
depends on timing.

## Downstream dependencies

The CRAN package index checked on 2026-09-24 (UTC) lists no reverse dependencies.
