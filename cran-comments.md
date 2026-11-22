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
reverse-proxy deployments, adds configuration options, and fixes app
lifecycle, proxy, and logging problems. NEWS.md lists every change. Those
that can affect existing users are:

* The management server, and the proxy when bound to a loopback address,
  reject requests whose `Host` header is not a loopback name (DNS-rebinding
  protection). Reverse proxies on the same machine must now forward the
  upstream address as the host; README.md documents the requirements.
* The proxy rejects request bodies larger than the new `max_request_size_mb`
  option (default 100 MB), and chunked request bodies, with HTTP 413 before
  reading them, instead of buffering them in memory.
* App processes start in their app directory and read the `.Renviron` and
  `.Rprofile` there, as R does when the app is run on its own, instead of
  those of the directory that `start_tss()` was called from.
* The public `/api/apps` endpoint no longer exposes app paths, ports, or
  process IDs.
* On-demand apps stop 30 seconds after their last WebSocket closes instead
  of immediately, so reloading a page no longer cold-starts the app.
* The package now requires R >= 4.1.0, as its `promises` and `quarto`
  dependencies already do, and newer minimum versions of `httpuv` (1.5.2),
  `later` (1.4.0), `curl` (3.3), `websocket` (1.1.0), and `ps` (1.2.0) for
  functions it now uses. It no longer imports `future`, `digest`, or
  `openssl`.

## Test environments

* Local: Windows 11 x64 (build 26300), x86_64-w64-mingw32,
  R 4.6.2 (2026-10-29 ucrt).
  Checked with `R CMD check --as-cran` on the source tarball built from
  commit b6aecc1, including the PDF manual and the CRAN incoming
  feasibility check.
* R-hub (commit b6aecc1, run
  https://github.com/lab1702/tinyshinyserver/actions/runs/45074000512):
  linux (R-devel), windows (R-devel), macos-arm64 (R-devel), gcc16, and
  ubuntu-gcc12.

The commit recording these results changes only this file, which is excluded
from the build, so the package checked is identical.

## R CMD check results

Local: 0 errors | 0 warnings | 0 notes.

R-hub at commit b6aecc1: 0 errors | 0 warnings | 0 notes on all five platforms.

Test results: 2311 passes locally and on all five R-hub platforms, with 9
intentional skips on CRAN (a timing check of the port test and eight tests
that start real R processes). All runs had 0 failures and 0 warnings. One
test's expectation count depends on timing.

## Downstream dependencies

The CRAN package index checked on 2026-11-22 (UTC) lists no reverse dependencies.
