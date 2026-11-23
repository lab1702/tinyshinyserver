## Submission

This is an update to version 0.3.0. The current CRAN version is 0.2.1,
published on 2026-09-24 (CRAN package index checked on 2026-11-23).

## CRAN check problems in 0.2.1

The CRAN check results for 0.2.1 showed an ERROR on
r-devel-linux-x86_64-debian-clang (as seen on 2026-11-20), and the noLD
additional check still shows one (its output is from 2026-11-17). On
2026-11-23, all 13 CRAN check flavors report OK for 0.2.1, including
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
* The proxy no longer forwards app responses larger than the new
  `max_response_size_mb` option (default 1024 MB). It ends the transfer and
  answers HTTP 502, instead of writing a response of any size to disk.
* App processes start in their app directory and read the `.Renviron` and
  `.Rprofile` there, as R does when the app is run on its own, instead of
  those of the directory that `start_tss()` was called from. The site
  profile also runs in apps, and an app's own `.Renviron` that sets
  `R_LIBS` or `R_LIBS_USER` sets its library paths.
* The public `/api/apps` endpoint no longer exposes app paths, ports, or
  process IDs.
* On-demand apps stop 30 seconds after their last WebSocket closes instead
  of immediately, so reloading a page no longer cold-starts the app.
* Configuration files that give a field twice, or a `restart_delay`,
  `health_check_interval`, or `appstart_timeout` above one day, are
  rejected. A timer of about 1e12 seconds stopped the server from answering.
* An R Markdown or Quarto app serves only its entry document: requests for
  its directory's other `.Rmd` and `.qmd` documents and R scripts receive
  HTTP 404, and its WebSocket connections for any other file are closed.
* If an unexpected error stops the server, `start_tss()` signals an error
  once the server and apps have stopped, instead of returning normally.
* The package now requires R >= 4.1.0, as its `promises` and `quarto`
  dependencies already do, and newer minimum versions of `httpuv` (1.5.5),
  `later` (1.4.0), `curl` (3.3), `websocket` (1.1.0), and `ps` (1.2.0) for
  functions it now uses. It no longer imports `future`, `digest`, or
  `openssl`.

## Test environments

* Local: Windows 11 x64 (build 26300), x86_64-w64-mingw32,
  R 4.6.2 (2026-10-29 ucrt).
  Checked with `R CMD check --as-cran` on the source tarball built from
  commit 1ac432c, including the PDF manual and the CRAN incoming
  feasibility check.
* R-hub (commit 1ac432c, run
  https://github.com/lab1702/tinyshinyserver/actions/runs/45101127926):
  linux (R-devel), windows (R-devel), macos-arm64 (R-devel), gcc16, and
  ubuntu-gcc12.

The commit recording these results changes only this file, which is excluded
from the build, so the package checked is identical.

## R CMD check results

Local: 0 errors | 0 warnings | 0 notes.

R-hub at commit 1ac432c: 0 errors | 0 warnings | 0 notes on all five platforms.

Test results: 2659 passes locally and on R-hub's windows (R-devel), with 11
intentional skips on CRAN (a timing check of the port test, and ten tests
that run R processes, such as apps that load shiny or a render with the
Quarto CLI, and check their output, memory use, library paths, or cleanup).
The other four R-hub platforms have one more skip: a test of keeping an
app's log when it is held open runs only on Windows, the one platform that
refuses to rename an open file. They have 2660 passes on the three Linux
platforms and 2658 on macos-arm64 (R-devel): a test of a document name in
another letter case makes two more checks on a file system that tells
letter case apart, which macOS's default file system, like Windows', does
not. All runs had 0 failures and 0 warnings. One test's expectation count
depends on timing.

## Downstream dependencies

The CRAN package index checked on 2026-11-23 (UTC) lists no reverse dependencies.
