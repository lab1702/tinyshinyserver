## Submission

Update from CRAN version 0.2.0 to 0.2.1.

This release fixes the test ERROR reported on r-devel-linux-x86_64-debian-gcc
and r-devel-linux-x86_64-fedora-gcc in the CRAN check results for 0.2.0.

## Changes in this release

* Fixed a race in WebSocket proxy test teardown that caused `invalid state`
  errors from `websocket::WebSocket$close()`. Test clients were closed after
  the servers they were connected to had stopped; the WebSocket I/O thread
  could observe the disconnect before R's event loop updated the client state.
  Clients are now closed before their servers are stopped. The failure was
  reproduced deterministically before the fix and no longer occurs after it.
* Process termination now tolerates child processes that exit between being
  listed and being signalled. This race caused a second, intermittent test
  ERROR on R-hub's `ubuntu-gcc12` platform, and could make a successful app
  stop or restart report failure. A regression test covers it.

## Test environments

* Local: Windows 11 x64 (build 28000), x86_64-w64-mingw32,
  R 4.6.1 (2026-06-24 ucrt).
  Checked with `R CMD check --as-cran` on the source tarball, with remote
  incoming checks disabled and OpenMP thread limit set to one.
* R-hub `ubuntu-gcc12` (R-hub's copy of r-devel-linux-x86_64-debian-gcc):
  Ubuntu 22.04.5 LTS, x86_64-pc-linux-gnu, R-devel (2026-04-13 r89874).
* R-hub `gcc16`: Fedora Linux 44, x86_64-pc-linux-gnu,
  R-devel (2026-09-23 r90586).
* R-hub Linux: Ubuntu 24.04.5 LTS, x86_64-pc-linux-gnu,
  R-devel (2026-09-23 r90586).
* R-hub Windows: Windows Server 2022 x64 (build 26100), x86_64-w64-mingw32,
  R-devel (2026-09-23 r90587 ucrt).
* R-hub macOS ARM64: macOS Tahoe 26.6.2, aarch64-apple-darwin23,
  R-devel (2026-09-23 r90587).

All five R-hub platform jobs passed on 2026-09-24 (UTC), checking commit
`d77119fa2aec7f3881f61404ca6f36c9ac4b4156` on branch `main`.
Verified job results and check logs:
[R-hub run 36025456524](https://github.com/lab1702/tinyshinyserver/actions/runs/36025456524).

## R CMD check results

Local and all five R-hub checks: 0 errors | 0 warnings | 0 notes.

Each R-hub check reported the following test results: 1382 passes, 0 failures,
0 warnings, 1 intentional skip on CRAN (the invalid-device-path write test).

## Downstream dependencies

The CRAN package index checked on 2026-09-24 (UTC) lists no reverse dependencies.
