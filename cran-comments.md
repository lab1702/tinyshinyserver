## Submission

Update from CRAN version 0.1.0 to 0.2.0.

## Test environments

* Local: Ubuntu 26.04.1 LTS, x86_64-pc-linux-gnu, R 4.6.1 (2026-06-24).
* Checked with `R CMD check --as-cran` on the source tarball.
* BLAS/OpenMP thread limits were set to one for the check.
* R-hub Linux: Ubuntu 24.04.5 LTS, x86_64-pc-linux-gnu,
  R-devel (2026-09-14 r90539).
* R-hub Windows: Windows Server 2022 x64 (build 26100), x86_64-w64-mingw32,
  R-devel (2026-09-14 r90539 ucrt).
* R-hub macOS ARM64: macOS Tahoe 26.6.2, aarch64-apple-darwin23,
  R-devel (2026-09-15 r90540).

All three R-hub platform jobs passed on 2026-09-16 (UTC), checking commit
`fe74287d28a4d84db160073f82827237c6de5a50` on branch `review20261115`.
Verified job results and check logs:
[R-hub run 35055227789](https://github.com/lab1702/tinyshinyserver/actions/runs/35055227789).

## R CMD check results

Local and all three R-hub checks: 0 errors | 0 warnings | 0 notes.

Each check reported the following test results: 1380 passes, 0 failures, 0 warnings, 1 intentional skip on CRAN
(the invalid-device-path write test).

## Downstream dependencies

The CRAN package index checked on 2026-09-16 (UTC) lists no reverse dependencies.

## Submission preparation

* Changed README localhost examples from hyperlinks to code so incoming URL
  checks do not attempt to connect to a local server.
* Confined help examples to temporary directories, with working-directory
  restoration and cleanup on normal return or error.
* Added explicit cleanup for application subprocesses started by tests and
  mocked application restarts in the management routing test.
* Replaced the port-80 test with an ephemeral loopback server and handled
  expected connection warnings when probing closed ports.
* Added a bounded wait for process exit after termination signals; the
  macOS-arm64 process-termination regression now passes in R-hub.
