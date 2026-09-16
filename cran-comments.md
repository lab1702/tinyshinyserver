## Submission

Update from CRAN version 0.1.0 to 0.2.0.

## Test environments

* Local: Ubuntu 26.04.1 LTS, x86_64-pc-linux-gnu, R 4.6.1 (2026-06-24).
* Checked with `R CMD check --as-cran` on the source tarball.
* BLAS/OpenMP thread limits were set to one for the check.
* Fresh Windows, macOS, and R-devel checks have not yet been run for this
  revision. The results below are from the local release-R check only.

## R CMD check results

0 errors | 0 warnings | 0 notes

Test results: 1375 passes, 0 failures, 0 warnings, 1 intentional skip on CRAN
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
