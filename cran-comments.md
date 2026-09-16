## Submission

Update from CRAN version 0.1.0 to 0.2.0.

## Test environments

* Local: Ubuntu 26.04.1 LTS, x86_64-pc-linux-gnu, R 4.6.1 (2026-06-24).
* Checked with `R CMD check --as-cran` on the source tarball.
* BLAS/OpenMP thread limits were set to one for the check.
* R-hub Linux and Windows passed on the preceding revision, as reported by
  the maintainer. macOS-arm64 reported a process-termination timing failure;
  this revision adds a bounded wait for process exit.
* Fresh R-hub and R-devel checks are pending for this revision. The results
  below are from the local release-R check only.

## R CMD check results

0 errors | 0 warnings | 0 notes

Test results: 1380 passes, 0 failures, 0 warnings, 1 intentional skip on CRAN
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
