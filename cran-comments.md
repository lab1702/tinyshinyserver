## Submission

This is an update to version 0.3.0. The current CRAN version is 0.2.1,
published on 2026-09-24 (CRAN package index checked on 2026-11-23).

## CRAN check problems in 0.2.1

The CRAN package index lists a deadline of 2026-12-09 for this package
(checked on 2026-11-23). This release fixes the check problem described
below.

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
waits up to 5 seconds for the port to close, and on CRAN skips its last
check rather than fail if the port is still in use then, since another
program on a shared check machine can take a freed port. The helper with
which other tests wait for a stopped test server's port to close had the
same fault in another form: it tested the port by listening on it, and that
listener was closed a moment later too. It now only connects to the port.

## Changes in this release

Besides that fix, this release hardens the proxy and management server for
local and reverse-proxy deployments, adds configuration options, and fixes
app lifecycle, proxy, and logging problems. NEWS.md lists every change,
starting with the breaking ones.

The package now requires R >= 4.1.0, as its `promises` and `quarto`
dependencies already do, and newer minimum versions of some imported
packages for functions it now uses. It no longer imports `future`, `digest`,
or `openssl`, and no longer suggests `roxygen2` or `devtools`.

## Test environments

* Local: Windows 11 x64 (build 26300), x86_64-w64-mingw32,
  R 4.6.2 (2026-10-29 ucrt).
  Checked with `R CMD check --as-cran` on the source tarball built from
  commit d059fb3, including the PDF manual and the CRAN incoming
  feasibility check. The test suite was also run there at that commit with
  `NOT_CRAN=true`, which runs the tests skipped on CRAN.
* R-hub (commit d059fb3, run
  https://github.com/lab1702/tinyshinyserver/actions/runs/45279376927):
  linux (R-devel), windows (R-devel), windows (R-oldrel, R 4.5.3),
  macos-arm64 (R-devel), macos-arm64 (R-oldrel, R 4.5.3), gcc16, and
  ubuntu-gcc12.

The commit recording these results changes only this file, which is excluded
from the build, so the package checked is identical.

## R CMD check results

Local: 0 errors | 0 warnings | 0 notes.

R-hub at commit d059fb3: 0 errors | 0 warnings | 0 notes on all seven platforms.

Test results: 3437 passes locally and on R-hub's two windows platforms
(R-devel and R-oldrel), with 13 intentional skips on CRAN (timing checks of
the port test and of forwarding many request headers; ten tests that run
R processes, such as apps that load shiny or a render with the Quarto CLI,
and check their output, memory use, library paths, or cleanup; and one test
that defines `.First()` in the global environment of the R session that runs
the tests).
The other five R-hub platforms have one more skip: a test of keeping an
app's log when it is held open runs only on Windows, the one platform that
refuses to rename an open file. They have 3436 passes on the three Linux
platforms and 3434 on the two macos-arm64 platforms (R-devel and R-oldrel).
A test of the library path specifiers makes two more checks on Windows,
where it also checks `%U` under a temporary `LOCALAPPDATA`, and a test of a
document name in another letter case makes two more checks on a file system
that tells letter case apart, which macOS's default file system, like
Windows', does not. All runs had 0 failures and 0 warnings. One test's
expectation count depends on timing. Locally with `NOT_CRAN=true`, the tests
have 3507 passes and no skips, failures, or warnings.

## Downstream dependencies

The CRAN package index checked on 2026-11-23 (UTC) lists no reverse dependencies.
