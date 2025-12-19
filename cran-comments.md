## Test environments

* local: linux-x86_64 (WSL2), R 4.5.2
* R-hub: linux, windows, macos-arm64

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Lars Bernhardsson <cran.sherry228@passinbox.com>'

  New submission

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Changes made from CRAN submission feedback 2025-12-19

* **DESCRIPTION**: Changed 'Shiny' to 'shiny' in Title and Description fields.

* **R/config-format.R and R/example-config.R**: Changed `\dontrun` to `if(interactive())`
  and re-generated documentation.

* **tests/testthat/...**: Many tests added for functions where it was possible.
  Local test results: 855 pass, 0 fail, 4 warnings (expected - network/file edge cases)
  Only missing tests that require infrastructure mockup to run.
