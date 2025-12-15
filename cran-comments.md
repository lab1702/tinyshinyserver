## Test environments

* local: linux-x86_64 (WSL2), R 4.5.2
* R-hub: linux, windows, macos

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Lars Bernhardsson <cran.sherry228@passinbox.com>'

  New submission

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Changes made from CRAN submission feedback 2025-12-15

* **DESCRIPTION**: Added single quotes around 'Shiny' in Title and Description
  fields to comply with CRAN policy for software names.

* **R/start_tss.R and man/start_tss.Rd**: Changed examples from `\dontrun{}` to
  `if(interactive()){}` as per CRAN guidelines. Simplified example to use
  `tempdir()` instead of copying files to current working directory.

* **R/utils.R**: Replaced custom logging appender using cat()
  with `logger::appender_tee()`.
