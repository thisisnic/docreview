## Test environments
* local R installation, R 4.1.0
* ubuntu 16.04 (on travis-ci), R 4.1.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Unexported function with an example
* The function which might show up as an unexported function with an example (`./inst/testpkg/R/add.Rd `) is intentional - it's inside a "dummy package" for use by docreview to run unit tests and code examples against, as one of the things that docreview does is check whether package functions have examples or not
