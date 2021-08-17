
<!-- README.md is generated from README.Rmd. Please edit that file -->

# docreview

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/docreview)](https://cran.r-project.org/package=docreview)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/thisisnic/docreview/workflows/R-CMD-check/badge.svg)](https://github.com/thisisnic/docreview/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/thisisnic/docreview/branch/main/graph/badge.svg)](https://codecov.io/gh/thisisnic/docreview)
<!-- badges: end -->

*An opinionated package for enhancing your R package documentation.*

When writing R packages, high quality documentation can make for a great
experience for your users.

You can use **docreview** to check that your R package documentation
passes a number of checks:

-   all exported functions should have documented examples of how to use
    them
-   vignettes should not be too long
-   vignettes should not be too complex

## Installation

You can install docreview from CRAN

``` r
install.packages("docreview")
```

If you’d like access to the most up-to-date features, you can install
the development version from GitHhub.

``` r
# install.packages("devtools")
devtools::install_github("thisisnic/docreview")
```

## Usage

``` r
library(docreview)
pkg_path <- system.file("testpkg", package = "docreview")
package_review(path = pkg_path)
```

See [the
vignette](https://thisisnic.github.io/docreview/articles/docreview.html)
for more detailed usage guides.

## Current default review checks

-   Vignettes have no more than 2000 words (warn) or 3000 words (fail)
-   Vignettes have no fewer than 100 words (warn) or 0 words (fail)
-   Vignettes have a Flesch-Kincaid readability score lower than 50
    (warn) or 30 (fail)
-   All exported functions have examples in their documentation
