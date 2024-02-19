
<!-- README.md is generated from README.Rmd. Please edit that file -->

# docreview 📝🔎

<!-- badges: start -->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/docreview)](https://cran.r-project.org/package=docreview)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/thisisnic/docreview/workflows/R-CMD-check/badge.svg)](https://github.com/thisisnic/docreview/actions?query=workflow%3AR-CMD-check)
[![codecov](https://app.codecov.io/gh/thisisnic/docreview//branch/main/graph/badge.svg)](https://app.codecov.io/gh/thisisnic/docreview/)
<!-- badges: end -->

*An opinionated package for enhancing your R package documentation.*

When writing R packages, high quality documentation can make for a great
experience for your users 📦📝🌟🙂

You can use **docreview** to check that your R package documentation
passes a number of checks relating to function documentation and
vignette quality. ✅ ✅ ❌

Note that this package is in *very* early stages of its development and
features are not yet stable, and so the package may undergo significant
changes. If you plan to use it as part of a workflow, please be aware
that new releases are very likely to introduce breaking changes to the
config files, and so use the package (and any updates) with caution.

## Installation

You can install docreview from CRAN.

``` r
install.packages("docreview")
```

If you’d like access to the most up-to-date features, you can install
the development version from GitHhub. 👩🏽‍🔧

``` r
remotes::install_github("thisisnic/docreview")
```

## Usage

``` r
library(docreview)
pkg_path <- system.file("testpkg", package = "docreview")

# ensure you have rebuilt your vignettes if you want to run checks requiring the HTML files
tools::buildVignettes(dir = pkg_path, quiet = TRUE)

# Review docs in the package
package_review(path = pkg_path)
```

See [the
vignette](https://thisisnic.github.io/docreview/articles/docreview.html)
for more detailed usage guides.

## Current default review checks

CRAN version:

🎯 Vignettes have no more than 2000 words (warn) or 3000 words (fail)

🎯 Vignettes have no fewer than 100 words (warn) or 0 words (fail)

🎯 Vignettes have a Flesch-Kincaid readability score lower than 50 (warn)
or 30 (fail)

🎯 All exported functions have examples in their documentation

Additional checks in the dev version:

🎯 All images in vignettes contain alt text
