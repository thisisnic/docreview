# docreview

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/docreview)](https://cran.r-project.org/package=docreview)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/thisisnic/docreview/workflows/R-CMD-check/badge.svg)](https://github.com/thisisnic/docreview/actions?query=workflow%3AR-CMD-check)
[![codecov](https://codecov.io/gh/thisisnic/docreview/branch/main/graph/badge.svg)](https://codecov.io/gh/thisisnic/docreview)

_An opinionated package for enhancing your R package documentation._

When writing R packages, high quality documentation can make for a great experience for your users.  

You can use docreview to check that your R package documentation passes a number of checks:

* all exported functions should have documented examples of how to use them
* vignettes should not be too long
* vignettes should not be too complex

## Installation

```{r}
remotes::install_github("thisisnic/docreview")
```

## Usage

```{r}
library(docreview)
pkg_path <- system.file("testpkg", package = "docreview")
package_review(path = pkg_path)
```

See [the vignette](https://thisisnic.github.io/docreview/articles/docreview.html) for more detailed usage guides.
