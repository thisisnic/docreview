% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package_review.R
\name{package_review}
\alias{package_review}
\title{Review package documentation}
\usage{
package_review(path = ".", config = get_config())
}
\arguments{
\item{path}{Path to package}

\item{config}{Review check configuration}
}
\description{
Review package documentation
}
\examples{
pkg_path <- system.file("testpkg", package = "docreview")
package_review(pkg_path)
}
