% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/package_review.R
\name{get_config}
\alias{get_config}
\title{Set config for docreview checks}
\usage{
get_config(
  config_path = system.file("configs", "docreview.yml", package = "docreview", mustWork =
    TRUE)
)
}
\arguments{
\item{config_path}{Path to config file}
}
\description{
Set config for docreview checks
}
\examples{
# Get default configuration
get_config()
}
