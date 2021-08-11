#' Review package documentation
#'
#' @param path Path to package
#' @param error_on_failure Raise an error on any review check failures
#' @param error_on_warning Raise an error on any review check warnings
#' @param doc_types Types of documentation to review
#' @param thresholds List of thresholds that result in fails or warnings
#' @export
#' @examples
#' pkg_path <- system.file("testpkg", package = "docreview")
#' package_review(pkg_path)
package_review <- function(path = ".", error_on_failure = FALSE,
                           error_on_warning = FALSE,
                           doc_types = c("functions", "vignettes"),
                           thresholds = default_thresholds()) {
  cli_h1("docreview Results")

  results <- list()

  if ("functions" %in% doc_types) {
    function_thresholds <- thresholds$functions
    results$functions <- function_review(path, function_thresholds)
    function_results_display(results$functions$details, function_thresholds)
  }

  if ("vignettes" %in% doc_types) {
    vignette_thresholds <- thresholds$vignette
    results$vignettes <- vignette_review(path, vignette_thresholds)
    vignette_results_display(results$vignettes$details, vignette_thresholds)
  }

  check_results(results, error_on_failure, error_on_warning)
}

#' Check results and raise error if necessary
#'
#' Inspired by testthat
#'
#' @param results List of docreview results
#' @param error_on_failure Raise error on review failures?
#' @param error_on_warning Raise error on review warnings?
#'
#' @keywords internal
check_results <- function(results, error_on_failure, error_on_warning) {
  total_failures <- sum(map_dbl(results, "failures"))
  total_warnings <- sum(map_dbl(results, "warnings"))

  if (error_on_warning && total_warnings > 0) {
    rlang::abort(
      paste("\nFailures found by docreview:", total_failures, "\nWarnings found by docreview:", total_warnings),
      call. = FALSE
    )
  }

  if (error_on_failure && total_failures > 0) {
    rlang::abort(
      paste("\nFailures found by docreview:", total_failures),
      call. = FALSE
    )
  }

  invisible(results)
}

#' Set config for docreview checks
#'
#' @importFrom yaml read_yaml
#' @export
set_thresholds <- function(config_path = system.file("docreview.yml", package = "docreview")) {
  read_yaml(config_path)
}
