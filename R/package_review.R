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

default_thresholds <- function() {
  set_thresholds()
}

#' Set thresholds at which review checks should fail
#'
#' @description This function is experimental and as the key package metrics
#' evolve, parameters may be subject to change.
#'
#' @param exports_without_examples Action to take when exports without examples
#' are identified. Possible values are "fail", "warn", or "none".
#' @param fk_fail Vignette Flesch-Kincaid scores lower than this will result in
#' a review check failure
#' @param fk_warn Vignette Flesch-Kincaid scores lower than this but higher than
#'  `fk_fail` will results in a review check warning
#' @param length_fail Vignette word count longer than this will result in a
#' review check failure
#' @param length_warn Vignette word count longer than this but shorter than
#' `length_fail` will result in a review check warning
#'
#' @export
#' @examples
#' set_thresholds(exports_without_examples = "warn")
set_thresholds <- function(exports_without_examples = "fail", fk_fail = 30,
                           fk_warn = 50, length_fail = 3000,
                           length_warn = 2000) {
  list(
    functions = list(
      exports_without_examples = match.arg(
        exports_without_examples,
        c("fail", "warn", "none")
      )
    ),
    vignettes = list(
      fk = list(fail = fk_fail, warn = fk_warn),
      length = list(fail = length_fail, warn = length_warn)
    )
  )
}
