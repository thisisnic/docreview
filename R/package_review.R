#' Review package documentation
#'
#' @param path Path to package
#' @param config Review check configuration
#' @export
#' @examples
#' pkg_path <- system.file("testpkg", package = "docreview")
#' package_review(pkg_path)
package_review <- function(path = ".", config = get_config()) {
  cli_h1(paste("docreview results for", basename(normalizePath(path, mustWork = TRUE))))

  results <- list()

  if (config$functions$active) {
    function_checks <- config$functions
    results$functions <- function_review(path, function_checks)
  }

  if (config$vignettes$active) {
    vignette_checks <- config$vignette
    results$vignettes <- vignette_review(path, vignette_checks)
  }

  if (config$functions$active) {
    function_results_display(results$functions$details, function_checks)
  }

  if (config$vignettes$active) {
    vignette_results_display(results$vignettes$details, vignette_checks)
  }

  check_results(results, config)
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
check_results <- function(results, config) {
  if (config$error_on_failure | config$error_on_warning) {
    total_failures <- sum(map_dbl(results, "failures"))
    total_warnings <- sum(map_dbl(results, "warnings"))

    if (config$error_on_warning && total_warnings > 0) {
      rlang::abort(
        paste("\nFailures found by docreview:", total_failures, "\nWarnings found by docreview:", total_warnings),
        call. = FALSE
      )
    }

    if (config$error_on_failure && total_failures > 0) {
      rlang::abort(
        paste("\nFailures found by docreview:", total_failures),
        call. = FALSE
      )
    }
  }

  invisible(results)
}

#' Set config for docreview checks
#'
#' @param config_path Path to config file
#' @importFrom yaml read_yaml
#' @export
#' @examples
#' # Get default configuration
#' get_config()
get_config <- function(config_path = system.file("configs", "docreview.yml", package = "docreview", mustWork = TRUE)) {
  read_yaml(config_path)
}
