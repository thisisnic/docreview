#' @keywords internal
#' @import cli purrr quanteda quanteda.textstats
NULL

#' Review package documentation
#'
#' @param path Path to package
#' @param error_on_failure Raise an error on any negative reviews
#' @param doc_types Types of documentation to review
#' @export
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

#' Review functions
#'
#' @param path Path to package
#'
#' @export
function_review <- function(path, thresholds = default_thresholds()$functions) {

  detailed_results <- list(
    exports_examples = get_exports_without_examples(path)
  )

  comments <- function_get_comments(detailed_results, thresholds)

  list(failures = comments$fail, warnings = comments$warn, details = detailed_results)
}

#' Count failures and warnings for function review
#'
#' @param results Results of function review
#' @param thresholds List of thresholds that result in fails or warnings
#' @keywords internal
function_get_comments <- function(results, thresholds) {

  comments <- list(fail = 0, warn = 0)

  # Count failures and warnings for exports without examples
  need_examples <- sum(results$exports_examples)

  if (need_examples > 0) {
    if (thresholds$exports_without_examples == "fail") {
      comments$fail <- comments$fail + need_examples
    } else if (thresholds$exports_without_examples == "warn") {
      comments$warn <- comments$warn + need_examples
    }
  }

  comments
}

#' Review vignettes
#'
#' @param path Path to package
#'
#' @export
vignette_review <- function(path, thresholds = default_thresholds()$vignettes) {

  vig_paths <- find_vignettes(path)
  detailed_results <- lapply(vig_paths, analyse_vignette)
  names(detailed_results) <- basename(vig_paths)

  comments <- vignettes_get_comments(detailed_results, thresholds)

  list(failures = comments$fail, warnings = comments$warn, details = detailed_results)

}

vignettes_get_comments <- function(results, thresholds){

  comments <- list(fail = 0, warn = 0)

  # Count failures and warnings for Flesch Kincaid scores
  fk_scores <- map(results, "flesch_kincaid")
  fk_fails <- length(fk_scores[fk_scores <= thresholds$fk$fail])
  fk_warns <- length(fk_scores[fk_scores > thresholds$fk$fail && fk_scores <= thresholds$fk$warn])

  # Count failures and warnings for lengths
  length_scores <- map(results, "length")
  length_fails <- length(length_scores[length_scores >= thresholds$length$fail])
  length_warns <- length(length_scores[length_scores < thresholds$length$fail && length_scores >= thresholds$length$warn])


  comments$fail <- comments$fail + fk_fails + length_fails
  comments$warn <- comments$warn + fk_warns + length_warns

  comments

}

default_thresholds <- function(){
  list(
    functions = list(
      exports_without_examples = "fail"
    ),
    vignettes = list(
      fk = list(
        fail = 30,
        warn = 50
      ),
      length = list(
        fail = 3000,
        warn = 2000
      )
    )
  )
}
