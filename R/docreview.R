#' @keywords internal
#' @import cli purrr quanteda quanteda.textstats
NULL

#' Review package documentation
#'
#' @param path Path to package
#' @param error_on_failure Raise an error on any negative reviews
#' @param doc_types Types of documentation to review
#' @export
package_review <- function(path = ".", error_on_failure = FALSE, doc_types = c("functions", "vignettes")){

  cli_h1("docreview Results")

  if ("functions" %in% doc_types) {
    func_results <- function_review(path)
    function_results_parse(func_results, error_on_failure)
  }

  if ("vignettes" %in% doc_types) {
    vig_results <- vignette_review(path)
    vignette_results_parse(vig_results, error_on_failure)
  }

}

#' Review functions
#'
#' @param path Path to package
#'
#' @export
function_review <- function(path) {

  list("exports_without_examples" = get_exports_without_examples(path))

}

#' Review vignettes
#'
#' @param path Path to package
#'
#' @export
vignette_review <- function(path) {
  vig_paths <- find_vignettes(path)
  out <- lapply(vig_paths, analyse_vignette)
  names(out) <- basename(vig_paths)
  out
}
