#' @keywords internal
#' @import cli purrr
NULL

#' Review package documentation
#'
#' @param path Path to package
#' @export
package_review <- function(path = ".", error_on_failure = FALSE, review = c("functions", "vignettes")){

  cli_h1("docreview Results")

  if ("functions" %in% review) {
    func_results <- function_review(path)
    parse_function_results(func_results)
  }

  if ("vignettes" %in% review) {
    vig_results <- vignette_review(path)
    parse_vignette_results(vig_results)
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
