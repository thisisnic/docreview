#' @keywords internal
#' @import cli purrr
NULL

#' Review package documentation
#'
#' @param path Path to package
#' @export
docreview <- function(path = ".") {
  cli_h1("docreview Results")
  function_review(path)
  error_review(path)
  vignette_review(path)
}

#' Review functions
#'
#' @param path Path to package
#'
#' @export
function_review <- function(path) {

  # - do all exported functions have examples
  # - are all parameters covered by examples
  # - readability of "details" sections
  out <- list()
  out$exports_without_examples <- get_exports_without_examples(path)
  parse_function_results(out)
}

#' Review error messages
#'
#' @param path Path to package
#'
#' @export
error_review <- function(path) {

  # - encourage use of "must", "should", etc
  # - readability
  out <- NULL
  parse_error_results(out)
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
  parse_vignette_results(out)
}
