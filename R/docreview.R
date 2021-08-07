#' @keywords internal
#' @import cli purrr rlang stringr
NULL

#' Review package documentation
#'
#' @param path Path to package
#' @export
docreview <- function(path = "."){

  cli_h1("docreview Results")
  review_functions(path)
  review_errors(path)
  review_vignettes(path)

}

#' Review functions
#'
#' @param path Path to package
#'
#' @export
review_functions <- function(path){

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
review_errors <- function(path){

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
review_vignettes <- function(path){

  vig_paths <- find_vignettes(path)
  out <- lapply(vig_paths, analyse_vignette)
  names(out) <- basename(vig_paths)
  parse_vignette_results(out)

}



