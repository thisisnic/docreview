#' Review package documentation
#'
#' @param path Path to package
#' @import cli
#' @import purrr
#' @export
docreview <- function(path = "."){

  cli_h1("docreview Results")
  review_functions(path)
  review_errors(path)
  review_vignettes(path)

}
