#' Review package documentation
#'
#' @param path Path to package
#' @import cli
#' @import purrr
#' @export
docreview <- function(path = "."){

  cli({
    cli_h1("docreview Results")
    cli_h2("Function Documentation")
    review_functions(path)
    cli_h2("Error Messages")
    review_errors(path)
    cli_h2("Vignettes")
    review_vignettes(path)
  })

}
