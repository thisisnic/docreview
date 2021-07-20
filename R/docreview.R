#' Review package documentation
#'
#' @param path Path to package
#'
#' @export
docreview <- function(path = "."){

  vignette_results <- review_vignettes(path)
  error_results <- review_errors(path)
  function_results <- review_functions(path)

  cli({
    cli_h1("docreview Results")
    cli_h2("Function Documentation")
    parse_results(function_results)
    cli_h2("Error Messages")
    parse_results(error_results)
    cli_h2("Vignettes")
    parse_results(vignette_results)
  })

}

#' Review vignettes
#'
#' @param path Path to package
#'
#' @export
review_vignettes <- function(path){

 problem_words <- c("simply", "naturally", "mere", "obvious", "trivial")

  vig_paths <- find_vignettes(path)
  out <- lapply(vig_paths, analyse_vignette, problem_words)
  names(out) <- basename(vig_paths)
  out

}

#' Review error messages
#'
#' @param path Path to package
#'
#' @export
review_errors <- function(path){


# - encourage use of "must", "should", etc
# - readability


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

}

#' Parse results into CLI list items
#'
#' @param results Review results, as an unnamed list
parse_results <- function(results){
  c(
    cli_ul(),
    lapply(results, cli_li),
    cli_end()
  )
}
