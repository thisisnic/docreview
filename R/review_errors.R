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

parse_error_results <- function(out){
  cli({
    cli_h2("Error Messages")
  })

}
