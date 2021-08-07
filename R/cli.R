parse_error_results <- function(out) {
  cli({
    # cli_h2("Error Messages")
  })
}

parse_function_results <- function(function_results) {
  no_examples <- function_results$exports_without_examples

  cli({
    cli_h2("Function Documentation")
    if (length(no_examples) > 0) {
      cli_alert_warning("The following exported functions do not contain examples in their documentation:")
      names(no_examples) <- rep("*", length(no_examples))
      cli_bullets(no_examples)
    } else {
      cli_alert_success("All exported functions contain examples in their documentation")
    }
  })
}


#' Parse Vignette Results
#'
#' @param vignette_results Output of calling vignette analysis function
parse_vignette_results <- function(vignette_results) {
  cli({
    cli_h2("Vignettes")

    cli_h3("Reading complexity")

    rc <- lapply(vignette_results, function(x) {
      x$flesch_kincaid
    })

    iwalk(rc, ~ score_complexity(.y, round(.x, 1)))

    lengths <- lapply(vignette_results, function(x) {
      x$length
    })

    cli_h3("Length")
    iwalk(lengths, ~ score_length(.y, .x))

    # This needs entirely refactoring and adding back in as a feature


    # pws <- lapply(vignette_results, function(x){
    #   x$problem_words
    # })
    # pws_sections <- pws[lengths(pws) > 0]

    # cli_h3("Problematic words")
    # iwalk(pws_sections, ~score_problem_section(.y, .x))
  })
}
