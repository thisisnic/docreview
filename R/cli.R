#' Parse function documentation analysis results
#'
#' @param results Results of function documentation analysis
#' @param error_on_failure Raise an error on any negative reviews
function_results_parse <- function(results, error_on_failure) {

  no_examples <- results$exports_without_examples

  cli({
    cli_h2("Function Documentation")
    if (length(no_examples) == 0) {

      cli_alert_success(
        "All exported functions contain examples in their documentation"
      )

    } else {

      if (error_on_failure) {
        no_examples_error(no_examples)
      } else {
        no_examples_output(no_examples)
      }

    }
  })
}

no_examples_output <- function(no_examples) {

  names(no_examples) <- rep("*", length(no_examples))

  cli_alert_warning(
    "Exported functions without examples in their documentation:"
  )
  cli_bullets(no_examples)
}

no_examples_error <- function(no_examples){
  rlang::abort(
    message = c(
      x = "All exported functions must contain examples in their documentation.",
      i = paste(
        "Exported functions without examples in their documentation:",
        paste(no_examples, collapse = ", ")
      )
    )
  )
}

#' Parse Vignette Results
#'
#' @param vignette_results Output of calling vignette analysis function
#' @param error_on_failure Raise an error on any negative reviews
#'
#' @keywords internal
vignette_results_parse <- function(vignette_results, error_on_failure) {

  cli({
    cli_h2("Vignettes")

    cli_h3("Reading complexity")

    rc <- lapply(vignette_results, function(x) {
      x$flesch_kincaid
    })

    iwalk(rc, ~ score_complexity(.y, round(.x, 1), error_on_failure = error_on_failure))

    lengths <- lapply(vignette_results, function(x) {
      x$length
    })

    cli_h3("Length")
    iwalk(lengths, ~ score_length(.y, .x, error_on_failure = error_on_failure))

  })
}
