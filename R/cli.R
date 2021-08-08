#' Parse function documentation analysis results
#'
#' @param results Results of function documentation analysis
#' @keywords internal
function_results_display <- function(results) {

  no_examples <- results$details$need_examples

  cli({
    cli_h2("Function Documentation")
    if (length(no_examples) == 0) {
      cli_alert_success(
        "All exported functions contain examples in their documentation"
      )
    } else {
      names(no_examples) <- rep("*", length(no_examples))
      cli_alert_danger(
        "Exported functions without examples in their documentation:"
      )
      cli_bullets(no_examples)
    }
  })
}

#' Parse Vignette Results
#'
#' @param vignette_results Output of calling vignette analysis function
#' @param error_on_failure Raise an error on any negative reviews
#'
#' @keywords internal
vignette_results_parse <- function(vignette_results) {
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
