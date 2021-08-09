#' Parse Vignette Results
#'
#' @param vignette_results Output of calling vignette analysis function
#' @param error_on_failure Raise an error on any negative reviews
#'
#' @keywords internal
vignette_results_display <- function(results, thresholds) {
  cli({
    cli_h2("Vignettes")

    if (length(results) == 0) {
      cli_h3("No vignettes detected")
    } else {
      cli_h3("Flesch Kincaid reading complexity scores:")

      fk_scores <- map_dbl(results, "flesch_kincaid")

      bullet_vals <- paste0(names(fk_scores), ": ", round(fk_scores, 2))

      bullet_names <- rep(" ", length(fk_scores))
      bullet_names[fk_scores <= thresholds$fk$fail] <- "x"
      bullet_names[fk_scores > thresholds$fk$fail & fk_scores <= thresholds$fk$warn] <- "!"
      bullet_names[fk_scores > thresholds$fk$warn] <- "v"

      names(bullet_vals) <- bullet_names

      cli_bullets(bullet_vals)

      cli_h3("Length scores")
      length_scores <- map_dbl(results, "length")

      bullet_vals <- paste0(names(length_scores), ": ", length_scores, " words.")

      bullet_names <- rep(" ", length(length_scores))
      bullet_names[length_scores >= thresholds$length$fail] <- "x"
      bullet_names[length_scores < thresholds$length$fail & length_scores >= thresholds$length$warn] <- "!"
      bullet_names[length_scores < thresholds$length$warn] <- "v"

      names(bullet_vals) <- bullet_names

      cli_bullets(bullet_vals)
    }


  })
}

#' Parse function documentation analysis results
#'
#' @param results Results of function documentation analysis
#' @keywords internal
function_results_display <- function(results, thresholds) {
  examples <- as.character(results$exports_examples)
  names(examples) <- names(results$exports_examples)

  cli({
    cli_h2("Function Documentation")

    no_example_action <- thresholds$exports_without_examples

    if (no_example_action %in% c("fail", "warn")) {
      cli_h3("Exported functions containing examples in their documentation: ")

      if (no_example_action == "fail") {
        examples[examples == "FALSE"] <- "x"
      } else if (no_example_action == "warn") {
        examples[examples == "FALSE"] <- "!"
      }

      examples[examples == "TRUE"] <- "v"

      exports <- names(examples)
      names(exports) <- examples

      cli_bullets(exports)
    }
  })
}
