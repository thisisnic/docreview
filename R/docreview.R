#' Review package documentation
#'
#' @param path Path to package
#' @import cli
#' @import purrr
#' @export
docreview <- function(path = "."){

  vignette_results <- review_vignettes(path)
  # error_results <- review_errors(path)
  # function_results <- review_functions(path)

  cli({
    # cli_h1("docreview Results")
    # cli_h2("Function Documentation")
    # parse_results(function_results)
    # cli_h2("Error Messages")
    # parse_results(error_results)
    cli_h2("Vignettes")
    parse_vignette_results(vignette_results)
  })

}

#' Parse Vignette Results
parse_vignette_results <- function(vignette_results){


  cli_h2("Reading complexity")

  rc <- lapply(vignette_results, function(x){
    x$flesch_kincaid
  })

  iwalk(rc, ~warn_complexity(.y, round(.x,1)))

  lengths <- lapply(vignette_results, function(x){
    x$length
  })

  cli_h2("Length")
  iwalk(lengths, ~warn_length(.y, .x))

  pws <- lapply(vignette_results, function(x){
    x$problem_words
  })
  pws_sections <- pws[lengths(pws) > 0]


  cli_h2("Problematic words")
  iwalk(pws_sections, ~warn_problem_section(.y, .x))


}

warn_length <- function(name, score, thresholds = c(3000, 2000)){

  if (score > thresholds[1]) {
    alert <- cli_alert_danger
  } else if (score > thresholds[2]) {
    alert <- cli_alert_warning
  } else {
    alert <- cli_alert_success
  }

  alert(paste(name, "has length:", score, "words."))

}

warn_complexity <- function(name, score, thresholds = c(30, 50)){

  if (score < thresholds[1]) {
    alert <- cli_alert_danger
  } else if (score < thresholds[2]) {
    alert <- cli_alert_warning
  } else {
    alert <- cli_alert_success
  }

  alert(paste(name, "has Flesch-Kincaid reading ability score:", score))

}

warn_problem_section <- function(name, content){
  cli({
    cli_alert_warning(
    paste(
      'Possible problem word ("simply", "naturally", "mere", "obvious", or "trivial") in vignette:',
      name
    )
  )
  lapply(content, function(problematic_section){

    cli_alert(
      paste(
        "Detected in the following section:",
        "\n",
        problematic_section,
        "\n"
      )
    )


  })
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
