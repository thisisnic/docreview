score_length <- function(name, score, thresholds = c(3000, 2000)) {
  if (is.na(score)) {
    return(cli_alert_danger(paste(name, " - could not be calculated")))
  } else if (score > thresholds[1]) {
    alert <- cli_alert_danger
  } else if (score > thresholds[2]) {
    alert <- cli_alert_warning
  } else {
    alert <- cli_alert_success
  }

  alert(paste(name, "has length:", score, "words."))

}

score_complexity <- function(name, score, thresholds = c(30, 50)) {
  if (is.na(score)) {
    return(cli_alert_danger(paste(name, " - could not be calculated")))
  } else if (score < thresholds[1]) {
    alert <- cli_alert_danger
  } else if (score < thresholds[2]) {
    alert <- cli_alert_warning
  } else {
    alert <- cli_alert_success
  }

  alert(paste(name, "has Flesch-Kincaid reading ability score:", score))

}

score_problem_section <- function(name, content) {
  cli({
    cli_alert_warning(
      paste(
        'Possible problem word ("simply", "naturally", "mere", "obvious", or "trivial") in vignette:',
        name
      )
    )
    lapply(content, function(problematic_section) {
      cli_alert(paste(
        "Detected in the following section:",
        "\n",
        problematic_section,
        "\n"
      ))
    })
  })
}
