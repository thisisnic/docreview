score_length <- function(name, score, thresholds, error_on_failure) {
  length_output(name, score, thresholds)

  if (error_on_failure) {
    length_error(name, score, thresholds)
  }
}

length_output <- function(name, score, thresholds) {
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

length_error <- function(name, score, thresholds) {
  if (is.na(score)) {
    rlang::abort(paste(name, " - could not be calculated"))
  } else if (score > thresholds[1]) {
    rlang::abort(paste(name, "has length:", score, "words."))
  }
}

score_complexity <- function(name, score, thresholds = c(30, 50), error_on_failure) {
  complexity_output(name, score, thresholds)

  if (error_on_failure) {
    complexity_error(name, score, thresholds)
  }
}

complexity_output <- function(name, score, thresholds) {
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

complexity_error <- function(name, score, thresholds) {
  if (is.na(score)) {
    rlang::abort(paste(name, " - could not be calculated"))
  } else if (score < thresholds[1]) {
    rlang::abort(paste(name, "has Flesch-Kincaid reading ability score:", score))
  }
}
