
#' Parse Vignette Results
parse_vignette_results <- function(vignette_results){


  cli_h3("Reading complexity")

  rc <- lapply(vignette_results, function(x){
    x$flesch_kincaid
  })

  iwalk(rc, ~warn_complexity(.y, round(.x,1)))

  lengths <- lapply(vignette_results, function(x){
    x$length
  })

  cli_h3("Length")
  iwalk(lengths, ~warn_length(.y, .x))

    # This needs entirely refactoring and adding back in as a feature


  # pws <- lapply(vignette_results, function(x){
  #   x$problem_words
  # })
  # pws_sections <- pws[lengths(pws) > 0]

  # cli_h3("Problematic words")
  # iwalk(pws_sections, ~warn_problem_section(.y, .x))


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

  vig_paths <- find_vignettes(path)
  out <- lapply(vig_paths, analyse_vignette)
  names(out) <- basename(vig_paths)
  parse_vignette_results(out)

}



#' Analyse a single vignette
#'
#' @param vig_path Path to directory where vignette is
analyse_vignette <- function(vig_path){

  vig_sects <- parse_vignette(vig_path)
  cleaned_md <- lapply(vig_sects, clean_chunks)
  fk <- get_fk_score(cleaned_md)
  lengths <- get_length(cleaned_md)
  pws <- detect_problem_words(cleaned_md)
  list(flesch_kincaid = fk$overall, length = lengths$overall, problem_words = pws)
}

detect_problem_words <- function(md){
  problem_words <- c("[Ss]imply", "[Nn]aturally", "[Mm]ere", "[Oo]bvious", "[Tt]rivial")
  problem_regex <- paste0(
      problem_words, collapse = "|"
    )
  out <- lapply(md, function(x){
    stringr::str_detect(x, problem_regex)
  })

  md[out == TRUE]

}

#' Get number of words in a markdown chunk
#'
#' @param md Markdown chunk
#'
#' @importFrom quanteda tokens
get_length <- function(md){
  token_counts <- lapply(md, function(x) length(tokens(x[[1]])[[1]]))
  list(overall = sum(unlist(token_counts)), sections = token_counts)
}

#' Get Flesch-Kincaid readability score
#'
#' @param code List of code chunks
#'
#' @importFrom quanteda.textstats textstat_readability
get_fk_score <- function(code){

  chunk_scores <- lapply(code, function(x){
    textstat_readability(x)$Flesch
  })

  # if fk < 0, something's probably gone a bit wrong
  reasonable_scores <- code[chunk_scores > 0]

  one_chunk <- paste(unlist(reasonable_scores), collapse = " ")
  overall_score <- textstat_readability(one_chunk)$Flesch

  list(overall = overall_score, sections = chunk_scores)
}

#' Clean markdown chunks
#'
#' Remove text in links and backticks from chunks
#'
#' @param chunk Code chunk
clean_chunks <- function(chunk){
  no_links <- remove_links(chunk)
  no_backticks <- remove_backticks(no_links)
  no_backticks
}

#' Remove links from a chunk
#'
#' Remove any links in markdown format
#'
#' @param chunk Code chunk
remove_links <- function(chunk){
  stringr::str_remove_all(chunk, "\\[([^\\[]+)\\]\\(.*?\\)")
}

#' Remove any code in backticks from a chunk
#'
#' @param chunk Code chunk
remove_backticks <- function(chunk){
  stringr::str_remove_all(chunk, "`.*?`")
}

#' Parse a vignette
#'
#' @param vig_path Path to vignette
#' @return List of length 1 character vectors containing contents of each markdown section
parse_vignette <- function(vig_path){

  vig <- parsermd::parse_rmd(vig_path)

  # Extract all markdown sections
  md_sections <- parsermd::rmd_select(vig, parsermd::has_type("rmd_markdown"))
  lapply(md_sections, extract_md_section)

}

#' Extract markdown section
#'
#' Extract the content from the markdown section and collapse it into one string
#'
#' @param md Markdown
extract_md_section <- function(md){
  doc <- parsermd::as_document(md)
  paste(doc, collapse = " ")
}

#' Find vignettes
#'
#' Find all vignettes in a package
#'
#' @param path Path to package
find_vignettes <- function(path = "."){
  vig_path <- file.path(path, "vignettes")
  list.files(vig_path, ".Rmd", full.names = TRUE)
}
