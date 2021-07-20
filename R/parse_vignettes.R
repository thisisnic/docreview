#' Analyse a single vignette
#'
#' @param vig_path Path to directory where vignette is
analyse_vignette <- function(vig_path, problem_words){

  vig_sects <- parse_vignette(vig_path)
  cleaned_md <- lapply(vig_sects, clean_chunks)
  fk <- get_fk_score(cleaned_md)
  lengths <- get_length(cleaned_md)
  pws <- detect_problem_words(cleaned_md, problem_words)
  list(flesch_kincaid = fk$overall, length = lengths$overall, problem_words = pws)
}

detect_problem_words <- function(md, problem_words){
  problem_regex <- paste0(
      problem_words, collapse = "|"
    )
  out <- lapply(md, function(x){
    stringr::str_detect(tolower(x), problem_regex)
  })

  md[out == TRUE]

}

#' Get number of words in a markdown chunk
#'
#' @param md Markdown chunk
get_length <- function(md){
  token_counts <- lapply(md, function(x) length(tokens(x[[1]])[[1]]))
  list(overall = sum(unlist(token_counts)), sections = token_counts)
}

#' Get Flesch-Kincaid readability score
#'
#' @param code List of code chunks
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
  md_sections <- parsermd::rmd_select(vig, has_type("rmd_markdown"))
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
