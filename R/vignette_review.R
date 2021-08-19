#' Review vignettes
#'
#' @param path Path to package
#' @param checks Checks to run
vignette_review <- function(path, checks) {
  vig_paths <- find_vignettes(path)
  detailed_results <- lapply(vig_paths, analyse_vignette, checks = checks)
  names(detailed_results) <- basename(vig_paths)

  comments <- vignettes_get_comments(detailed_results, checks)

  list(failures = comments$fail, warnings = comments$warn, details = detailed_results)
}

vignettes_get_comments <- function(results, checks) {

  comments <- list(fail = 0, warn = 0)

  if (!is.null(checks$`flesch-kincaid`) && checks$`flesch-kincaid`$active) {
    # Count failures and warnings for Flesch Kincaid scores
    fk_scores <- map(results, "flesch_kincaid")
    fk_thresholds <- checks$`flesch-kincaid`$thresholds$poor_readbility

    fk_fails <- length(fk_scores[fk_scores <= fk_thresholds$fail])
    fk_warns <- length(fk_scores[fk_scores > fk_thresholds$fail & fk_scores <= fk_thresholds$warn])

    comments$fail <- comments$fail + fk_fails
    comments$warn <- comments$warn + fk_warns
  }

  if (!is.null(checks$length) && checks$length$active) {
    # Count failures and warnings for lengths
    length_scores <- map(results, "length")
    long_thresholds <- checks$length$thresholds$too_long
    short_thresholds <- checks$length$thresholds$too_short

    long_fails <- length(length_scores[length_scores >= long_thresholds$fail])
    long_warns <- length(length_scores[length_scores < long_thresholds$fail & length_scores >= long_thresholds$warn])

    short_fails <- length(length_scores[length_scores <= short_thresholds$fail])
    short_warns <- length(length_scores[length_scores > short_thresholds$fail & length_scores <= short_thresholds$warn])

    comments$fail <- comments$fail + long_fails + short_fails
    comments$warn <- comments$warn + long_warns + short_warns
  }

  if (!is.null(checks$image_alt_text) && checks$image_alt_text$active) {
    alt_checks <- checks$image_alt_text

    imgs <- unlist(map(results, "image_alt_text"))
    imgs[is.na(imgs)] <- ""

    failed_images <- imgs[nchar(imgs) < alt_checks$min_chars]
    n_fail <- length(failed_images)

    if (n_fail > alt_checks$fail) {
      comments$fail <- comments$fail + n_fail
    } else if (n_fail < alt_checks$fail & n_fail > alt_checks$warn) {
      comments$warn <- comments$warn + n_fail
    }
  }

  comments
}

#' Analyse a single vignette
#'
#' @param vig_path Path to directory where vignette is
#' @keywords internal
analyse_vignette <- function(vig_path, checks) {

      parsed_vig <- parse_vignette(vig_path)

      out <- list()

      if (checks$`flesch-kincaid`$active) {
        fk <- get_fk_score(parsed_vig$cleaned)
        out$flesch_kincaid <- fk$overall
      }

      if (checks$`length`$active) {
        lengths <- get_length(parsed_vig$cleaned)
        out$length <- lengths$overall
      }

      if (checks$`problem_words`$active) {
        pws <- detect_problem_words(parsed_vig$cleaned)
        out$problem_words <- pws
      }

      if (checks$image_alt_text$active) {
        out$image_alt_text <- check_image_alt_text(vig_path)
      }

      out
}

#' Check each image for an alt text description
#'
#' @param vig_path Path to vignette
#' @keywords internal
check_image_alt_text <- function(vig_path) {
  # Build vignette in temporary directory
  td <- tempfile()
  dir.create(td)

  withr::with_options(list(knitr.duplicate.label = "allow"), {
    rmarkdown::render(input = vig_path, output_dir = td)
  })

  # Get path to it
  compiled_vig_path <- stringr::str_replace_all(basename(vig_path), ".Rmd$", ".html")
  full_path <- file.path(td, compiled_vig_path)

  # Read it in and get images and alt text
  vig_html <- rvest::read_html(full_path)

  images <- rvest::html_elements(vig_html, "img")
  alt_text <- rvest::html_attr(images, "alt")

  alt_text
}

detect_problem_words <- function(md) {
  problem_words <- c("[Ss]imply", "[Nn]aturally", "[Mm]ere", "[Oo]bvious", "[Tt]rivial")
  problem_regex <- paste0(
    problem_words,
    collapse = "|"
  )
  out <- lapply(md, function(x) {
    stringr::str_detect(x, problem_regex)
  })

  md[out == TRUE]
}

#' Get number of words in a markdown chunk
#'
#' @param md Markdown chunk
#' @keywords internal
get_length <- function(md) {
  token_counts <- lapply(md, function(x) length(tokens(x[[1]])[[1]]))
  list(overall = sum(unlist(token_counts)), sections = token_counts)
}

#' Get Flesch-Kincaid readability score
#'
#' @param code List of code chunks
#' @keywords internal
get_fk_score <- function(code) {
  chunk_scores <- lapply(code, function(x) {
    suppressWarnings(textstat_readability(x)$Flesch)
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
#'
#' @keywords internal
clean_chunks <- function(chunk) {
  no_links <- remove_links(chunk)
  no_code <- remove_code(no_links)
  no_bullets <- remove_bullets(no_code)
  no_pipe_tables <- remove_pipe_tables(no_bullets)
  stringr::str_trim(no_pipe_tables)
}

#' Remove pipe table from a chunk
#'
#' Remove any pipe tables in markdown code
#'
#' @param chunk Code chunk
#' @keywords internal
remove_pipe_tables <- function(chunk) {
  stringr::str_replace_all(chunk, "\\|(.*)\\|", "<TABLE>")
}

#' Remove bullet points from a chunk
#'
#' Remove any bullet points in markdown format
#'
#' @param chunk Code chunk
#' @keywords internal
remove_bullets <- function(chunk) {
  stringr::str_remove_all(chunk, "\\*")
}

#' Remove links from a chunk
#'
#' Remove any links in markdown format
#'
#' @param chunk Code chunk
#' @keywords internal
remove_links <- function(chunk) {
  stringr::str_replace_all(chunk, "\\[([^\\[]+)\\]\\(.*?\\)", "<HYPERLINK>")
}

#' Remove any code in backticks from a chunk
#'
#' @param chunk Code chunk
#' @keywords internal
remove_code <- function(chunk) {
  stringr::str_replace_all(chunk, "`{1,3}[^`]+`{1,3}", "<CODE>")
}

#' Parse a vignette
#'
#' @param vig_path Path to vignette
#' @return List of length 1 character vectors containing contents of each markdown section
#' @keywords internal
parse_vignette <- function(vig_path) {
  vig_lines <- readLines(vig_path)

  no_headers <- stringr::str_replace_all(
    vig_lines,
    "^#.*",
    "<SECTION>"
  )

  one_chunk <- paste(no_headers, collapse = " ")

  not_cleaned <- divide_by_section(one_chunk)

  no_code <- remove_code(one_chunk)
  no_vignette_headers <- stringr::str_remove_all(no_code, "---.*?---")

  no_links <- remove_links(no_vignette_headers)
  no_bullets <- remove_bullets(no_links)
  no_pipe_tables <- remove_pipe_tables(no_bullets)
  sections <- divide_by_section(no_pipe_tables)

  cleaned <- purrr::map(sections, stringr::str_trim)

  out <- list(raw = not_cleaned, cleaned = cleaned)

  out
}

divide_by_section <- function(md_text) {
  no_vignette_headers <- stringr::str_remove_all(md_text, "---.*?---")
  divided <- stringr::str_split(no_vignette_headers, "<SECTION>")[[1]]

  trimmed <- stringr::str_trim(divided)
  trimmed[trimmed != ""]
}
