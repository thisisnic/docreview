#' Get exports which don't have any examples
#'
#' @param path Path to package
#' @return Logical vector of whether exported functions have examples
#' @keywords internal
find_exports_without_examples <- function(path = ".") {

  rd_paths <- find_rd_files(path)
  rd_files <- map(rd_paths, tools::parse_Rd)
  names(rd_files) <- basename(rd_paths)
  rds <- map(rd_files, extract_rd_components)

  group_by_all


  # length(rd_files)
  #
  # examples <- find_examples(path)
  #
  # # TODO: refactor so we're not assuming every Rd filename matches given that some .Rds contain multiple functions
  # names(examples) <- gsub(".Rd", "", names(examples))
  #
  # has_examples <- map_lgl(examples, ~ length(.x) > 0)
  #
  # exports <- find_exported_functions(path)
  #
  # has_examples[names(has_examples) %in% exports]
}

#' Find vignettes
#'
#' Find all vignettes in a package
#'
#' @param path Path to package
#' @keywords internal
find_vignettes <- function(path = ".") {
  vig_path <- file.path(path, "vignettes")
  vigs <- list.files(vig_path, ".Rmd", full.names = TRUE)
  if(length(vigs) == 0){
    rlang::warn("Vignette checks enabled but no vignettes detected")
  }
  vigs
}

#' Get exports
#'
#' @param path Package root
#' @keywords internal
#' @return Exported functions, character vector
find_exported_functions <- function(path = ".") {
  ns_path <- file.path(path, "NAMESPACE")
  ns <- readLines(ns_path)
  exports <- ns[grep("export", ns)]
  return(gsub("(export\\()|())", "", exports))
}
