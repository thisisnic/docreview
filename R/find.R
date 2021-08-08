#' Get exports which don't have any examples
#'
#' @param path Path to package
#' @return Logical vector of whether exported functions have examples
#' @keywords internal
get_exports_without_examples <- function(path = ".") {
  examples <- find_examples(path)

  # TODO: refactor so we're not assuming every Rd filename matches given that some .Rds contain multiple functions
  names(examples) <- gsub(".Rd", "", names(examples))

  has_examples <- map_lgl(examples, ~ length(.x) > 0)

  exports <- find_exported_functions(path)

  has_examples[names(has_examples) %in% exports]
}

#' Find vignettes
#'
#' Find all vignettes in a package
#'
#' @param path Path to package
#' @keywords internal
find_vignettes <- function(path = ".") {
  vig_path <- file.path(path, "vignettes")
  list.files(vig_path, ".Rmd", full.names = TRUE)
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

#' Get examples from a package
#'
#' @param path Path to package
#' @keywords internal
find_examples <- function(path = ".") {
  rd_paths <- find_rd_files(path)
  map(rd_paths, ~ get_example(.x))
}

#' Get RD files from a package
#'
#' @param path Path to package
#' @keywords internal
find_rd_files <- function(path = ".") {
  man_path <- file.path(path, "man")
  rd_files <- list.files(man_path, ".Rd")
  rd_paths <- file.path(man_path, rd_files)
  names(rd_paths) <- rd_files
  rd_paths
}
