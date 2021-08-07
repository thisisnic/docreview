#' Get exports which don't have any examples
#'
#' @param path Path to package
#'
#' @export
get_exports_without_examples <- function(path = ".") {

  examples <- find_examples(path)

  has_examples <- purrr::map_lgl(examples, ~length(.x) > 0)

  no_examples <- names(has_examples[has_examples == FALSE])

  raw_names <- gsub(".Rd", "", no_examples)

  exports <- find_exported_functions(path)

  raw_names[raw_names %in% exports]

}

#' Find vignettes
#'
#' Find all vignettes in a package
#'
#' @param path Path to package
#' @export
find_vignettes <- function(path = "."){
  vig_path <- file.path(path, "vignettes")
  list.files(vig_path, ".Rmd", full.names = TRUE)
}

#' Get exports
#'
#' @param path Package root
#' @return Exported functions, character vector
#' @export
find_exported_functions <- function(path = ".") {
  ns_path <- file.path(path, "NAMESPACE")
  ns <- readLines(ns_path)
  exports <- ns[grep("export", ns)]
  return(gsub("(export\\()|())", "", exports))
}

#' Get examples from a package
#'
#' @param pkg_path Path to package
#' @export
find_examples <- function(path = ".") {
  rd_paths <- find_rd_files(path)
  purrr::map(rd_paths, ~get_example(.x))
}

#' Get RD files from a package
#'
#' @param path Path to package
#' @export
find_rd_files <- function(path = "."){
  man_path <- file.path(path, "man")
  rd_files <- list.files(man_path, ".Rd")
  rd_paths <- file.path(man_path, rd_files)
  names(rd_paths) <- rd_files
  rd_paths
}
