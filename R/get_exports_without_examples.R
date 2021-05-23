#' Get exports which don't have any examples
#'
#' @param path Path to package
#'
#' @export
get_exports_without_examples <- function(path) {

  examples <- get_examples(path)

  has_examples <- purrr::map_lgl(examples, ~length(.x) > 0)

  no_examples <- names(has_examples[has_examples == FALSE])

  raw_names <- gsub(".Rd", "", no_examples)

  exports <- get_exports(path)

  raw_names[raw_names %in% exports]

}

#' Get examples from a package
#'
#' @param pkg_path Path to package
get_examples <- function(pkg_path) {
  man_path <- file.path(pkg_path, "man")
  rd_files <- list.files(man_path, ".Rd")
  rd_paths <- file.path(man_path, rd_files)
  names(rd_paths) <- rd_files

  purrr::map(rd_paths, ~get_example(.x))
}

get_example_code_from_rd <- utils::getFromNamespace (".Rd_get_example_code", "tools")

#' Get example from RD file
#'
#' @param rd_path Path to RD file
get_example <- function(rd_path) {
  rd <- tools::parse_Rd(rd_path)
  get_example_code_from_rd(rd)
}


#' Get exports
#'
#' @param path Package root
#' @return Exported functions, character vector
get_exports <- function(path) {
  ns_path <- file.path(path, "NAMESPACE")
  ns <- readLines(ns_path)
  exports <- ns[grep("export", ns)]
  return(gsub("(export\\()|())", "", exports))
}
