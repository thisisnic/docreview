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
  rd_paths <- get_rd_files(pkg_path)
  purrr::map(rd_paths, ~get_example(.x))
}

#' Get RD files from a package
#'
#' @param pkg_path Path to package
get_rd_files <- function(pkg_path){
  man_path <- file.path(pkg_path, "man")
  rd_files <- list.files(man_path, ".Rd")
  rd_paths <- file.path(man_path, rd_files)
  names(rd_paths) <- rd_files
  rd_paths
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

#' Get the RD file containing documentation for a function
get_rd_filename_for_function <- function(pkg_path, function_name){
  rd_files <- get_rd_files(pkg_path)
  matches <- lapply(rd_files, function(x){
    fns <- get_function_names(x)
    aliases <- get_aliases(x)
    function_name %in% fns || function_name %in% aliases
  })
  return(names(matches[matches == TRUE]))
}

get_aliases <- function(rd_path){
  rd <- tools::parse_Rd(rd_path)
  alias_tags <- rd[tools:::RdTags(rd) == "\\alias"]
  vapply(alias_tags, function(x){
    x[[1]][1]
  }, character(1))
}


#' Get the names of all functions documented in this RD file
#'
#' @param rd_path
get_function_names <- function(rd_path){
  rd <- tools::parse_Rd(rd_path)
  unique(c(tools:::.Rd_get_name(rd), get_aliases(rd_path)))
}

#' Get the arguments of all functions documented in this RD file
#'
#' @param rd_path
get_function_args <- function(rd_path){
  rd <- tools::parse_Rd(rd_path)
  tools:::.Rd_get_argument_names(rd)
}


