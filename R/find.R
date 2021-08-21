#' Get exports which don't have any examples
#'
#' @param path Path to package
#' @return Logical vector of whether exported functions have examples
#' @keywords internal
analyse_rds <- function(path = ".") {
  rd_paths <- find_rd_files(path)
  rd_files <- map(rd_paths, tools::parse_Rd)
  names(rd_files) <- gsub(".Rd", "", basename(rd_paths))
  map(rd_files, extract_rd_components)
}

#' Get RD files from a package
#'
#' @param path Path to package
#' @noRd
find_rd_files <- function(path = ".") {
  man_path <- file.path(path, "man")
  list.files(man_path, ".Rd", full.names = TRUE)
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
  if (length(vigs) == 0) {
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
