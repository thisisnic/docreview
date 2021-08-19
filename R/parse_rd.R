#' Get RD files from a package
#'
#' @param path Path to package
#' @noRd
find_rd_files <- function(path = ".") {
  man_path <- file.path(path, "man")
  list.files(man_path, ".Rd", full.names = TRUE)
}

#' Extract the vaiour components from an .Rd file
#' @noRd
extract_rd_components <- function(rd){
  list(
    name = rd_get_name(rd),
    aliases = rd_get_aliases(rd),
    examples = rd_get_example_code(rd),
    args = rd_get_argument_name(rd),
    usage = rd_get_usage(rd)
  )
}
  # list of named vectors, each containint the name of the .Rd file, the alises documented within, and the examples
#' Get example code from parsed RD
#'
#' @keywords internal
#' @import utils
rd_get_example_code <- utils::getFromNamespace(".Rd_get_example_code", "tools")

rd_get_name <- utils::getFromNamespace(".Rd_get_name", "tools")

rd_get_argument_name <- utils::getFromNamespace(".Rd_get_argument_names", "tools")

rd_get_section <- utils::getFromNamespace(".Rd_get_section", "tools")

rd_get_text <- utils::getFromNamespace(".Rd_get_text", "tools")

rd_get_usage <- function(rd){
  usage <- rd_get_section(rd, "usage")
  one_string <- paste(as.character(usage), collapse = "")
  no_start <- gsub("^\\\\usage\\{", "", one_string)
  no_end <- gsub("}$", "", no_start)
  # gets rid of weird multiline methods
  no_methods1 <- gsub("\\\\method\\{.*?\\}\\{.*?\\}\\(.*(\\)|\\(\\()[^,]", "", no_end)
  exprs <- rlang::parse_exprs(no_methods1)

  as.character(exprs)

}

rd_get_aliases <- function(rd){
  x <- rd[tools:::RdTags(rd) == "\\alias"]
  unlist(x)
}
