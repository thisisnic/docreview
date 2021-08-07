#' Get example from RD file
#'
#' @param rd_path Path to RD file
#' @keywords internal
get_example <- function(rd_path) {
  rd <- tools::parse_Rd(rd_path)
  get_example_code_from_rd(rd)
}

#' Get example code from parsed RD
#'
#' @keywords internal
get_example_code_from_rd <- utils::getFromNamespace(".Rd_get_example_code", "tools")


rd_get_name <- utils::getFromNamespace(".Rd_get_name", "tools")

rd_get_argument_name <- utils::getFromNamespace(".Rd_get_argument_names", "tools")


#' Get the names of all functions documented in this RD file
#'
#' @param rd_path Path to RD file
#' @keywords internal
get_function_names <- function(rd_path){
  rd <- tools::parse_Rd(rd_path)


  unique(c(rd_get_name(rd), get_aliases(rd_path)))
}

#' Get the arguments of all functions documented in this RD file
#'
#' @param rd_path Path to RD file
#'
#' @keywords internal
get_function_args <- function(rd_path){
  rd <- tools::parse_Rd(rd_path)
  rd_get_argument_name(rd)
}

#' Get all alias tags from RD file
#'
#' @param rd_path Path to RD file
#' @keywords internal
get_aliases <- function(rd_path){
  rd <- tools::parse_Rd(rd_path)
  alias_tags <- rd[tools:::RdTags(rd) == "\\alias"]
  vapply(alias_tags, function(x){
    x[[1]][1]
  }, character(1))
}
