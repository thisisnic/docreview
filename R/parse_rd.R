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
