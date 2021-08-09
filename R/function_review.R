#' Review functions
#'
#' @param path Path to package
#' @param thresholds List of thresholds that result in fails or warnings
#' @export
#' @examples
#' pkg_path <- system.file("testpkg", package = "docreview")
#' function_review(pkg_path)
function_review <- function(path, thresholds = default_thresholds()$functions) {
  detailed_results <- list(
    exports_examples = find_exports_without_examples(path)
  )

  comments <- function_get_comments(detailed_results, thresholds)

  list(failures = comments$fail, warnings = comments$warn, details = detailed_results)
}

#' Count failures and warnings for function review
#'
#' @param results Results of function review
#' @param thresholds List of thresholds that result in fails or warnings
#' @keywords internal
function_get_comments <- function(results, thresholds) {
  comments <- list(fail = 0, warn = 0)

  # Count failures and warnings for exports without examples
  need_examples <- sum(!results$exports_examples)

  if (need_examples > 0) {
    if (thresholds$exports_without_examples == "fail") {
      comments$fail <- comments$fail + need_examples
    } else if (thresholds$exports_without_examples == "warn") {
      comments$warn <- comments$warn + need_examples
    }
  }

  comments
}

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

