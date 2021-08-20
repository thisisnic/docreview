#' Review functions
#'
#' @param path Path to package
#' @param checks Checks to run
function_review <- function(path, checks = get_config()$functions) {
  detailed_results <- list()

  if (checks$exports_without_examples$active) {
    detailed_results$exports_examples <- find_exports_without_examples(path)
  }

  comments <- function_get_comments(detailed_results, checks)

  list(failures = comments$fail, warnings = comments$warn, details = detailed_results)
}

#' Count failures and warnings for function review
#'
#' @param results Results of function review
#' @param thresholds List of thresholds that result in fails or warnings
#' @keywords internal
function_get_comments <- function(results, checks) {
  comments <- list(fail = 0, warn = 0)

  if (checks$exports_without_examples$active) {
    # Count failures and warnings for exports without examples
    need_examples <- sum(!results$exports_examples)

    if (need_examples >= checks$exports_without_examples$missing_examples$fail) {
      comments$fail <- comments$fail + need_examples
    } else if (need_examples >= checks$exports_without_examples$missing_examples$warn) {
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
  get_example_code_from_rd(rd)
}
