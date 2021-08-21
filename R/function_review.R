#' Review functions
#'
#' @param path Path to package
#' @param checks Checks to run
function_review <- function(path, checks = get_config()$functions) {
  detailed_results <- list()

  if (checks$exports_without_examples$active) {
    rds <- analyse_rds(path)
    exports_names <- find_exported_functions(path)
    exports <- rds[names(rds) %in% exports_names]

    # Use max here in case this param isn't set
    arity <- max(0, checks$exports_without_examples$missing_examples$min_arity, na.rm = TRUE)

    detailed_results$exports_examples <- map_lgl(
      exports,
      ~ check_examples_arity(
        .x,
        arity
      )
    )
  }

  comments <- function_get_comments(detailed_results, checks)

  list(failures = comments$fail, warnings = comments$warn, details = detailed_results)
}

#' Check that functions have examples
#'
#' Check that functions with arity (number of arguments) greater than or equal
#' to the specified value are accompanied by examples in their docs
#'
#' @param func List of function properties
#' @param min_arity Only return `TRUE` when no examples if arity >= `min_arity`
#' @return `TRUE` if function has sufficient examples or `FALSE` if not
#' @noRd
check_examples_arity <- function(func, min_arity) {
  !(length(func$examples) == 0 && length(func$args) >= min_arity)
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
