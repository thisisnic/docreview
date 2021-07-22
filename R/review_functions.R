#' Review functions
#'
#' @param path Path to package
#'
#' @export
review_functions <- function(path){

  # - do all exported functions have examples
 # - are all parameters covered by examples
 # - readability of "details" sections
  out <- list()
  out$exports_without_examples <- get_exports_without_examples(path)
  parse_function_results(out)
}


parse_function_results <- function(function_results){

  no_examples <- function_results$exports_without_examples

  cli({
        cli_h2("Function Documentation")
        if (length(no_examples) > 0) {
          cli_alert_warning("The following exported functions do not contain examples in their documentation:")
          names(no_examples) <- rep("*", length(no_examples))
          cli_bullets(no_examples)
        } else {
          cli_alert_success("All exported functions contain examples in their documentation")
        }
  })
}



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

get_example_code_from_rd <- utils::getFromNamespace(".Rd_get_example_code", "tools")

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


#' Parse a function call and divide up by parameters
#'
#' Take a function call and split it up so we have each parameter expression separate
#' Apparently bracket matching can't be done using regex so this manual approach is needed
#'
#' @param expr_text Function call expression text
#' @param func_name Function name
#'
#' @examples
#' # First param is func call
#' parse_example("match_arrow(Scalar$create(\"Mazda RX4 Wag\"), cars_tbl$name)", "match_arrow")
#'
#' # Can handle inline comments
#' parse_example("match_arrow(Scalar$create(4), cars_tbl$cyl) # 0-indexed", "match_arrow")
#'
#' # First param is func call containing vector
#' parse_example("match_arrow(Array$create(c(4, 6, 8)), cars_tbl$cyl)", "match_arrow")
#'
#' # Second param is func call containing vector
#' parse_example("match_arrow(cars_tbl$cyl, Array$create(c(4, 6, 8)))", "match_arrow")
parse_example <- function(expr_text, func_name) {

  # strip out any inline comments
  expr_text <- stringr::str_extract_all(expr_text, paste0(func_name, "\\(.*\\)"))

  # remove actual function name
  text <- stringr::str_remove(expr_text, func_name)

  # remove opening and closing brackets
  text <- stringr::str_remove(text, "\\(")
  text <- stringr::str_remove(text, "\\)$")

  # Extract parameters
  params = list()
  pos = 1
  curr_start = 1
  bracket_count = 0
  while (pos < nchar(text)) {
    curr_char <- stringr::str_sub(text, start = pos, end = pos)
    if (curr_char == "," && bracket_count == 0) {
      params <- append(params, stringr::str_sub(text, start = curr_start, end = pos - 1))
      curr_start <- pos + 1
    } else if (curr_char == "(") {
      bracket_count = bracket_count + 1
    } else if (curr_char == ")") {
      bracket_count = bracket_count - 1
    }
    pos <- pos + 1
  }
  if (pos == nchar(text)) {
    params <- append(params, stringr::str_sub(text, start = curr_start, end = pos))
  }
  return(stringr::str_trim(params))
}

rd_path <- "../arrow/r/man/match_arrow.Rd"
func_args <- get_function_args(rd_path)
args_used <- parse_example("match_arrow(cars_tbl$cyl, Array$create(c(4, 6, 8)))", "match_arrow")

name_args <- function(args_used, func_args){

  remaining_args <- func_args
  arg_names <- vector("character")

  for (arg in args_used) {
    if (length(remaining_args) > 0) {
      split <- stringr::str_split(arg, "(=|<-)")[[1]]
      if (length(split) > 1) {

      }

    }
  }

}
#' # returns TRUE
#' check_assigned("x <- Array$create(c(2, 4, 6))")
#'
#' # returns FALSE
#' check_assigned("Array$create(c(1,2), type = int8())")
check_assigned <- function(code, func_args){

  # check if it's a named argument that's been assigned
  named_args_detected <- detect_named_args(code, func_args)

  if (!named_args_detected) {
    detect_unnamed_args(code)
  }

}

assignment_regex <- "(=|<-)"

detect_named_args <- function(code, func_args){
  stringr::str_detect(
    code,
    paste0(
      "(",
      # function arguments other than dots
      paste0(func_args[func_args != "..."], collapse = "|"),
      ")",
      # 0 or more whitespace characters
      "[:blank:]*",
      assignment_regex
    )
  )
}

detect_unnamed_args <- function(code){
  stringr::str_detect(code, "[[:alnum:][:blank:]\\.\\_]*(=|<-)")
}
