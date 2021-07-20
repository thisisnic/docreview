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
      if (length(split) > 1){

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
