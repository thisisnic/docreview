#' Extract the various components from an .Rd file
#' @noRd
extract_rd_components <- function(rd){
  list(
    name = rd_get_name(rd),
    aliases = rd_get_aliases(rd),
    examples = rd_get_examples(rd),
    args = rd_get_argument_name(rd),
    usage = rd_get_usage(rd)
  )
}

rd_get_examples <- function(rd){
  examples <- rd_get_example_code(rd)
  # Add back in this functionality when we've worked out how to handle e.g. dplyr::explain where there is \donttest
  #paste(rlang::parse_exprs(examples))
  examples
}

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
  # remove any line which starts with "\method{name}{name}()"
  no_methods <- gsub("\\\\method\\{.*?\\}\\{.*?\\}\\(.*(\\)|\\(\\()[^,]", "", no_end)
  exprs <- rlang::parse_exprs(no_methods)
  as.character(exprs)
}

rd_get_aliases <- function(rd){
  x <- rd[tools:::RdTags(rd) == "\\alias"]
  unlist(x)
}
