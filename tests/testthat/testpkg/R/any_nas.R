#' Count NAs in vectors
#'
#' @param ... Vectors
#' @return Are there any NAs in any vectors?
any_nas <- function(...){
  values <- list(...)
  any(is.na(unlist(values)))
}
