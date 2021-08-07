#' Add two numbers together
#'
#' @param x A number
#' @param y Another number
#' @param na.rm Drop NAs?
#' @examples
#' add(1, 2)
#' @export
add <- function(x, y, na.rm = FALSE) {
  if (na.rm || !any(is.na(c(x, y)))) {
    return(x + y)
  } else {
    return(NA)
  }
}
