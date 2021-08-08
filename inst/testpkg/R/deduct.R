#' Deduct one number from another
#'
#' @param x A number
#' @param y Another number
#' @param na.rm Drop NAs?
#' @export
deduct <- function(x, y, na.rm = FALSE) {
  if (!na.rm || !any_nas(x,y)) {
    return(x - y)
  } else {
    return(NA)
  }
}
