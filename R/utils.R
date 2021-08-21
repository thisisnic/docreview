#' See if any checks requires HTML docs to be built
#' @noRd
html_required <- function(checks) {
  html_req_checks <- unlist(map(checks, "html_required"))
  any(html_req_checks)
}

#' See if any .Rmd files are missing .html files and vice versa
#' @noRd
check_vignettes_built <- function(path) {
  html_files <- list.files(file.path(path, "vignettes"), ".html")
  rmd_files <- list.files(file.path(path, "vignettes"), ".Rmd")

  too_many_html <- setdiff(
    gsub(".html", "", html_files),
    gsub(".Rmd", "", rmd_files)
  )

  not_enough_html <- setdiff(
    gsub(".Rmd", "", rmd_files),
    gsub(".html", "", html_files)
  )

  if (length(too_many_html) > 0) {
    rlang::abort(
      message = c(
        "There must be a 1:1 mapping between `.html` and `.Rmd` files in vignette directory of target package.",
        x = paste("Extra HTML files: ", paste(paste0(too_many_html, ".html"), collapse = ", "))
      )
    )
  }

  if (length(not_enough_html) > 0) {
    rlang::abort(
      message = c(
        "There must be a 1:1 mapping between `.html` and `.Rmd` files in vignette directory of target package.",
        x = paste("Rmd files without html files: ", paste(paste0(not_enough_html, ".Rmd"), collapse = ", "))
      )
    )
  }
}
