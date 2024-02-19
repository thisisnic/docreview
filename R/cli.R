#' Parse Vignette Results
#'
#' @param results Output of calling vignette analysis function
#' @param config list containing config settings
#'
#' @keywords internal
vignette_results_display <- function(results, config) {
  cli({
    cli_h2("Vignettes")

    if (length(results) == 0) {
      cli_h3("No vignettes detected")
    } else {
      fk <- config$`flesch-kincaid`
      if (!is.null(fk) && fk$active) {
        cli_h3("Flesch Kincaid reading complexity scores")
        fk_scores <- map_dbl(results, "flesch_kincaid")

        bullet_vals <- paste0(names(fk_scores), ": ", round(fk_scores, 2))

        bullet_names <- rep(" ", length(fk_scores))
        bullet_names[fk_scores <= fk$fail] <- "x"
        bullet_names[fk_scores > fk$fail & fk_scores <= fk$warn] <- "!"
        bullet_names[fk_scores > fk$warn] <- "v"

        names(bullet_vals) <- bullet_names

        cli_bullets(bullet_vals)
      }

      lg <- config$length
      if (!is.null(lg) && lg$active) {
        cli_h3("Length scores")

        length_scores <- map_dbl(results, "length")

        bullet_vals <- paste0(names(length_scores), ": ", length_scores, " words.")

        bullet_names <- rep(" ", length(length_scores))
        bullet_names[length_scores >= lg$too_long$fail | length_scores <= lg$too_short$fail] <- "x"

        bullet_names[(length_scores < lg$too_long$fail & length_scores >= lg$too_long$warn) |
          (length_scores > lg$too_short$fail & length_scores <= lg$too_short$warn)] <- "!"

        bullet_names[bullet_names == " "] <- "v"

        names(bullet_vals) <- bullet_names

        cli_bullets(bullet_vals)
      }

      alt_checks <- config$image_alt_text
      if (!is.null(alt_checks) && alt_checks$active) {
        cli_h3("Image alt text")
        iwalk(results, get_image_vignette_cli, min_chars = alt_checks$min_chars)
      }
    }
  })
}

#' Parse function documentation analysis results
#'
#' @param results Results of function documentation analysis
#' @keywords internal
function_results_display <- function(results, config) {
  examples <- as.character(results$exports_examples)
  names(examples) <- names(results$exports_examples)

  if (config$active) {
    cli({
      cli_h2("Function Documentation")

      if (config$exports_without_examples$active) {
        cli_h3("Exported functions containing examples in their documentation: ")

        examples[examples == "FALSE"] <- "x"
        examples[examples == "TRUE"] <- "v"

        exports <- names(examples)
        names(exports) <- examples

        cli_bullets(exports)
      }
    })
  }
}

get_image_vignette_cli <- function(vignette_res, name, min_chars) {
  imgs <- vignette_res$image_alt_text
  n_imgs <- length(imgs)

  cli_text(
    paste(
      "Total # images in", name, ":", length(imgs)
    )
  )

  if (n_imgs > 0) {
    failed_images <- imgs[nchar(imgs) < min_chars]
    cli_text(
      paste(
        "Total # images in", name, "containing alt text: ", n_imgs - length(failed_images)
      )
    )

    # Blank field is as bad as no field
    imgs[is.na(imgs)] <- ""

    bullet_names <- map_chr(imgs, ~ ifelse(.x > min_chars, "v", "x"))

    bullets <- paste("Image", seq_along(imgs))
    names(bullets) <- bullet_names

    cli_bullets(bullets)
  }
}
