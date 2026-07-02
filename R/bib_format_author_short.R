#' Format Author for Short Citation
#'
#' Formats one author for a short in-text citation by keeping only the surname,
#' or the full literal name for institutional authors.
#'
#' @param x A character string containing one author.
#'
#' @return A character string with the surname or literal author name.
#'
#' @keywords internal
bib_format_author_short <- function(x) {

  x <- stringr::str_squish(x)

  if (bib_author_part_is_literal(x)) {
    return(
      x |>
        stringr::str_remove("^\\{") |>
        stringr::str_remove("\\}$") |>
        stringr::str_squish()
    )
  }

  if (stringr::str_detect(x, ",")) {
    parts <- stringr::str_split(x, ",", n = 2)[[1]]
    return(stringr::str_squish(parts[1]))
  }

  parts <- stringr::str_split(x, "\\s+")[[1]]

  parts[length(parts)]
}
