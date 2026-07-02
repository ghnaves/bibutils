#' Format Author for Full Citation
#'
#' Formats one author for a full citation using a surname-and-initials form.
#' Literal institutional or corporate authors are kept in full.
#'
#' @param x A character string containing one author. The author may be in
#'   `"Surname, Given names"` format, `"Given names Surname"` format, or
#'   enclosed in braces as a literal author, such as
#'   \code{\{United Nations\}}.
#'
#' @return
#' A character string formatted as `"Surname, G. H. N."`, or the full literal
#' author name when the author is enclosed in braces.
#'
#' @details
#' This helper is intended for formatting individual authors after a BibTeX
#' `author` field has been split with `bib_split_authors()`.
#'
#' Given names with fewer than four characters are omitted, except when they
#' are already abbreviated with a period, such as `"M."` or `"V."`.
#'
#'
#' @keywords internal
bib_format_author_long <- function(x) {

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
    surname <- stringr::str_squish(parts[1])
    given <- stringr::str_squish(parts[2])
  } else {
    parts <- stringr::str_split(x, "\\s+")[[1]]
    surname <- parts[length(parts)]
    given <- paste(parts[-length(parts)], collapse = " ")
  }

  given_parts <- stringr::str_split(given, "\\s+")[[1]]

  initials <- purrr::map_chr(given_parts, function(z) {

    z <- stringr::str_replace_all(z, "[{}]", "")
    z <- stringr::str_squish(z)

    if (stringr::str_detect(z, "^[[:alpha:]]\\.$")) {
      return(z)
    }

    if (nchar(z) < 4) {
      return(NA_character_)
    }

    paste0(stringr::str_sub(z, 1, 1), ".")
  })

  initials <- initials[!is.na(initials)]

  if (length(initials) == 0) {
    return(surname)
  }

  paste0(surname, ", ", paste(initials, collapse = " "))
}
