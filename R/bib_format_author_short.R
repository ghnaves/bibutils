#' Format a Personal Author Name for Short Citations
#'
#' Converts a personal author name into a compact surname--initials format for
#' use in short citations.
#'
#' @param x A character string containing one personal author name. Names may
#'   follow either the BibTeX-style `"Surname, Given names"` format or the
#'   `"Given names Surname"` format.
#'
#' @return
#' A character string formatted as `"Surname, G."` or, when multiple given
#' names are available, `"Surname, G. M."`. A one-word name is returned
#' unchanged.
#'
#' @details
#' For names containing a comma, the text before the first comma is treated as
#' the surname and the text after it as the given names. For names without a
#' comma, the final word is treated as the surname and all preceding words are
#' converted to initials.
#'
#' This helper is intended for personal author names only. Institutional or
#' corporate authors should be handled separately through
#' \code{\link{bib_author_is_literal}}.
#'
#' @examples
#' \dontrun{
#' bib_format_author_short("Givisiez, Gustavo")
#' # "Givisiez, G."
#'
#' bib_format_author_short("Gustavo Givisiez")
#' # "Givisiez, G."
#'
#' bib_format_author_short("Olson, David M.")
#' # "Olson, D. M."
#'
#' bib_format_author_short("Plato")
#' # "Plato"
#'}
#' @keywords internal
bib_format_author_short <- function(x) {
  x <- stringr::str_squish(x)

  # Caso "Sobrenome, Nome"
  if (stringr::str_detect(x, ",")) {
    parts <- stringr::str_split(x, ",", n = 2)[[1]]
    surname <- stringr::str_squish(parts[1])
    given <- stringr::str_squish(parts[2])

    initials <- given |>
      stringr::str_split("\\s+") |>
      purrr::pluck(1) |>
      stringr::str_sub(1, 1) |>
      paste0(".", collapse = " ")

    return(paste0(surname, ", ", initials))
  }

  # Caso "Nome Sobrenome"
  parts <- stringr::str_split(x, "\\s+")[[1]]

  if (length(parts) == 1) {
    return(parts[1])
  }

  surname <- parts[length(parts)]
  given <- parts[-length(parts)]

  initials <- given |>
    stringr::str_sub(1, 1) |>
    paste0(".", collapse = " ")

  paste0(surname, ", ", initials)
}
