#' Extract Citation Information
#'
#' Extracts the citation key together with short and full citation strings
#' from a bibliographic record created by \code{bib_entry_to_tibble()}.
#'
#' @param x A one-row data frame or tibble containing the bibliographic
#'   metadata. The object must include the variables
#'   `source_key`, `author`, `source_year`, `title`, `publisher`,
#'   `doi`, and `url`.
#'
#' @return
#' A named list with three elements:
#' \describe{
#'   \item{key}{The bibliographic key (`source_key`).}
#'   \item{short}{A short author--year citation.}
#'   \item{full}{A complete citation string.}
#' }
#'
#' @details
#' This function is a convenience wrapper around
#' \code{\link{bib_make_citation_short}} and
#' \code{\link{bib_make_citation_full}}. It is intended for situations where
#' both citation formats and the corresponding source key are needed, such as
#' metadata generation or database registration.
#'
#' @examples
#' x <- tibble::tibble(
#'   source_key = "wpp2024",
#'   author = "United Nations",
#'   source_year = 2024,
#'   title = "World Population Prospects 2024",
#'   publisher = "United Nations",
#'   doi = NA_character_,
#'   url = "https://population.un.org/wpp/"
#' )
#'
#' bib_get_citations(x)
#'
#' @export
bib_get_citations <- function(x) {

  list(
    key = x$source_key,
    short = bib_make_citation_short(
      author = x$author,
      source_year = x$source_year
    ),
    full = bib_make_citation_full(
      author = x$author,
      source_year = x$source_year,
      title = x$title,
      publisher = x$publisher,
      doi = x$doi,
      url = x$url
    )
  )

}
