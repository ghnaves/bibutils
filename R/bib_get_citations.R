#' Extract Citation Information
#'
#' Extracts the citation key together with short and full citation strings
#' from a bibliographic record created by \code{bib_entry_to_tibble()}.
#'
#'
#' @param x A one-row data frame or tibble containing bibliographic metadata.
#'   The object must include `source_key`, `author`, `source_year`, `title`,
#'   `publisher`, `doi`, and `url`. If available, `author_raw` is used to
#'   preserve BibTeX braces when formatting author names.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{key}{The bibliographic key (`source_key`).}
#'   \item{short}{A short author-year citation.}
#'   \item{full}{A complete citation string.}
#' }
#'
#' @details
#' This function is a convenience wrapper around [bib_make_citation_short()]
#' and [bib_make_citation_full()]. It is intended for situations where both
#' citation formats and the corresponding source key are needed, such as
#' metadata generation or database registration.
#'
#' @examples
#' x <- tibble::tibble(
#'   source_key = "wpp2024",
#'   author = "United Nations",
#'   author_raw = "{{United Nations}}",
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

  author_for_citation <- if (
    "author_raw" %in% names(x) &&
    !is.na(x$author_raw)
  ) {
    x$author_raw
  } else {
    x$author
  }

  list(
    key = x$source_key,
    short = bib_make_citation_short(
      author = author_for_citation,
      source_year = x$source_year
    ),
    full = bib_make_citation_full(
      author = author_for_citation,
      source_year = x$source_year,
      title = x$title,
      publisher = x$publisher,
      doi = x$doi,
      url = x$url
    )
  )
}
