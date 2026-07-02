#' Create a Short In-text Citation
#'
#' Creates a short author-year citation from a BibTeX author field and year.
#'
#' @param author A character string containing the BibTeX `author` field.
#' @param source_year An integer or character representing the publication year.
#'
#' @return A character string such as `"Givisiez (2025)"`,
#'   `"Givisiez & Oliveira (2025)"`, or `"Olson et al. (2025)"`.
#'
#' @export
bib_make_citation_short <- function(author, source_year) {

  if (is.na(author) || is.na(source_year)) {
    return(NA_character_)
  }

  authors <- bib_split_authors(author)

  authors_short <- purrr::map_chr(
    authors,
    bib_format_author_short
  )

  citation <- dplyr::case_when(
    length(authors_short) == 1 ~ authors_short[1],
    length(authors_short) == 2 ~ paste(authors_short, collapse = " & "),
    length(authors_short) == 3 ~ paste0(
      authors_short[1], ", ",
      authors_short[2], " & ",
      authors_short[3]
    ),
    length(authors_short) > 3 ~ paste0(authors_short[1], " et al."),
    TRUE ~ NA_character_
  )

  paste0(citation, " (", source_year, ")")
}
