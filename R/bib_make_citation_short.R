#' Create a Short Citation
#'
#' Generates a short author--year citation from a BibTeX author field and
#' publication year. The function extracts the first author (or institution)
#' and returns a citation in the format `"Author (Year)"`.
#'
#' @param author A character string containing the BibTeX `author` field.
#'   Multiple authors should be separated by `" and "`, following the BibTeX
#'   convention.
#' @param source_year An integer or character representing the publication
#'   year.
#'
#' @return
#' A character string containing the short citation. Returns `NA_character_`
#' if either `author` or `source_year` is missing.
#'
#' @details
#' The function assumes that the `author` field follows the BibTeX format.
#' Only the first author is used in the citation. For personal names written
#' as `"Surname, Given"` the surname is extracted. Institutional authors
#' (e.g., `"IBGE"` or `"United Nations"`) are returned unchanged.
#'
#' @examples
#' bib_make_citation_short(
#'   author = "United Nations",
#'   source_year = 2024
#' )
#' # "United Nations (2024)"
#'
#' bib_make_citation_short(
#'   author = "Smith, John and Doe, Jane",
#'   source_year = 2022
#' )
#' # "Smith (2022)"
#'
#' @export
bib_make_citation_short <- function(author, source_year) {

  if (is.na(author) || is.na(source_year))
    return(NA_character_)

  first_author <- stringr::str_split(
    author,
    "\\s+and\\s+"
  )[[1]][1]

  surname <- first_author |>
    stringr::str_squish() |>
    stringr::str_replace(",.*$", "") |>
    stringr::str_split("\\s+") |>
    purrr::pluck(1)

  paste0(surname, " (", source_year, ")")
}
