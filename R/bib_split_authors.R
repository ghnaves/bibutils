#' Split BibTeX Authors
#'
#' Splits a BibTeX author field into individual authors, using `and` as the
#' separator while preserving braces around literal authors.
#'
#' @param author A character string containing a BibTeX `author` field.
#'
#' @return A character vector with one element per author.
#'
#' @keywords internal
bib_split_authors <- function(author) {

  author <- stringr::str_squish(author)

  if (startsWith(author, "{") && endsWith(author, "}")) {
    author <- substring(author, 2, nchar(author) - 1)
  }

  stringr::str_split(author, "\\s+and\\s+")[[1]] |>
    stringr::str_squish()
}
