#' Create a short in-text citation from BibTeX author and year.
#'
#' @param author A character string containing the cleaned BibTeX `author`
#'   field. Multiple authors may be separated by `" and "`, `" e "`, or
#'   `" & "`.
#' @param source_year An integer or character representing the publication
#'   year.
#' @param author_is_literal Logical. If `TRUE`, `author` is treated as a
#'   literal institutional or corporate author and is not reduced to a
#'   surname. This is useful for BibTeX authors originally protected with
#'   braces, such as \preformatted{`{CIESIN Columbia University}`}.
#'
#' @return A character string containing a short citation in the form:
#'   `"Silva (2020)"`, `"Silva & Souza (2020)"`,
#'   `"Silva, Souza & Pereira (2020)"`, or `"Silva et al. (2020)"`.
#'
#' @examples
#' bib_make_citation_short("Silva, João", 2020)
#' bib_make_citation_short("Silva, João and Souza, Maria", 2020)
#' bib_make_citation_short("CIESIN Columbia University", 2018, author_is_literal = TRUE)
#'
#' @export
bib_make_citation_short <- function(author,
                                    source_year,
                                    author_is_literal = FALSE) {

  if (is.na(author) || is.na(source_year)) {
    return(NA_character_)
  }

  author <- stringr::str_squish(author)

  if (isTRUE(author_is_literal)) {
    return(paste0(author, " (", source_year, ")"))
  }

  authors <- stringr::str_split(
    author,
    "\\s+(?:and|e|&)\\s+"
  )[[1]] |>
    stringr::str_squish()

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
