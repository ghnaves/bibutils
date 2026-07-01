#' Create a short in-text citation from BibTeX author and year.
#'
#' @param author A character string containing the BibTeX `author` field.
#'   Multiple authors may be separated by `" and "`, `" e "`, or `" & "`.
#' @param source_year An integer or character representing the publication
#'   year.
#'
#' @return A character string containing a short citation in the form:
#'   `"Silva (2020)"`, `"Silva & Souza (2020)"`,
#'   `"Silva, Souza & Pereira (2020)"`, or `"Silva et al. (2020)"`.
#'
#' @export
bib_make_citation_short <- function(author, source_year) {

  if (is.na(author) || is.na(source_year)) {
    return(NA_character_)
  }

  authors <- stringr::str_split(
    author,
    "\\s+(?:and|e|&)\\s+"
  )[[1]] |>
    stringr::str_squish()

  surnames <- authors |>
    stringr::str_replace(",.*$", "") |>
    purrr::map_chr(~ stringr::str_split(.x, "\\s+")[[1]][1])

  citation <- switch(
    as.character(length(surnames)),
    "1" = surnames[1],
    "2" = paste(surnames, collapse = " & "),
    "3" = paste0(
      surnames[1], ", ",
      surnames[2], " & ",
      surnames[3]
    ),
    paste0(surnames[1], " et al.")
  )

  paste0(citation, " (", source_year, ")")
}
