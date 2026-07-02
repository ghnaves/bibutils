#' Create a Full Citation
#'
#' Generates a complete citation string from bibliographic metadata extracted
#' from a BibTeX entry. The output is intended for metadata, reports, and
#' documentation rather than for a formally formatted reference according to a
#' specific citation style, such as APA, ABNT, or Chicago.
#'
#' @param author A character string containing the cleaned BibTeX `author`
#'   field.
#' @param source_year An integer or character representing the publication
#'   year.
#' @param title A character string containing the publication title.
#' @param publisher An optional character string containing the publisher,
#'   institution, or organization responsible for the resource.
#' @param doi An optional character string containing the Digital Object
#'   Identifier (DOI).
#' @param url An optional character string containing the publication URL.
#'
#' @return A character string containing the complete citation. Missing or
#'   empty fields are omitted from the output.
#'
#' @details
#' The function concatenates the available bibliographic fields into a single
#' readable citation. It does not attempt to conform to a particular
#' bibliographic style.
#'
#' @examples
#' bib_make_citation_full(
#'   author = "United Nations",
#'   source_year = 2024,
#'   title = "World Population Prospects 2024",
#'   publisher = "United Nations",
#'   url = "https://population.un.org/wpp/"
#' )
#'
#' bib_make_citation_full(
#'   author = "Hijmans, Robert J.",
#'   source_year = 2023,
#'   title = "terra: Spatial Data Analysis",
#'   doi = "10.32614/CRAN.package.terra"
#' )
#'
#' @export
bib_make_citation_full <- function(author,
                                   source_year,
                                   title,
                                   publisher = NA_character_,
                                   doi = NA_character_,
                                   url = NA_character_) {

  parts <- c(
    if (!is.na(author) && nzchar(author)) author else NA_character_,
    if (!is.na(source_year) && nzchar(source_year)) {
      paste0("(", source_year, ").")
    } else {
      NA_character_
    },
    if (!is.na(title) && nzchar(title)) paste0(title, ".") else NA_character_,
    if (!is.na(publisher) && nzchar(publisher)) paste0(publisher, ".") else NA_character_,
    if (!is.na(doi) && nzchar(doi)) paste0("DOI: ", doi, ".") else NA_character_,
    if (!is.na(url) && nzchar(url)) paste0("Available at: ", url, ".") else NA_character_
  )

  parts <- parts[!is.na(parts)]

  paste(parts, collapse = " ")
}
