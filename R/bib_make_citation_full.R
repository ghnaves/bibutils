#' Create a Full Citation
#'
#' Generates a complete citation string from bibliographic metadata extracted
#' from a BibTeX entry. The output is intended for use in metadata, reports,
#' or documentation, rather than as a formally formatted reference according
#' to a specific citation style (e.g., APA, ABNT, Chicago).
#'
#' @param author A character string containing the BibTeX `author` field.
#' @param source_year An integer or character representing the publication
#'   year.
#' @param title A character string containing the publication title.
#' @param publisher An optional character string containing the publisher or
#'   organization.
#' @param doi An optional character string containing the Digital Object
#'   Identifier (DOI).
#' @param url An optional character string containing the publication URL.
#'
#' @return
#' A character string containing the complete citation. Missing fields are
#' omitted from the output.
#'
#' @details
#' The function concatenates the available bibliographic fields into a single
#' readable citation. It does not attempt to conform to any particular
#' bibliographic style, making it suitable for metadata records and internal
#' documentation.
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
    author,
    paste0("(", source_year, ")."),
    title,
    if (!is.na(publisher)) publisher else NA_character_,
    if (!is.na(doi)) paste0("DOI: ", doi) else NA_character_,
    if (!is.na(url)) paste0("Available at: ", url) else NA_character_
  )

  parts <- parts[!is.na(parts) & nzchar(parts)]

  paste(parts, collapse = " ")
}
