#' Extract a Field from a BibTeX Entry
#'
#' Extracts and cleans the value of a field from a single BibTeX entry.
#'
#' @param entry_text A character string containing one complete BibTeX entry.
#' @param field A character string with the name of the BibTeX field to
#'   extract, such as `"title"`, `"author"`, `"date"`, `"doi"`, or `"url"`.
#'
#' @return
#' A cleaned character string containing the field value. Returns
#' `NA_character_` if the field is not found or cannot be parsed.
#'
#' @details
#' This function returns a cleaned value using `bib_clean_text()`. Braces and
#' external quotes are removed. Use `bib_extract_field_raw()` when the original
#' BibTeX structure must be preserved.
#'
#' @examples
#' \dontrun{
#' sample_file <- system.file("extdata", "sample_source.bib", package = "bibutils")
#' entry_text <- bib_read_entry(sample_file, 1)
#' bib_extract_field(entry_text, "title")
#' bib_extract_field(entry_text, "author")
#' }
#'
#' @export
bib_extract_field <- function(entry_text, field) {
  bib_clean_text(
    bib_extract_field_raw(entry_text, field)
  )
}
