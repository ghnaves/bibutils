#' Extract year of a BibTeX entry from a .bib file
#'
#' Reads a BibTeX file and returns a numeric of the year of publication
#' for a single entry, either by numeric index
#'
#' @param entry_text a value generate by bib_extract_field function
#' @param field the name of the field to extract the year from (default: "date").
#'
#' @return A integer containing the year
#' @export
#'
#' @examples
#' \dontrun{
#' entry_text = bib_read_entry("data/raw/sample_source.bib", 1)
#' bib_extract_year(entry_text)
#' }

bib_extract_year <- function(entry_text, field = 'date') {
  x = bib_extract_field(entry_text, field)
  if (is.na(x) || is.null(x) || !nzchar(x)) {
    return(NA_integer_)
  }
  y <- stringr::str_extract(x, "\\d{4}")
  if (is.na(y)) return(NA_integer_)
  as.integer(y)
}
