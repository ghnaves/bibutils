#' Extract the Year from a BibTeX Entry
#'
#' Extracts the first four-digit year from a date-like field in a BibTeX entry.
#'
#' @param entry_text A character string containing one complete BibTeX entry.
#' @param field A character string with the name of the BibTeX field from
#'   which the year should be extracted. Defaults to `"date"`.
#'
#' @return
#' An integer containing the extracted year. Returns `NA_integer_` if the field
#' is missing, empty, or does not contain a four-digit year.
#'
#' @examples
#' entry_text <- "@dataset{wpp2024, date = {2024-07-01}}"
#' bib_extract_year(entry_text)
#'
#' @export
bib_extract_year <- function(entry_text, field = "date") {
  x <- bib_extract_field(entry_text, field)

  if (is.null(x) || is.na(x) || !nzchar(x)) {
    return(NA_integer_)
  }

  y <- stringr::str_extract(x, "\\d{4}")

  if (is.na(y)) {
    return(NA_integer_)
  }

  as.integer(y)
}
