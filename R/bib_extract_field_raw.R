#' Extract a Raw Field from a BibTeX Entry
#'
#' Extracts the raw value of a field from a single BibTeX entry without
#' cleaning or removing BibTeX delimiters.
#'
#' @param entry_text A character string containing one complete BibTeX entry.
#' @param field A character string with the name of the BibTeX field to
#'   extract, such as `"title"`, `"author"`, `"date"`, `"doi"`, or `"url"`.
#'
#' @return
#' A character string containing the raw field value, including its original
#' external delimiters when present. Returns `NA_character_` if the field is
#' not found or cannot be parsed.
#'
#' @details
#' Unlike [bib_extract_field()], this function does not call the internal
#' helper `bib_clean_text()` and preserves BibTeX syntax such as braces and
#' quotes. It is useful when structural information must be retained, for
#' example to identify literal institutional authors written as:
#'
#' \preformatted{
#' "{{CIESIN Columbia University}}"
#' }
#'
#' @examples
#' entry_text <- "@dataset{ciesin2018,
#'   author = {{CIESIN Columbia University}},
#'   title = {Example dataset}
#' }"
#'
#' bib_extract_field_raw(entry_text, "author")
#' # "{{CIESIN Columbia University}}"
#'
#' @export
bib_extract_field_raw <- function(entry_text, field) {

  pattern <- paste0(
    stringr::str_escape(field),
    "\\s*=\\s*"
  )

  loc <- regexpr(
    pattern,
    entry_text,
    ignore.case = TRUE,
    perl = TRUE
  )

  if (loc[1] == -1) {
    return(NA_character_)
  }

  start <- loc[1] + attr(loc, "match.length")
  txt <- substr(entry_text, start, nchar(entry_text))
  txt <- sub("^\\s+", "", txt)

  first_char <- substr(txt, 1, 1)

  if (first_char == "\"") {
    chars <- strsplit(txt, "")[[1]]
    end_idx <- NA_integer_

    for (i in seq_along(chars)[-1]) {
      if (
        chars[i] == "\"" &&
        chars[i - 1] != "\\"
      ) {
        end_idx <- i
        break
      }
    }

    if (is.na(end_idx)) {
      return(NA_character_)
    }

    return(substr(txt, 1, end_idx))
  }

  if (first_char == "{") {
    chars <- strsplit(txt, "")[[1]]
    level <- 0
    end_idx <- NA_integer_

    for (i in seq_along(chars)) {
      if (chars[i] == "{") {
        level <- level + 1
      }

      if (chars[i] == "}") {
        level <- level - 1
      }

      if (level == 0) {
        end_idx <- i
        break
      }
    }

    if (is.na(end_idx)) {
      return(NA_character_)
    }

    return(substr(txt, 1, end_idx))
  }

  trimws(sub(",.*$", "", txt))
}
