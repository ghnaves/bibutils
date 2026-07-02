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
#' sample_file = system.file("extdata", "sample_source.bib", package = "bibutils")
#' entry_text <- bib_read_entry(sample_file, 1)
#' bib_extract_field(entry_text, "title")
#' bib_extract_field(entry_text, "author")
#' }
#'
#' @export
bib_extract_field <- function(entry_text, field) {
  pattern <- paste0(field, "\\s*=\\s*")
  loc <- regexpr(pattern, entry_text, ignore.case = TRUE, perl = TRUE)

  if (loc[1] == -1) {
    return(NA_character_)
  }

  start <- loc[1] + attr(loc, "match.length")
  txt <- substr(entry_text, start, nchar(entry_text))
  txt <- sub("^\\s+", "", txt)

  first_char <- substr(txt, 1, 1)

  if (first_char == "\"") {
    chars <- strsplit(substr(txt, 2, nchar(txt)), "")[[1]]
    end_idx <- NA_integer_

    for (i in seq_along(chars)) {
      if (chars[i] == "\"" && chars[max(1, i - 1)] != "\\") {
        end_idx <- i
        break
      }
    }

    if (is.na(end_idx)) {
      return(NA_character_)
    }

    raw_val <- substr(txt, 2, end_idx)
    return(bib_clean_text(raw_val))
  }

  if (first_char == "{") {
    chars <- strsplit(txt, "")[[1]]
    level <- 0
    end_idx <- NA_integer_

    for (i in seq_along(chars)) {
      if (chars[i] == "{") level <- level + 1
      if (chars[i] == "}") level <- level - 1

      if (level == 0) {
        end_idx <- i
        break
      }
    }

    if (is.na(end_idx)) {
      return(NA_character_)
    }

    raw_val <- substr(txt, 1, end_idx)
    return(bib_clean_text(raw_val))
  }

  raw_val <- sub(",.*$", "", txt)
  bib_clean_text(raw_val)
}
