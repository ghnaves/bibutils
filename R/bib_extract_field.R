#' Extract a field of a BibTeX entry from a .bib file
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
