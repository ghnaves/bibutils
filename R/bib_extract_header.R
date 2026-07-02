#' Extract the Header from a BibTeX Entry
#'
#' Extracts the entry type and citation key from the header of a single
#' BibTeX entry.
#'
#' @param entry_text A character string containing one complete BibTeX entry.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{source_type}{The BibTeX entry type, such as `"article"`,
#'   `"book"`, or `"dataset"`.}
#'   \item{source_key}{The BibTeX citation key.}
#' }
#'
#' @examples
#' \dontrun{
#' entry <- "@dataset{ciesin2018, title = {Example}}"
#' bib_extract_header(entry)
#'}
#'
#' @export
bib_extract_header <- function(entry_text) {
  m <- stringr::str_match(entry_text, "^@(\\w+)\\{([^,]+),")

  list(
    source_type = ifelse(is.na(m[, 2]), NA_character_, trimws(m[, 2])),
    source_key  = ifelse(is.na(m[, 3]), NA_character_, trimws(m[, 3]))
  )
}
#' Clean Text Extracted from a BibTeX Field
#'
#' Removes external quotes, balanced external braces, remaining internal
#' braces, and extra whitespace from text extracted from a BibTeX field.
#'
#' @param x A character string extracted from a BibTeX field.
#'
#' @return A cleaned character string. Returns `NA_character_` for missing,
#'   empty, or `NULL` input.
#'
#' @details
#' This function is intended to create clean display values from BibTeX
#' fields. It removes BibTeX structural markers such as external braces.
#' Therefore, it should not be used when the original BibTeX structure needs
#' to be preserved, for example to detect literal institutional authors.
#'
#' @examples
#'
#' bib_clean_text("{{CIESIN Columbia University}}")
#' # "CIESIN Columbia University"
#'
#' bib_clean_text("{World Population Prospects 2024}")
#' # "World Population Prospects 2024"
#'
#' @export
bib_clean_text <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) {
    return(NA_character_)
  }

  x <- trimws(x)

  if (startsWith(x, "\"") && endsWith(x, "\"")) {
    x <- substring(x, 2, nchar(x) - 1)
  }

  repeat {
    if (startsWith(x, "{") && endsWith(x, "}")) {

      chars <- strsplit(x, "")[[1]]
      level <- 0
      balanced <- TRUE

      for (i in seq_along(chars)) {
        if (chars[i] == "{") level <- level + 1
        if (chars[i] == "}") level <- level - 1

        if (level == 0 && i < length(chars)) {
          balanced <- FALSE
          break
        }
      }

      if (balanced) {
        x <- substring(x, 2, nchar(x) - 1)
        x <- trimws(x)
      } else {
        break
      }

    } else {
      break
    }
  }

  x <- gsub("[{}]", "", x)
  x <- gsub("\\s+", " ", x)

  trimws(x)
}
