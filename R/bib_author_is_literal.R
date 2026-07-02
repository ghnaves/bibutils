#' Detect Literal BibTeX Author Names
#'
#' Detects whether an author string is enclosed in curly braces, indicating
#' that it should be treated as a literal author name rather than parsed as a
#' personal name.
#'
#' @param x A character string containing one author name from a BibTeX
#'   `author` field.
#'
#' @return
#' A logical value. Returns `TRUE` when `x` is enclosed in one or two pairs
#' of curly braces, `FALSE` otherwise, and `NA` for missing or empty input.
#'
#' @details
#' In BibTeX, braces can be used to preserve institutional or corporate author
#' names as literal strings. This is particularly useful when an institutional
#' author contains multiple words and should not be reduced to a surname.
#'
#' This function should be applied to raw BibTeX author values, before
#' cleaning with \code{\link{bib_clean_text}}.
#'
#' Both single-brace and double-brace forms are recognized:
#'
#' - \preformatted{"{United Nations}"}
#' - \preformatted{"{{CIESIN Columbia University}}"}
#'
#' This helper is used internally by \code{\link{bib_make_citation_short}} to
#' prevent institutional author names from being parsed as personal names.
#'
#' @examples
#' \dontrun{
#' bib_author_is_literal("{{CIESIN Columbia University}}")
#' TRUE
#'
#' bib_author_is_literal("{United Nations}")
#' TRUE
#'
#' bib_author_is_literal("Smith, John")
#' # FALSE
#' }
#' @keywords internal
#'
#' @export
bib_author_is_literal <- function(x) {

  if (is.na(x) || !nzchar(stringr::str_squish(x))) {
    return(NA)
  }

  x <- stringr::str_squish(x)

  stringr::str_detect(
    x,
    "^\\{(?:\\{.*\\}|.*)\\}$"
  )
}
