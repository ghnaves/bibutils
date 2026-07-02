#' Detect a Literal BibTeX Author Field
#'
#' Detects whether an entire BibTeX `author` field is protected by double
#' braces, indicating a literal institutional or corporate author.
#'
#' @param x A character string containing the raw BibTeX `author` field.
#'
#' @return A logical value. Returns `TRUE` for a double-braced literal author,
#'   `FALSE` otherwise, and `NA` for missing or empty input.
#'
#' @examples
#' \dontrun{
#' bib_author_is_literal("{{CIESIN Columbia University}}")
#' # TRUE
#'
#' bib_author_is_literal("{United Nations}")
#' # FALSE
#'
#' bib_author_is_literal("Smith, John")
#' # FALSE
#'}
#'
#' @keywords internal
bib_author_is_literal <- function(x) {

  if (is.na(x) || !nzchar(stringr::str_squish(x))) {
    return(NA)
  }

  x <- stringr::str_squish(x)

  stringr::str_detect(x, "^\\{\\{.+\\}\\}$")
}
