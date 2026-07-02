#' Detect Literal Author Part
#'
#' Detects whether one author part is enclosed in braces and should be treated
#' as a literal institutional or corporate author.
#'
#' @param x A character string containing one author.
#'
#' @return `TRUE` if the author is enclosed in braces, otherwise `FALSE`.
#'
#' @keywords internal
bib_author_part_is_literal <- function(x) {
  stringr::str_detect(
    stringr::str_squish(x),
    "^\\{.+\\}$"
  )
}
