#' Read one BibTeX entry from a .bib file
#'
#' Reads a BibTeX file and returns a single entry either by numeric index
#' or by BibTeX key.
#'
#' @param bib_file Path to a `.bib` file.
#'
#' @return A character string containing the raw BibTeX entry.
#' @export
#'
#' @examples
#' \dontrun{
#' bib_extract_header("data/raw/sample_source.bib", 1)
#' }
bib_extract_header <- function(entry_text) {
  m <- stringr::str_match(entry_text, "^@(\\w+)\\{([^,]+),")

  list(
    source_type = ifelse(is.na(m[, 2]), NA_character_, trimws(m[, 2])),
    source_key  = ifelse(is.na(m[, 3]), NA_character_, trimws(m[, 3]))
  )
}

bib_clean_text <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(trimws(x))) {
    return(NA_character_)
  }

  x <- trimws(x)

  # remove aspas externas
  if (startsWith(x, "\"") && endsWith(x, "\"")) {
    x <- substring(x, 2, nchar(x) - 1)
  }

  # remove múltiplas camadas de chaves externas balanceadas
  repeat {
    if (startsWith(x, "{") && endsWith(x, "}")) {

      chars <- strsplit(x, "")[[1]]
      level <- 0
      balanced <- TRUE

      for (i in seq_along(chars)) {
        if (chars[i] == "{") level <- level + 1
        if (chars[i] == "}") level <- level - 1

        # se zerar antes do fim → não é camada externa pura
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

  # remove chaves internas restantes
  x <- gsub("[{}]", "", x)

  # normaliza espaços
  x <- gsub("\\s+", " ", x)
  trimws(x)
}
