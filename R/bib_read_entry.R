#' Read One BibTeX Entry from a .bib File
#'
#' Reads a BibTeX file and returns a single entry, selected either by numeric
#' index or by BibTeX citation key.
#'
#' @param bib_file Path to a `.bib` file.
#' @param entry A numeric index or a character string with the BibTeX citation
#'   key.
#'
#' @return
#' A character string containing the raw BibTeX entry.
#'
#' @examples
#' \dontrun{
#' bib_read_entry("data/raw/sample_source.bib", 1)
#' bib_read_entry("data/raw/sample_source.bib", "ciesin2018")
#' }
#'
#' @export
bib_read_entry <- function(bib_file, entry = 1) {

  if (!file.exists(bib_file)) {
    stop("Arquivo .bib nao encontrado: ", bib_file)
  }

  lines <- readLines(bib_file, warn = FALSE, encoding = "UTF-8")
  txt <- paste(lines, collapse = "\n")

  entries <- stringr::str_split(txt, "\n(?=@)", simplify = FALSE)[[1]]
  entries <- entries[nzchar(trimws(entries))]

  if (length(entries) == 0) {
    stop("Nenhuma entrada BibTeX encontrada.")
  }

  if (is.character(entry)) {
    entry_escaped <- stringr::str_escape(entry)
    hit <- which(stringr::str_detect(
      entries,
      paste0("^@\\w+\\{", entry_escaped, ",")
    ))
    if (length(hit) == 0) {
      stop("Chave BibTeX nao encontrada: ", entry)
    }
    return(entries[[hit[1]]])
  }

  if (!is.numeric(entry) || length(entry) != 1 || is.na(entry)) {
    stop("`entry` deve ser um indice numerico ou uma chave BibTeX.")
  }

  if (entry < 1 || entry > length(entries)) {
    stop("Indice de entrada invalido.")
  }

  entries[[entry]]
}
