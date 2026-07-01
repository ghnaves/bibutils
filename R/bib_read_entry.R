#' Read one BibTeX entry from a .bib file
#'
#' Reads a BibTeX file and returns a single entry either by numeric index
#' or by BibTeX key.
#'
#' @param bib_file Path to a `.bib` file.
#' @param entry Entry index or BibTeX key.
#'
#' @return A character string containing the raw BibTeX entry.
#' @export
#'
#' @examples
#' \dontrun{
#' bib_read_entry("data/raw/sample_source.bib", 1)
#' }

bib_read_entry <- function(bib_file, entry = 1) {
  if (!file.exists(bib_file)) {
    stop("Arquvo .bib nao encontrado: ", bib_file)
  }

  lines <- readLines(bib_file, warn = FALSE, encoding = "UTF-8")
  txt <- paste(lines, collapse = "\n")

  entries <- stringr::str_split(txt, "\n(?=@)", simplify = FALSE)[[1]]
  entries <- entries[nzchar(trimws(entries))]

  if (length(entries) == 0) {
    stop("Nenhuma entrada BibTeX encontrada.")
  }

  if (is.character(entry)) {
    hit <- which(stringr::str_detect(
      entries,
      paste0("^@\\w+\\{", stringr::fixed(entry), ",")
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
