#' Generate a Tibble from a .bib File
#'
#' Reads a BibTeX file and returns a tibble with standardized bibliographic
#' metadata for one entry, selected by numeric index or BibTeX citation key.
#'
#' The returned tibble preserves the raw BibTeX entry and includes derived
#' short and full citation strings.
#'
#' @param bib_file Path to a `.bib` file.
#' @param entry A numeric index or a character string with the BibTeX citation
#'   key.
#'
#' @return A one-row tibble containing bibliographic metadata, the raw BibTeX
#'   entry, and derived citation strings.
#'
#' @examples
#' \dontrun{
#' bib_entry_to_tibble("data/raw/sample_source.bib", 1)
#' bib_entry_to_tibble("data/raw/sample_source.bib", "ciesin2018")
#' }
#'
#' @export
bib_entry_to_tibble <- function(bib_file, entry = 1) {

  entry_text <- bib_read_entry(bib_file, entry = entry)
  header <- bib_extract_header(entry_text)

  author_raw <- bib_extract_field_raw(entry_text, "author")
  author <- bib_extract_field(entry_text, "author")
  source_year <- bib_extract_year(entry_text, "date")

  author_is_literal <- bib_author_is_literal(author_raw)

  citation_short <- bib_make_citation_short(
    author = author,
    source_year = source_year,
    author_is_literal = author_is_literal
  )

  citation_full <- bib_make_citation_full(
    author = author,
    source_year = source_year,
    title = bib_extract_field(entry_text, "title"),
    publisher = bib_extract_field(entry_text, "publisher"),
    doi = bib_extract_field(entry_text, "doi"),
    url = bib_extract_field(entry_text, "url")
  )

  tibble::tibble(
    source_key = header$source_key,
    source_type = header$source_type,
    bibtex_entry = entry_text,
    title = bib_extract_field(entry_text, "title"),
    author = author,
    author_is_literal = author_is_literal,
    publisher = bib_extract_field(entry_text, "publisher"),
    source_year = source_year,
    source_date = bib_extract_field(entry_text, "date"),
    doi = bib_extract_field(entry_text, "doi"),
    url = bib_extract_field(entry_text, "url"),
    urldate = bib_extract_field(entry_text, "urldate"),
    abstract = bib_extract_field(entry_text, "abstract"),
    keywords = bib_extract_field(entry_text, "keywords"),
    citation_short = citation_short,
    citation_full = citation_full,
    license = NA_character_,
    notes = NA_character_
  )
}
