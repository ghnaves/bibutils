#' Generate a tibble based on a .bib file
#'
#' Reads a BibTeX file and returns a data frame with a standard set of fields
#' for a single entry, either by numeric index
#'
#' @param bib_file Path to a `.bib` file.
#' @param entry Entry index or BibTeX key.
#'
#' @return a data frame with a standard set of fields for a single entry
#' @export
#'
#' @examples
#' \dontrun{
#' bib_entry_to_tibble("data/raw/sample_source.bib", 1)
#' }


bib_entry_to_tibble <- function(bib_file, entry = 1) {
    entry_text <- bib_read_entry(bib_file, entry = entry)
    header <- bib_extract_header(entry_text)

    tibble::tibble(
      source_key  = header$source_key,
      source_type = header$source_type,
      title       = bib_extract_field(entry_text, "title"),
      author      = bib_extract_field(entry_text, "author"),
      publisher   = bib_extract_field(entry_text, "publisher"),
      source_year = bib_extract_year(entry_text, "date"),
      source_date = bib_extract_field(entry_text, "date"),
      doi         = bib_extract_field(entry_text, "doi"),
      url         = bib_extract_field(entry_text, "url"),
      urldate     = bib_extract_field(entry_text, "urldate"),
      abstract    = bib_extract_field(entry_text, "abstract"),
      keywords    = bib_extract_field(entry_text, "keywords"),
      citation    = NA_character_,
      license     = NA_character_,
      notes       = NA_character_
    )
  }
