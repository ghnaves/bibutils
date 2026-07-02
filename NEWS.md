# bbutils 0.1.3

## New features

* Added `bib_author_is_literal()` to identify literal BibTeX author names,
    including institutional and corporate authors protected with braces.
* Added `bib_extract_field_raw()` to extract BibTeX field values while
    preserving original delimiters and structural information.
* Updated `bib_entry_to_tibble()` to retain the raw BibTeX entry in
    bibtex_entry.
* Updated `bib_entry_to_tibble()` to include author_is_literal,
    citation_short, and citation_full.

## Improvements

* Updated `bib_extract_field()` to focus on returning cleaned display values.
* Updated `bib_make_citation_short()` to support literal institutional authors
    through the author_is_literal argument.
* Updated `bib_get_citations()` to use `author_is_literal` when generating
    short citations.
* Improved documentation for BibTeX entry parsing, field extraction, text
    cleaning, author handling, and year extraction.

# bbutils 0.1.2

## New features

* Added `bib_make_citation_short()`.
* Added `bib_make_citation_full()`.
* Added `bib_get_citations()`.
