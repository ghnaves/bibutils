devtools::document()
devtools::build()
# devtools::load_all()
install.packages("~/projects/bibutils_0.1.1.tar.gz", repos = NULL, type = "source")
library(bibutils)
bib_read_entry('data/sample_source.bib')
bib_entry_to_tibble('data/sample_source.bib')
bib_get_citations(bib_entry_to_tibble('data/sample_source.bib'))




