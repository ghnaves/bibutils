test_that("short citations are formatted correctly", {

  expect_equal(
    bib_make_citation_short(
      "{Givisiez, Gustavo Henrique Naves}",
      2025
    ),
    "Givisiez (2025)"
  )

  expect_equal(
    bib_make_citation_short(
      "{Givisiez, Gustavo Henrique Naves and Oliveira, Elzira Lucia}",
      2025
    ),
    "Givisiez & Oliveira (2025)"
  )

  expect_equal(
    bib_make_citation_short(
      "{Olson, David M. and Dinerstein, Eric and Wikramanayake, Eric D. and Burgess, Neil D.}",
      2025
    ),
    "Olson et al. (2025)"
  )
})

test_that("full citations format author initials", {

  expect_equal(
    bib_make_citation_full(
      author = "{Givisiez, Gustavo Henrique Naves}",
      source_year = 2025,
      title = "Geografia da Populacao"
    ),
    "Givisiez, G. H. N. (2025). Geografia da Populacao."
  )

  expect_equal(
    bib_make_citation_full(
      author = "{{United Nations}}",
      source_year = 2024,
      title = "World Population Prospects 2024",
      publisher = "United Nations"
    ),
    "United Nations (2024). World Population Prospects 2024. United Nations."
  )
})
