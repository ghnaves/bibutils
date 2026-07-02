test_that("bib_author_is_literal detects protected literal names", {
  expect_true(bib_author_is_literal("{{CIESIN Columbia University}}"))
  expect_true(bib_author_is_literal("{United Nations}"))
  expect_false(bib_author_is_literal("Smith, John"))
})
