# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(bibutils)

test_check("bibutils")

test_that("short citations are formatted correctly", {
  expect_equal(
    bib_make_citation_short("{Givisiez, Gustavo Henrique Naves}", 2025),
    "Givisiez (2025)"
  )

  expect_equal(
    bib_make_citation_short(
      "{Olson, David M. and Dinerstein, Eric and Wikramanayake, Eric D. and Burgess, Neil D.}",
      2025
    ),
    "Olson et al. (2025)"
  )
})
