# Functions under test:
#   R/count_weighted_functions.R
#   R/DescriptivesTable.R  (weighted_stats argument)
#
# Run with devtools::test(), or devtools::load_all() before sourcing this file
library(testthat)
library(data.table)

test_that("NUM1 reports SD when the variable has missing values", {
  d <- data.table::data.table(x = c(1, 2, 3, NA))
  res <- count_NUM1(d, "x", popN = 4)
  expect_equal(as.numeric(res[[3]][1]), stats::sd(c(1, 2, 3)))
})
