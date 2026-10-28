# Test for control_types = TRUE with different columns (#17)
#
# Functions tested:
# R/DescriptivesTable.R

cohort <- data.table::data.table(
  group = rep(c("CONTROL", "EXPOSED"), each = 10),
  outcome = rep(c(TRUE, FALSE), 10),
  t0 = as.IDate("2020-01-01") + 0:19,
  t1 = as.POSIXct("2020-01-01") + 0:19
)

metadata <- data.table::data.table(
  var = "outcome",
  type = "TF",
  expectedCat = NA,
  label = "Outcome",
  parent = NA_character_,
  parent_cat = NA_character_,
  header = NA_character_
)

testthat::test_that("control_types works with both IDate and POSIXct columns", {
  testthat::expect_no_error(DescriptivesTable(
    copy(cohort),
    metadata,
    control_types = TRUE
  ))
})
