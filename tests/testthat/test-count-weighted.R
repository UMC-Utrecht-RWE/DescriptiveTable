# Tests for weighted counts and descriptive statistics
#
# Functions under test:
#   R/count_weighted_functions.R
#   R/DescriptivesTable.R  (weighted_stats argument)
#
# Run with devtools::test(), or devtools::load_all() before sourcing this file
library(testthat)
library(data.table)

# Example cohort with 2 tretment groups
cohort <- data.table(
  group = rep(c("EXPOSED", "CONTROL"), each = 4),
  w = c(1, 2, 3, 4, 4, 3, 2, 1),
  age = c(10, 20, 30, 40, 10, 20, 30, 40),
  sex = c("F", "F", "M", "M", "F", "M", "M", "F"),
  sick = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
  severity = c("mild", NA, "severe", NA, "mild", NA, "severe", NA)
)

metadata <- data.table(
  var = c("age", "sex", "sick", "severity"),
  type = c("NUM1", "CAT", "TF", "CAT"),
  expectedCat = c(NA, "F, M", NA, "mild, severe"),
  label = c("Age", "Sex", "Sick", "Severity"),
  parent = c(NA, NA, NA, "sick"),
  parent_cat = c(NA, NA, NA, "TRUE"),
  header = NA_character_
)

descriptives <- function(popdf = cohort, ...) {
  DescriptivesTable(
    popdf = copy(popdf),
    table_metadata = metadata,
    groupcol = "group",
    output_format = "processed",
    calculate_asd = FALSE,
    keep_varinfo = TRUE,
    control_types = FALSE,
    output_asd = FALSE,
    ...
  )
}

test_that("weights of 1 give the unweighted table", {
  ones <- copy(cohort)[, w := 1]
  expect_equal(
    descriptives(ones, use_weights = "w", weighted_stats = TRUE),
    descriptives(ones)
  )
})

test_that("min and max are not weighted", {
  d <- data.table(x = c(1, 2, 3, 100), w = c(0.5, 1, 1, 0.5))
  res <- count_weighted_NUM2(d, "x", weightName = "w")

  expect_equal(as.numeric(res$V2[2]), 1)
  expect_equal(as.numeric(res$V3[2]), 100)
})

test_that("a covariate that is missing does not error", {
  blank <- copy(cohort)[, age := NA_real_]
  expect_no_error(descriptives(
    blank,
    use_weights = "w",
    weighted_stats = TRUE
  ))
  expect_no_error(
    count_weighted_NUM2(data.table(x = NA_real_, w = 1), "x", weightName = "w")
  )
})
test_that("weighted statistics without a weights column error", {
  expect_error(
    descriptives(weighted_stats = TRUE),
    "requires a weights column"
  )
})

test_that("a weights column absent from popdf errors", {
  expect_error(
    descriptives(use_weights = "iptw", weighted_stats = TRUE),
    "not found in popdf"
  )
})

test_that("use_weights alone leaves counts and statistics unweighted", {
  expect_equal(descriptives(use_weights = "w"), descriptives())
})

test_that("weighted_stats changes table body", {
  expect_false(isTRUE(all.equal(
    descriptives(use_weights = "w", weighted_stats = TRUE),
    descriptives()
  )))
})
