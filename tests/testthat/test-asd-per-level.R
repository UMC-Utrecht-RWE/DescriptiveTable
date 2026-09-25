# Tests for per-category ASDs of categorical variables
#
# Functions under test:
#   R/compute_asd.R        (asd_per_level argument)
#   R/DescriptivesTable.R  (asd_per_level argument)
#
# Run with devtools::test(), or devtools::load_all() before sourcing this file
library(testthat)
library(data.table)

groups <- c("CONTROL", "EXPOSED")

# Cohort with a 4-level categorical; categories C and D are balanced across the
# groups, A and B are not
cohort <- data.table(
  group = rep(groups, each = 10),
  w = c(1:10, 10:1),
  region = c(
    rep(c("A", "B", "C", "D"), times = c(2, 4, 2, 2)),
    rep(c("A", "B", "C", "D"), times = c(5, 1, 2, 2))
  ),
  sick = rep(c(TRUE, FALSE), 10)
)

metadata <- data.table(
  var = c("region", "sick"),
  type = c("CAT", "TF"),
  expectedCat = c("A, B, C, D", NA),
  label = c("Region", "Sick"),
  parent = NA_character_,
  parent_cat = NA_character_,
  header = NA_character_
)

asd <- function(...) {
  compute_asd(copy(cohort), metadata, "1", group_names = groups, ...)
}

descriptives <- function(...) {
  DescriptivesTable(
    copy(cohort),
    metadata,
    control_types = FALSE,
    output_asd = FALSE,
    ...
  )
}


per_level <- asd(asd_per_level = TRUE)
overall <- asd()

# Expected ASD per category, worked out by hand from the cohort above. Each one
# is the difference between the two groups in the proportion falling in that
# category, divided by the pooled standard deviation of a 0/1 variable:
#
#   |p_t - p_c| / sqrt((p_t * (1 - p_t) + p_c * (1 - p_c)) / 2)
#
#         p_c    p_t   |diff|   pooled SD     ASD
#    A   0.20   0.50     0.30      0.4528   0.663
#    B   0.40   0.10     0.30      0.4062   0.739
#    C   0.20   0.20     0.00      0.4000   0.000
#    D   0.20   0.20     0.00      0.4000   0.000
#
# stddiff rounds its output to 3 decimals
expected <- c(A = 0.663, B = 0.739, C = 0, D = 0)

test_that("category rows are added below the overall row, only for CAT", {
  expect_equal(per_level[var == "region", cat], c(NA, "A", "B", "C", "D"))
  expect_equal(nrow(per_level[var == "sick"]), 1L)
})

test_that("each category ASD compares its proportion across the groups", {
  expect_equal(as.numeric(per_level[!is.na(cat), asd_1]), unname(expected))
})


test_that("asd_per_level leaves the rest of the output untouched", {
  expect_equal(names(overall), c("var", "type", "cat", "asd_1"))
  expect_equal(nrow(overall), nrow(metadata))
  expect_true(all(is.na(overall$cat)))
  expect_equal(per_level[is.na(cat), asd_1], overall$asd_1)
})


test_that("use_weights reaches the per-category ASD", {
  weighted <- asd(asd_per_level = TRUE, use_weights = "w")

  # compute_asd should be building this indicator and handing it, with the
  # weights, to the same binary function we call directly here
  indf <- as.data.frame(copy(cohort)[, region_A := as.numeric(region == "A")])
  by_hand <- wtd.stddiff.binary(
    indf,
    gcol = which(names(indf) == "group"),
    vcol = which(names(indf) == "region_A"),
    var = "region_A",
    use_weights = "w"
  )[1, "stddiff"]

  expect_equal(as.numeric(weighted[cat == "A", asd_1]), unname(by_hand))
  # the weights have to change the answer, or the check above proves nothing
  expect_false(isTRUE(all.equal(
    weighted[cat == "A", asd_1],
    per_level[cat == "A", asd_1]
  )))
})

test_that("per-category ASDs reach the table, under the header's overall", {
  tab <- descriptives(asd_per_level = TRUE)

  expect_equal(tab[label == "Region", asd_1], as.numeric(overall[1, asd_1]))
  expect_equal(tab[var == "region", asd_1], unname(expected))
})

test_that("without asd_per_level the category rows carry no ASD", {
  tab <- descriptives()

  expect_equal(tab[label == "Region", asd_1], as.numeric(overall[1, asd_1]))
  expect_true(all(is.na(tab[var == "region", asd_1])))
})

test_that("output_format = 'raw' returns the per-category ASD", {
  tab <- descriptives(output_format = "raw", asd_per_level = TRUE)

  expect_equal(as.numeric(tab[var == "region", asd_1]), as.numeric(per_level[!is.na(cat), asd_1]))
})
