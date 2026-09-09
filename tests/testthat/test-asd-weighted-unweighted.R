# Tests for weighted and unweighted standardized differences (ASDs)
#
# Functions under test:
#   R/wtd_stdiff_functions.R
#   R/asd_helper.R
#
# Run with devtools::test(), or devtools::load_all() before sourcing this file.
# These tests cover both weighted and unweighted behaviour of wtd.stddiff.*().
library(testthat)

# The numeric values deliberately produce an ASD of about 0.024598, so a test
# can detect the 3-decimal rounding currently introduced by stddiff.
cohort_asd <- data.frame(
  group = rep(c("CONTROL", "EXPOSED"), each = 8),
  w = c(
    1, 2, 1, 3, 1, 2, 1, 1,
    3, 1, 2, 1, 2, 1, 3, 1
  ),
  age = c(
    10, 20, 30, 40, 50, 60, 70, 80,
    14.73, 20, 30, 40, 50, 60, 70, 80
  ),
  sick = c(
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE
  ),
  severity = c(
    "mild", "mild", "mild", "moderate", "moderate", "severe", "severe", "severe",
    "mild", "mild", "moderate", "moderate", "moderate", "severe", "severe", "severe"
  ),
  stringsAsFactors = FALSE
)

asd_variable <- c(
  NUM1 = "age",
  TF = "sick",
  CAT = "severity"
)

# use_weights should accept either FALSE (unweighted) or the name of a weights column.
call_asd <- function(type, data = cohort_asd, use_weights = FALSE) {
  variable <- unname(asd_variable[type])
  fn <- switch(type,
    NUM1 = wtd.stddiff.numeric,
    TF = wtd.stddiff.binary,
    CAT = wtd.stddiff.category
  )

  fn(
    data = data,
    gcol = which(names(data) == "group"),
    vcol = which(names(data) == variable),
    use_weights = use_weights,
    group_names = c("CONTROL", "EXPOSED")
  )
}

extract_asd <- function(result) {
  unname(result[1, "stddiff"])
}

# Independent, unrounded reference calculations. Only the ASD is reproduced
# here; the test does not duplicate the complete output-building code.
reference_numeric_asd <- function(data) {
  control <- data$age[data$group == "CONTROL"]
  exposed <- data$age[data$group == "EXPOSED"]

  control <- control[!is.na(control)]
  exposed <- exposed[!is.na(exposed)]

  abs(mean(exposed) - mean(control)) /
    sqrt((stats::var(exposed) + stats::var(control)) / 2)
}

reference_binary_asd <- function(data) {
  x <- as.factor(data$sick)
  positive <- levels(x)[length(levels(x))]

  p_control <- mean(x[data$group == "CONTROL"] == positive, na.rm = TRUE)
  p_exposed <- mean(x[data$group == "EXPOSED"] == positive, na.rm = TRUE)

  abs(p_exposed - p_control) /
    sqrt((p_exposed * (1 - p_exposed) + p_control * (1 - p_control)) / 2)
}

reference_category_asd <- function(data) {
  x <- as.factor(data$severity)
  keep <- !is.na(x) & !is.na(data$group)
  prop <- prop.table(table(x[keep], data$group[keep]), 2)

  control <- prop[-1, "CONTROL"]
  exposed <- prop[-1, "EXPOSED"]
  k <- length(control)
  covariance <- matrix(0, nrow = k, ncol = k)

  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (i == j) {
        covariance[i, j] <- 0.5 * (
          exposed[i] * (1 - exposed[i]) +
            control[i] * (1 - control[i])
        )
      } else {
        covariance[i, j] <- -0.5 * (
          exposed[i] * exposed[j] + control[i] * control[j]
        )
      }
    }
  }

  difference <- exposed - control
  as.numeric(sqrt(t(difference) %*% solve(covariance) %*% difference))
}

reference_asd <- function(type, data = cohort_asd) {
  switch(type,
    NUM1 = reference_numeric_asd(data),
    TF = reference_binary_asd(data),
    CAT = reference_category_asd(data)
  )
}


test_that("unweighted ASDs do not require a weights column", {
  no_weights <- cohort_asd[, setdiff(names(cohort_asd), "w")]

  for (type in names(asd_variable)) {
    expect_no_error(
      result <- call_asd(type, data = no_weights, use_weights = FALSE)
    )
  }
})


test_that("unweighted ASDs match the raw formulas and retain full precision", {
  for (type in names(asd_variable)) {
    actual <- extract_asd(call_asd(type, use_weights = FALSE))
    expected <- reference_asd(type)

    expect_equal(actual, expected, tolerance = 1e-12, info = type)
  }
})


test_that("weights of 1 reproduce the unweighted ASD", {
  ones <- cohort_asd
  ones$w <- 1

  for (type in names(asd_variable)) {
    weighted <- extract_asd(call_asd(type, data = ones, use_weights = "w"))
    unweighted <- extract_asd(call_asd(type, data = ones, use_weights = FALSE))

    expect_equal(weighted, unweighted, tolerance = 1e-12, info = type)
  }
})


test_that("non-uniform weights still change the ASD", {
  for (type in names(asd_variable)) {
    weighted <- extract_asd(call_asd(type, use_weights = "w"))
    unweighted <- extract_asd(call_asd(type, use_weights = FALSE))

    expect_false(isTRUE(all.equal(weighted, unweighted)), info = type)
  }
})


test_that("weight values are ignored when use_weights is FALSE", {
  missing_weights <- cohort_asd
  missing_weights$w[c(1, 9)] <- NA_real_

  for (type in names(asd_variable)) {
    expect_equal(
      extract_asd(call_asd(type, data = missing_weights, use_weights = FALSE)),
      extract_asd(call_asd(type, data = cohort_asd, use_weights = FALSE)),
      tolerance = 1e-12,
      info = type
    )
  }
})


test_that("asd_helper uses the unified unweighted implementation", {
  for (type in names(asd_variable)) {
    variable <- unname(asd_variable[type])

    helper_result <- asd_helper(
      indf = cohort_asd,
      y = which(names(cohort_asd) == "group"),
      x = which(names(cohort_asd) == variable),
      met = type,
      use_weights = FALSE
    )

    direct_result <- call_asd(type, use_weights = FALSE)

    expect_equal(
      extract_asd(helper_result),
      extract_asd(direct_result),
      tolerance = 1e-12,
      info = type
    )
  }
})


test_that("unified unweighted ASDs remain compatible with stddiff after rounding", {
  skip_if_not_installed("stddiff")

  legacy_functions <- list(
    NUM1 = stddiff::stddiff.numeric,
    TF = stddiff::stddiff.binary,
    CAT = stddiff::stddiff.category
  )

  for (type in names(asd_variable)) {
    variable <- unname(asd_variable[type])
    legacy <- legacy_functions[[type]](
      data = cohort_asd,
      gcol = which(names(cohort_asd) == "group"),
      vcol = which(names(cohort_asd) == variable)
    )
    unified <- call_asd(type, use_weights = FALSE)

    expect_equal(
      round(extract_asd(unified), 3),
      extract_asd(legacy),
      info = type
    )
  }
})
