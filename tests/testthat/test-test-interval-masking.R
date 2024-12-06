library(testthat)
test_that("interval masking works with one masked variable", {
  
  count <- c(3, 97, 100)
  masked_count <- c("[1-4]", "97","[99-102]")
  
  expect_equal(interval_mask(count), masked_count)
  
  expect_true(is.character(interval_mask(count)))
})

test_that("interval masking works with multiple masked variables", {
  
  count <- c(1,3,4, 97,100)
masked_count <- c("[1-4]", "[1-4]", "[1-4]", "97","[96-105]")
  
  expect_equal(interval_mask(count), masked_count)
  
  expect_true(is.character(interval_mask(count)))
})

test_that('interval masking throws warning when lower bound of interval is lower than 5', {
  
  count <- c(1,2,3,7)
  
  expect_warning(interval_mask(counts))
  
}
)
