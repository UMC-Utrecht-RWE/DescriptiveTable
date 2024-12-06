library(testthat)
test_that("interval masking works with one masked variable", {
  
  count <- c(3, 97, 100)
  masked_count <- c("[1-4]", "97","[99-102]")
  
  expect_equal(interval_mask(count), masked_count)
  
  expect_true(is.character(interval_mask(count)))
})
