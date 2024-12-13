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
  
  expect_warning(interval_mask(count))
  
}
)

test_that('interval masking does not throw warning when lower bound of interval is greater than 5', {
  
  count <- c(1,2,3,16)
  
  expect_silent(interval_mask(count))
  
}
)

test_that('interval masking only interval masks one value when there are two equal counts that could be interval masked',
          {
            count <- c(1,3,18,18)
            interval_mask(count)
            
            expect_equal(interval_mask(count), c("[1-4]", "[1-4]", "[14-20]", "18"))
          }
)

