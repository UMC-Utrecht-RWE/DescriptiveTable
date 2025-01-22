library(testthat)

test_that("masking a numerical vector returns correct output", {
  
  # One count needs to be masked
  count <- c(3, 97, 100)
  masked_count <- c("[1-4]", "97","[99-102]")
  expect_equal(mask_vector(count), masked_count)
  # Only one count is present
  expect_equal(mask_vector(4), '[1-4]')
  expect_equal(mask_vector(8, threshold = 9), '[1-8]')
  expect_true(is.character(mask_vector(count))) 
  
  # Several counts need to be masked and one interval masked
  count <- c(1,3,4, 97,100)
  masked_count <- c("[1-4]", "[1-4]", "[1-4]", "97","[96-105]")
  expect_equal(mask_vector(count), masked_count)
  
  expect_true(is.character(mask_vector(count)))
  
  # All counts need to be naively masked, none interval masked
  expect_equal(mask_vector(c(1,2,6,3), threshold = 7, output_warnings = F), c('[1-6]', '[1-6]', '[1-6]', '[1-6]'))
  expect_equal(mask_vector(c(0, 1,2,1,1), threshold = 7, output_warnings = F), c(0,'[1-6]', '[1-6]', '[1-6]', '[1-6]'))
  
  
  # No count needs to be masked
  expect_equal(mask_vector(c(6, 97, 100)), c(6,97,100))
})


test_that('warnings are thrown correctly', {
  
  # masking a vector throws warning when not all value combinations are possible'
  # Internally, this happens when the lower bound of the theoretical interval is lower than the threshold
  expect_warning(mask_vector(c(1,2,3,7)))
  expect_warning(mask_vector(c(14,14), threshold = 17))
  
  # no warning if warnings are turned off
  
  expect_no_warning(mask_vector(c(1,2,3,7), output_warnings = FALSE))
  
  # No warning because lower bound of interval is larger than threshold
  expect_silent(mask_vector(c(1,2,3,29), threshold = 8))
  })



test_that('when there are two counts that are 1) equal and 2) the maximum value of the unmasked, the function mask_vector will only mask one 
          of them',
          {
            count <- c(1,3,18,18)
            expect_equal(mask_vector(count), c("[1-4]", "[1-4]", "[14-20]", "18"))
          }
)

test_that('if specified, masking a vector returns a (correct) percentage',{
  
  # If there's no masked variables, sum of masked counts adds up to 10
  count <- c(6, 97, 100)
  expect_equal(sum(as.numeric(mask_vector(count, percentage = TRUE))), 100)
  
  count <- c(3,4,99, 102, 104)
  total <- sum(count)
  
  naive.lower.bound <- 100*(1/total)
  naive.upper.bound <- 100*(6/total)
  naive.interval <- paste0('[', naive.lower.bound, '-', naive.upper.bound, ']')
  
  largeint.upper.bound <- 100*((total - 102-99 - 2*1)/total)
  largeint.lower.bound <- 100*((total - 102-99 - 2*6)/total)
  largeint <- paste0('[', largeint.lower.bound, '-', largeint.upper.bound, ']')
  
  percentages <- c(naive.interval, naive.interval, 100*(99/total), 100*(102/total), largeint)
  expect_equal(mask_vector(count, threshold = 7, percentage = TRUE), percentages)
})

test_that('special values are correctly handled', {
  # One count needs to be masked
  count <- c(3, 97, 100)
  masked_count <- c("[1-4]", "97","[99-102]")
  
  count <- c(3, 97, 0, 100)
  masked_count <- c("[1-4]", "97",0,"[99-102]")
  expect_equal(mask_vector(count), masked_count)
  
  count <- c(0, 3, 97, NA, 'NE', NaN, 100, -99, -77, -66, '', 'NR', 'NA')
  masked_count <- c(0, "[1-4]", "97",NA, 'NE', NaN, "[99-102]", -99, -77, -66, '', 'NR', 'NA')
  expect_equal(mask_vector(count), masked_count)
  
  # 
  count <- c(0, 3, 97, NA, 'NE', NaN, 100, -99, -77, -66, '', 'NR', 'NA')
  result <- unname(sapply(count, is_exception))
  expect_equal(result, c(T,F,F,T,T,T,F,T,T,T,T,T,T))

})

