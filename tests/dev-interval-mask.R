
interval_mask <- function(counts, threshold = 5, output_warnings = TRUE){
  naive_interval <- 1:(threshold - 1) # interval to mask individual counts enforced by daps
  total <- sum(counts) 
  naively_masked <- ifelse(counts <5, TRUE, FALSE) # logical, values that will be naively masked
  n_masked <- sum(naively_masked) # number of masked values
  not_naively_masked <- !naively_masked # logical, values that are not naively masked
  # value that will be interval masked
  interval_masked <- max(counts[not_naively_masked]) # value that will be interval masked
  ## When two counts have the same value, potentially they both could be interval masked
  ## Next three lines are necessary to select only one
  interval_masked_index <- which(counts == interval_masked)[1] # index interval_masked_value (only first one)
  interval_mask_logical <- rep(FALSE, length(counts)) 
  interval_mask_logical[interval_masked_index] <- TRUE # Vector where the only TRUE corresponds to index of ONE interval masked value 
  
  # Select not_masked_variables
  not_masked_logical <- not_naively_masked & !interval_mask_logical
  not_masked <- counts[not_masked_logical]
  
  # Create interval using formula in the slides
  upper.int <- total - sum(not_masked) - n_masked*1
  lower.int <- total - sum(not_masked) - n_masked*(threshold-1)

  # Apply masking
  ## For values below threshold, just mask
  masked_counts <- ifelse(counts <5, '[1-4]', counts)
  ## For interval masked values, substitute by interval
  ## When there are twe counts that are equal to the count that should be interval masked,
  ## only mask one of them
  masked_counts[interval_masked_index] <-  paste0('[',lower.int, '-', upper.int, ']')
  
  # optional warnings 
  if(output_warnings == TRUE) {
    ## warning explicits which combinations of values are or are not possible
    if(lower.int <5) {
      excluded_int <- lower.int:(threshold-1)
      interval_combs <- do.call(expand.grid, replicate(n_masked, naive_interval, simplify = FALSE))
      
      interval_combs$sums <- rowSums(interval_combs)
      
      ## WARNING: I THINK THIS IS THE OTHER WAY AROUND?
      excluded_values <- subset(interval_combs, !interval_combs$sums %in% excluded_int)[, !names(interval_combs) == 'sums']
      possible_values <- subset(interval_combs, interval_combs$sums %in% excluded_int)[, !names(interval_combs) == 'sums']
      
      
      perc_impossible <- paste0((nrow(excluded_values)/nrow(interval_combs))*100, '%')
      
      warning(paste0('This table may not be safe. Of all possible combinations of naively masked values, ', 
                     perc_impossible, ' value combinations can be excluded'))
      cat('Here are the combinations of masked values that are not possible')
      print(excluded_values)
      cat('Here are the combinations of unmasked values that are possible')
      print(possible_values)
      
    }
  }
  
  return(masked_counts)
  
}

count <- c(1,2,97,100)
interval_mask(count)

count <- c(1,2,7)
interval_mask(count, output_warnings = TRUE)
interval_mask(count, output_warnings = FALSE)


threshold <- 5
lower.int <- 2
n_masked <- 3
naive_interval <- 1:4

excluded_int <- lower.int:(threshold-1)
grid <- do.call(expand.grid, replicate(n_masked, naive_interval, simplify = FALSE))

grid$sums <- rowSums(grid)

excluded_values <- subset(grid, grid$sums %in% excluded_int)[, !names(grid) == 'sums']
possible_values <- subset(grid, !grid$sums %in% excluded_int)[, !names(grid) == 'sums']

print(excluded_values)
print(possible_values)

perc_impossible <- nrow(excluded_values)/nrow(grid)


