# function to mask vectors
# number ; below which number to mask counts (typically 5)
mask_count <- function(x, threshold = 5){
  unlist(sapply(x, function(s){
    if(s %in% c("NE","NA",0,NA,NaN,"") | as.numeric(s) < 0 | grepl("<",s)) return(s)
    if(as.numeric(s) == 0) return(s)
    if(as.numeric(s) < threshold) return(paste0("<", threshold))
    if(!as.numeric(s) < threshold) return(sprintf("%1.0f", as.numeric(s)))
  }))
}


interval_mask <- function(x,threshold = 5, output_warnings = TRUE){
  naive_interval <- 1:(threshold - 1)
  counts <- x
  total <- sum(counts)
  is_masked <- ifelse(counts <5, TRUE, FALSE)
  n_masked <- sum(is_masked)
  is_possible_interval <- !is_masked
  potential_interval <- sort(counts[is_possible_interval], decreasing = TRUE)
  
  is_interval_masked <- potential_interval[1]
  is_not_interval_masked <- potential_interval[-1]
  
  upper.int <- total - sum(is_not_interval_masked) - n_masked*1
  lower.int <- total - sum(is_not_interval_masked) - n_masked*(threshold-1)
  
  # Apply masking
  ## For valuess below threshold, just mask
  masked_counts <- ifelse(counts <5, '[1-4]', counts)
  ## For interval masked values, substitute by interval
  ## When there are twe counts that are equal to the count that should be interval masked,
  ## only mask one of them
  interval_masked_index <- which(counts == is_interval_masked)[1]
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


