#' Helper: check if a category value is a valid count or an exception value
#'
#' @param value Category count or corresponding exception value
#'
#' @returns Logical indicating whether the value is within the exception values.
#' If TRUE, category count is not a positive interger, and thus is not subject to matching. If FALSE, the value is a proper count that may be subject to masking.
#' @export
#'
#' @examples
is_exception <- function(value){
  ifelse(value %in% c(0, "NE","NA",NA,NaN,"","NR", -66, -77, -88, -99), TRUE,FALSE)
}


#' Mask one vector.
#' 
#' Inputs a vector of categorical counts. Returns the masked version of the input. Ignores exceptions.
#' Concretely, it masks a) if there is any, all values below the threshold and 2) if any value is higher than the threshold, the largest of those, according to prespecified rules.
#' 
#' @param input_vector Numeric or character vector of category counts, possibly including special values such as NA, NE, etc. 
#' @param threshold Numeric. Threshold below which values need to be masked, externally determined.
#' @param output_warnings Logical. Should warning be issued displaying which value combinations are not possible, if any?
#' @param percentage Logical. Should output be raw numbers or corresponding percentages?
#'
#' @returns If no masking is needed, the original vector or its percentage version. If masking is needed, the masked counterpart or its percentage version.
#' 
#' @export
#'
#' @examples
mask_vector <- function(input_vector, threshold = 5, output_warnings = TRUE, percentage = FALSE){
  
  # Flag non-zero numeric index. Function only works with those,
  # and leaves all exceptions (including zero) unchanged.
  numeric_indices <- !sapply(input_vector, is_exception)
  # Store non-zero numeric values
  counts <- as.numeric(input_vector[numeric_indices])
  # Compute total counts, useful at different stages
  total <- sum(counts) # sum of each numerical category counts
  
  
  # String to replace masked values, different depending if outcome is percentage or raw
  if(percentage){
    below_threshold_pcn_lower <- (1/total)*100
    below_threshold_pcn_upper <- ((threshold-1)/total)*100
    below_threshold_sub <- paste0('[',below_threshold_pcn_lower, '-' , below_threshold_pcn_upper, ']')
  } else {
    below_threshold_sub <- paste0('[1-', threshold-1, ']')
  }
  
  
  
  
  
  # Handle input of length 1 (i.e. one cat or TF)
  ## If the input is non_numeric, left as is
  if(length(input_vector) == 1 & is_exception(input_vector[1])) return(input_vector)
  ## If there is one non-zero numeric count, naive mask
  ## (works even if input vector is of size >1 for NA, etc)
  if(length(counts) == 1){
    input_vector[numeric_indices] <- ifelse(counts < threshold, below_threshold_sub, counts)
    input_vector[!numeric_indices] <- input_vector[!numeric_indices]
    return(input_vector)
  }
  
  
  
  ## 1. Identify masked and unmasked values (naively masked and additional interval masked value)
  ## We determine values and positions, useful when masking the vector
  naively_masked_logical <- ifelse(counts < threshold, TRUE, FALSE) # logical, values that will be naively masked
  not_naively_masked_logical <- !naively_masked_logical # logical, values that are not naively masked
  
  # Create vector of masked counts (for storage, modified later)
  if(percentage==TRUE){
    masked_counts <- (counts/total)*100
  } else {
    masked_counts <- counts
  }
  
  
  n_masked <- sum(naively_masked_logical) # number of masked values, useful later
  
  if(any(not_naively_masked_logical & !all(not_naively_masked_logical))) {
    ## We determine which value is interval masked by looking for the largest not-naively masked value
    ## This might not be uniquely defined, as two or more categories could have the same max count
    ## We account for this by selecting only one of those.
    interval_masked_value <- max(counts[not_naively_masked_logical]) # maximum value among not naively masked
    interval_masked_index <- which(counts == interval_masked_value)[1] # get index of first category count equal to maximum value (might be only one)
    ## Generate logical indicating which value is the one interval masked 
    interval_mask_logical <- rep(FALSE, length(counts)) ## Generate logical with all FALSE as placeholder
    interval_mask_logical[interval_masked_index] <- TRUE # Vector where the only TRUE corresponds to index of ONE interval masked value 
    
    ## Logical indicating which values are neither naively masked not interval masked
    not_masked_logical <- not_naively_masked_logical & !interval_mask_logical
    ## Vector of values from non-masked categories
    not_masked_values <- counts[not_masked_logical]
    
    ## 2. Generate interval according to formula on slides, based on computable values
    ## The intuition for the size of the interval is as follows.
    ## 1. We substract from the total the sum of all values that will not be masked 
    ## (i.e., the values that are not lower than the threshold or the previosuly selected
    ## interval masked value). 
    ## 2. The lower interval value is the value that the interval masked count would take if every of 
    ## of the naively masked values (values below threshold) happened to be the larger possible
    ## value for them, i.e., threshold - 1. Thus, number of naively masked values * threshold - 1
    ## 3. The upper interval value is the value that the interval masked count would take if every of 
    ## the naively masked values happened to be equal to the lower value each can take (always 1).
    ## I.e., naively masked values - 1
    ## Note that the formula does not use the actual value for the count that is interval masked.
    ## However, by construction, the actual value for the count will *always* fall in this interval
    
    ## Now, we generate the interval
    theoretical.upper.int <- total - sum(not_masked_values) - n_masked*1
    theoretical.lower.int <- total - sum(not_masked_values) - n_masked*(threshold-1)
    ## 2.1 We know the lower value of the interval is equal or higher to the threshold.
    ## This is because, if the lower value were lower than the threshold, if we have been naively masked and not interval masked!
    ## Thus, the effective lower interval is:
    effective.lower.int <- max(theoretical.lower.int, threshold)
    ## 3 Mask interval masked variable
    if(percentage == TRUE){
      interval_mask_sub_lower <- (effective.lower.int/total)*100
      interval_mask_sub_upper <- (theoretical.upper.int/total)*100
      interval_mask_sub <- paste0('[', interval_mask_sub_lower, '-', interval_mask_sub_upper, ']')
    } else {
      interval_mask_sub <- paste0('[',effective.lower.int, '-', theoretical.upper.int, ']')
    }
    
    masked_counts[interval_masked_index] <- interval_mask_sub
    
  }
  
  ## 3. Mask values lower than threshold
  masked_counts <- ifelse(counts < threshold, below_threshold_sub, masked_counts)
  
  input_vector[numeric_indices] <- masked_counts
  output_vector <- input_vector
  
  ## WARNINGS
  ## Optional
  ##### Why is the warning triggered? 
  ## Masking implies than values lower than threshold are stated to fall in the interval [1,threshold]
  ## When this masking is repeated for multiple values, one desirable property is that 
  ## the masked table (if there's one, also the masked value) is compatible with the masked table
  ## I.e., for two masked values and threshold 5, counts that are compatible with table are all of:
  ## [1,1], [1,2], [1,3], [1,4], [2,1] ... until [4,4]
  ## While this is desirable, this is simply not possible for some tables.
  ## The warning is thrown when this is not possible
  ## Additionally, it makes explicit which value combinations of the original counts are not compatible
  ## with the masked table
  if(output_warnings == TRUE) {
    
    if ((exists("theoretical.lower.int") && theoretical.lower.int < threshold) | all(naively_masked_logical)) {
      # To understand which values are not possible, recall how intervals are computed
      # Upper value of interval is the value the interval masked count would have taken
      # IF all the naively interval values are equal to 1. By construction, this value is never modified
      # Lower value of interval is the value the interval masked count would have take
      # IF all the naively interval values are equal to threshold - 1.
      # This value is truncated when it's lower than the thresholds is the threshold
      # Thus, by definition, combinations were naively masked intervals are 'too high'
      # won't possible.
      # Concretely, after substracting from the total the sum of the values that are not masked,
      # and the effective lower bound of the interval (which is equal to te threshold)
      # we obtain the maximum quantity that the naively masked values can add to
      # The total, sum of values not masked, and effective lower bound of the interval
      # are bounds that one cannot modify. 
      # When you make this substraction, the result is the 'remaining' quantity. You can
      # split that remaining across the naively masked values in different ways, but never in 
      # a way that adds up to something that is higher than that value
      
      ## The values that are incompatible with the table are different depending on whether there's one interval masked value
      ##
      if(any(not_naively_masked_logical)) {
        maximum_to_split <- total - sum(not_masked_values) - effective.lower.int}
      if(all(naively_masked_logical)) {
        maximum_to_split <- total
      }
      
      # 1. Now, create a grid of all the combinations of values in the intervals [1, threshold]
      naive_interval <- 1:(threshold - 1) # interval to mask individual counts enforced by dap
      combinations_grid <- do.call(expand.grid, replicate(n_masked, naive_interval, simplify = FALSE))
      # 2. Compute what each potential combination of naively masked values add up to
      combinations_grid$sums <- rowSums(combinations_grid)
      # 3. Compute which interval combinations are possible and which are impossible
      combinations_grid$possible <- ifelse(combinations_grid$sums > maximum_to_split, 'Impossible', 'Possible')
      
      # 4. Compute percentage
      n_possible <- length(combinations_grid$possible[combinations_grid$possible == 'Possible'])
      n_impossible <- length(combinations_grid$possible[combinations_grid$possible == 'Impossible'])
      n <- length(combinations_grid$possible)
      
      perc_impossible <- paste0((n_impossible/n)*100, '%')
      
      warning(paste0('This table may not be safe. Of all possible combinations of naively masked values, ', 
                     perc_impossible, ' value combinations can be excluded'))
      ## Print/store possible value combinations? I.e., print interval_combs?
      ## TO-DO. Also think about how to present this information (e.g. consider permutations as one?)
      # Order dataframe
      combinations_grid <- combinations_grid[order(combinations_grid$sums),]
      print(combinations_grid)
    }
  }
  
  return(output_vector)
  
}

#' Interval mask a tableout
#'
#' @param tableout Table 1 output as outputted by the function DescriptivesTable
#' @param threshold Numeric. Threshold below which values need to be masked, determined externally
#' @param output_warnings Logical. Should warning be issued displaying which value combinations are not possible?
#'
#' @returns
#' @export
#'
#' @examples
mask_vector_wrapper <- function(tableout, threshold = 5, output_warnings = FALSE){
  # 1. Filter applicable variable types
  tableout_cat <- tableout %>% 
    as.data.frame() %>% 
    filter(type %in% c('CAT', 'TF'))
  # 2. Obtain unique categories
  categories <- unique(tableout_cat$var)
  
  # This part will be iterated later on
  for(i in seq_along(categories)){
    cat <- categories[i]
    print(i)
    # Filter only one category
    tableout_reduced <- tableout_cat[tableout_cat$var == cat,]
    
    # Extract control and exposed vector
    v1_control_raw <- as.numeric(tableout_reduced$V1_CONTROL)
    v1_exposed_raw <- as.numeric(tableout_reduced$V1_EXPOSED)
    
    # Apply interval mask function
    mask_v1_control_raw <- mask_vector(v1_control_raw, threshold = threshold, output_warnings = output_warnings)
    mask_v1_exposed_raw <- mask_vector(v1_exposed_raw, threshold = threshold, output_warnings = output_warnings)
    mask_v1_control_pcn <- mask_vector(v1_control_raw, threshold = threshold, output_warnings = output_warnings, percentage = TRUE)
    mask_v1_exposed_pcn <- mask_vector(v1_exposed_raw, threshold = threshold, output_warnings = output_warnings, percentage = TRUE)
    
    # Get row indices of first and last ocurrence
    first_index <- which(tableout$var == cat)[1]
    last_index <- tail(which(tableout$var == cat), n = 1)
    
    # Replace appropriate rows and columns, based on index and column name
    tableout[first_index:last_index, 'V1_CONTROL'] <- mask_v1_control_raw
    tableout[first_index:last_index, 'V1_EXPOSED'] <- mask_v1_exposed_raw
    tableout[first_index:last_index, 'V2_CONTROL'] <- mask_v1_control_pcn
    tableout[first_index:last_index, 'V2_EXPOSED'] <- mask_v1_exposed_pcn
  }
  
  return(tableout)
  
}