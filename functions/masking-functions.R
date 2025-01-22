#' Helper: check if a category value is a valid count or an exception value
#'
#' @param value Category count or corresponding exception value
#'
#' @returns Logical indicating whether the value is within the exception values.
#' If TRUE, category count is not a positive integer, and thus is not subject to masking. If FALSE, the value is a proper count that may be subject to masking.
#' @export
#'
#' @examples
is_exception <- function(value){
  ifelse(value %in% c(0, "NE","NA",NA,NaN,"","NR", -66, -77, -88, -99), TRUE,FALSE)
}


#' Mask one vector.
#' 
#' Inputs a vector of categorical counts. All counts correspond to one variable. Returns the masked version of the input. Ignores exception values.
#' Concretely, it masks a) if there is any, all values below the threshold and 2) if any value is higher than the threshold, the largest of those, according to pre-specified rules.
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
  
  ###### 1. IDENTIFY COUNTS AND INGORE EXCEPTIONS #####
  # Flag and store positive integers (equivalently, non-exceptions values)
  # Function works by applying masking to positive counts, and leaving all exceptions unchanged
  positive_counts_indices <- !sapply(input_vector, is_exception)
  counts <- as.numeric(input_vector[positive_counts_indices])
  
  ##### 2. CREATE STRING TO SUBSTITUTE VALUES BELOW THRESHOLD
  
  # Compute total: sum of all category counts (ignores if exceptions should correspond to a non-zero value)
  total <- sum(counts) 
  # Create string to replace masked values below the threshold. String is different depending on whether result is
  # integers or percentages.
  if(percentage){
    below_threshold_pcn_lower <- (1/total)*100
    below_threshold_pcn_upper <- ((threshold-1)/total)*100
    below_threshold_sub <- paste0('[',below_threshold_pcn_lower, '-' , below_threshold_pcn_upper, ']')
  } else {
    below_threshold_sub <- paste0('[1-', threshold-1, ']')
  }
  
  ###### 3. HANDLE INPUTS OF LENGTH 1 
  
  # Handle inputs of length 1: types TF or cat with one category
  ## If the input is non_numeric, left as is
  if(length(input_vector) == 1 & is_exception(input_vector[1])) return(input_vector)
  ## Input may be of length >1, even if there is only with positive integer, due to exception values
  ## In that case, apply proper masking to that count and leave other values as is
  if(length(counts) == 1){
    # Substitute count according to rule
    input_vector[positive_counts_indices] <- ifelse(counts < threshold, below_threshold_sub, counts)
    return(input_vector)
  }
  
  ###### 4. APPLY MASKING TO NON-ZERO COUNTS
  
  ## 1. Identify masked and unmasked values 
  ## Masked values are 'naively' masked (below threshold) and additional interval masked value.
  
  ## Create logical vectors: IS or NOT naively masked
  naively_masked_logical <- ifelse(counts < threshold, TRUE, FALSE) # logical, values that will be naively masked
  not_naively_masked_logical <- !naively_masked_logical # logical, values that are not naively masked
  
  # Create vector 'masked counts'. Masking is applied later.
  # Vector depends on output type (percentage or integers)
  if(percentage==TRUE){
    masked_counts <- (counts/total)*100
  } else {
    masked_counts <- counts
  }
  
  ###### 4.1. APPLY INTERVAL MASKING
  
  # Compute number of masked values, needed for both warnings and computing bounds of masked interval
  n_masked <- sum(naively_masked_logical) 
  
  ###### 4.1.1 Compute and apply interval masking
  # Interval masking is only applied when both conditions are true: 
  # 1) not all values are naively masked
  # 2) at least one value is naively masked, i.e., not all values are unmasked (in which case nothing is necessary)
  
  if(any(not_naively_masked_logical & !all(not_naively_masked_logical))) {
    
    ########## SELECT COUNT WHICH IS INTERVAL MASKED
    ## The count to be interval masked is the largest count that is not naively masked
    ## This might not be uniquely defined, as two or more categories could have the same max count.
    ## E.g., counts like: 3,3, 18, 18. You only want to interval mask one of the 18s, not both
    ## (see tests for more examples)
    ## We account for this by selecting only one of multiple candidates
    
    # maximum value among not naively masked
    interval_masked_value <- max(counts[not_naively_masked_logical]) 
    # get index of first category count equal to maximum value 
    ## (works if vector has length 1, i.e., the maximum value is uniquely identified)
    interval_masked_index <- which(counts == interval_masked_value)[1] 
    ## Generate logical indicating which value is the one interval masked 
    interval_mask_logical <- rep(FALSE, length(counts)) ## All FALSE as placehold
    interval_mask_logical[interval_masked_index] <- TRUE # Vector where the only TRUE corresponds to index of ONE interval masked value 
    
    
    
    ####### COMPUTE INTERVAL ########
    
    ## Generate interval according to formula on slides.
    ## The intuition for the bounds  of the interval is as follows.
    ## 1. We substract from the total the sum of all values that will not be masked 
    ## (i.e., the values that are not lower than the threshold or the previosuly selected
    ## interval masked value). 
    ## After this, we have the total and the sum of all the values that are neither naively not interval masked
    ## Now, one scenario is that all the naively masked values would, if unmasked, take the value 1
    ## The upper bound of the interval is the value that would make this scenario possible:
    ### The sum of all the naively masked values being one, the upper bound of the interval and the unmasked values
    ### is equal to the total.
    ## Another scenario is that all the naivel ymasked values would, if unmasked, take the value 
    ## threshold - 1. The lower bound of the interval is the value that satisfies this:
    ## the sum of all naively masked values being threshold - 1, and the lower bound of the interval,
    ## and the unmasked values is equal to the total.
    ## IMPORTANTLY: in some cases there is no way to create an interval like this, see below
    
    ## Note that the formula does not use the actual value for the count that is interval masked.
    ## However, by construction, the actual value for the count will *always* fall in this interval
    
    ## Logical indicating which values are neither naively masked not interval masked
    not_masked_logical <- not_naively_masked_logical & !interval_mask_logical
    ## Vector of values from non-masked categories
    not_masked_values <- counts[not_masked_logical]
    
    ## Now, we generate the interval
    theoretical.upper.int <- total - sum(not_masked_values) - n_masked*1
    theoretical.lower.int <- total - sum(not_masked_values) - n_masked*(threshold-1)
    
    #### IMPORTANT EXCEPTION:
    ## We KNOW that the lower bound of the interval masked value has to be equal or higher than the threshold
    ## This is because, if the lower value were lower than the threshold, if we have been naively masked and not interval masked!
    ## Thus, the effective lower interval is:
    
    effective.lower.int <- max(theoretical.lower.int, threshold)
    
    ####### MASK INTERVAL ##########
    
    ####### APPLY THE MASKING
  
    ### First, generate string for substitution.
    ## As before, this string depends on whether output is percentage or integers
    if(percentage == TRUE){
      interval_mask_sub_lower <- (effective.lower.int/total)*100
      interval_mask_sub_upper <- (theoretical.upper.int/total)*100
      interval_mask_sub <- paste0('[', interval_mask_sub_lower, '-', interval_mask_sub_upper, ']')
    } else {
      interval_mask_sub <- paste0('[',effective.lower.int, '-', theoretical.upper.int, ']')
    }
    
    ### Then, substitute value in masked counts
    masked_counts[interval_masked_index] <- interval_mask_sub
    
  }
  
  ###### 4.2. APPLY 'NAIVE' MASKING
  
  ## Simply mask values lower than threshold
  ## Note that, if there's no value to be interval masked, this is irrelevant.
  ## If there is, this was already modified in the previous chunk
  
  masked_counts <- ifelse(counts < threshold, below_threshold_sub, masked_counts)
  
  ####### 5. SUBSTITUTE MASKED COUNTS IN OUPUT VECTOR
  
  ## (in this way, exceptions are left as is)
  input_vector[positive_counts_indices] <- masked_counts
  output_vector <- input_vector ## THIS IS THE RETURN VALUE
  
  ####### 6. (OPTIONAL) WARNING
  ##### When and why is the warning triggered? 
  ## Masking implies than values lower than threshold are reported to fall in the interval [1,threshold]
  ## When this masking is repeated for multiple values, one desirable property is that the masked values
  ## cannot be automatically computed from the (known) total. Another desirable property is that 
  ## any combination of values within the replacement interval for the naively masked values
  ## is compatible with the masked vector.
  ## I.e., for two masked values and threshold 5, counts that are compatible with table are all of:
  ## [1,1], [1,2], [1,3], [1,4], [2,1] ... until [4,4]
  ## While this is desirable, this is simply not possible for some tables.
  ## The warning is thrown when this is not possible
  ## Additionally, it makes explicit which value combinations of the original counts are not compatible
  ## with the masked table
  
  if(output_warnings == TRUE) {
    ## Warning is triggered if 
    ### a) the interval needs to be truncated or
    ### b) all the values are naively masked (in which case, if the total is knwon)
    ### not all values will be possible
    
    if ((exists("theoretical.lower.int") && theoretical.lower.int < threshold) | all(naively_masked_logical)) {
      # To understand which values are not possible, recall how intervals are computed
      # Upper value of interval is the value the interval masked count would have taken
      # IF all the naively interval values are equal to 1. By construction, this value is never modified
      # Lower value of interval is the value the interval masked count would have take
      # IF all the naively interval values are equal to threshold - 1.
      # This value is truncated when it's lower than the thresholds. 
      ## (because if it was lower than the threshold, it would not be interval but naively masked)
      # Thus, by definition, combinations were naively masked intervals are 'too high'
      # won't be possible.
      
      # Concretely, after substracting from the total the sum of the values that are not masked,
      # and the effective lower bound of the interval (which is equal to the threshold),
      # we obtain the maximum quantity that the naively masked values can add to. 
      # If the naively masked values would add up to a value higher than that, the lower bound of the interval
      # would be lower than the threshold (which is not possible)
      
      # Note that the total, sum of values not masked, and effective lower bound of the interval
      # are bounds that one cannot modify. 
      
      # After substraction, the result is the 'remaining' quantity. You can
      # split that remaining across the naively masked values in different ways, but never in 
      # a way that adds up to something that is higher than that value
      
      ## The values that are incompatible with the table are different depending on whether there's one interval masked value
      
      ## NOTE: This applies if there is an interval masked value
      ## If all the values are lower than the threshold, the maximum these values can add up to
      ## is simplyu the (assumed known) total
      if(any(not_naively_masked_logical)) {
        maximum_to_split <- total - sum(not_masked_values) - effective.lower.int}
      if(all(naively_masked_logical)) {
        maximum_to_split <- total
      }
      
      # 1. Create a grid of all the combinations of values in the intervals [1, threshold]
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
      ## @TO-DO:
      ### Decide if consider permutations as one
      ### Think about what the better way of displaying this is: as a list, print the grid directly...
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
  
  # 1. Filter applicable variable types, i.e., leave cat or tf
  tableout_var <- tableout %>% 
    as.data.frame() %>% 
    filter(type %in% c('CAT', 'TF'),
           var != 'I_HIV_ALGO') # IMPORTANT: THIS IS JUST A TEST WORKAROUND, AS THE PROBLEM SEEMS TO BE IN TABLEOUT
  # 2. Obtain unique variables,
  variables <- unique(tableout_var$var)
  
  # Iterate
  for(i in seq_along(variables)){
    var <- variables[i] # Get the variable name

    # Filter table to only that category
    tableout_reduced <- tableout_var[tableout_var$var == var,]
    # Extract vector of category couns for exposed and control groups, exposed
    v1_control_unmasked <- as.numeric(tableout_reduced$V1_CONTROL)
    v1_exposed_unmasked <- as.numeric(tableout_reduced$V1_EXPOSED)
    
    # Apply interval mask function
    mask_v1_control_masked_int <- mask_vector(v1_control_unmasked, threshold = threshold, output_warnings = output_warnings)
    mask_v1_exposed_masked_int <- mask_vector(v1_exposed_unmasked, threshold = threshold, output_warnings = output_warnings)
    mask_v1_control_pcn_int <- mask_vector(v1_control_unmasked, threshold = threshold, output_warnings = output_warnings, percentage = TRUE)
    mask_v1_exposed_pcn_int <- mask_vector(v1_exposed_unmasked, threshold = threshold, output_warnings = output_warnings, percentage = TRUE)
    
    # Get row indices of first and last ocurrence of the variable in the 
    # original descriptibles table
    first_index <- which(tableout$var == var)[1]
    last_index <- tail(which(tableout$var == var), n = 1)
    
    # Replace appropriate rows and columns, based on index of variable ocurrence and column names
    tableout[first_index:last_index, 'V1_CONTROL'] <- mask_v1_control_masked_int
    tableout[first_index:last_index, 'V1_EXPOSED'] <- mask_v1_exposed_masked_int
    tableout[first_index:last_index, 'V2_CONTROL'] <- mask_v1_control_pcn_int
    tableout[first_index:last_index, 'V2_EXPOSED'] <- mask_v1_exposed_pcn_int
  }
  
  return(tableout)
  
}