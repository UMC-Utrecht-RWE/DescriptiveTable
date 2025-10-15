#' @description
#' Format numbers by adding thousand separators 
#' and rounding to a relevant decimal
#' @param vec a numeric or integer vector
#' @param big_mark_seperator a separator for every thousand
#' @param rounding_digits a number of decimal digits to show
format_num <- function(vec,
                       big_mark_seperator = "",
                       rounding_digits = 2) {
  if (is.integer(vec)) vec <- formatC(vec, big.mark = big_mark_seperator)
  if (is.numeric(vec)) {
    vec <- formatC(vec,
                   big.mark = big_mark_seperator,
                   format = "f",
                   digits = rounding_digits
    )
  }
  return(vec)
}

#' Helper: check if the value for the count is equal to one of the exception values 
#' (i.e., not a valid positive count)
#'
#' @param value Category count or corresponding exception value
#'
#' @returns Logical indicating whether the value is one of the exception values.
#' If TRUE, category count is not a positive integer, and thus is not subject to masking. 
#' If FALSE, the value is a proper count that may be subject to masking.
#' @export
#'
#' @examples
is_exception <- function(value){
  ifelse(value %in% c(0, "NE","NA",NA,NaN,"","NR", -66, -77, -88, -99), TRUE,FALSE)
}


#' Mask one vector.
#' 
#' Inputs a vector of counts corresponding to one variable. Each element is
#' the count for each possible categories of that variable.
#' Returns the masked version of the input. If there are any, ignores exception values.
#' The following values are masked 1) all counts below the threshold are mapped to '<threshold',
#' 2) if there is at least one count higher than the threshold, the largest of those is mapped to an interval
#' built according to prespecified rules.
#' 
#' @param input_vector Numeric or character vector of counts. Possibly including special values such as NA, NE, etc. Non-special values should be positive integer counts.
#' @param threshold Integer. Threshold below which values need to be masked. Determined externally.
#' @param output_warnings Logical. Should warning be issued displaying which value combinations are not possible, if any?
#' @param percentage Logical. Should output be raw numbers or corresponding percentages?
#' @param total Integer. Defaults is NULL. If NULL, total will be computed each time as the sum of the category counts. Otherwise, provided value will be used. Relevant for interval masking.
#'
#' @returns If no masking is needed, the original vector or its percentage version. If masking is needed, the masked vector or its percentage version.
#' 
#' @export
#'
#' @examples
mask_vector <- function(input_vector, 
                        threshold = 5, 
                        rounding_digits = 2,
                        big_mark_seperator = "",
                        output_warnings = TRUE, 
                        percentage = FALSE,
                        total = NULL){
  
  ###### 1. IDENTIFY COUNTS AND INGORE EXCEPTIONS #####
  # Flag positive integers (equivalently, non-exceptions values)
  positive_counts_indices <- !sapply(input_vector, is_exception)
  # Store positive integers
  counts <- as.numeric(input_vector[positive_counts_indices])
  # Masking is applied to numerical counts. Exception values are left unchanged.
  
  # If not provided, compute total as the sum of each category
  if(is.null(total)){
    total <- sum(counts)
  } 
  
  # IF nothing to be masked, return input_vector with formatting.
  # This ensure vectors that need a masking procedure will be in a string format.
  if(all(input_vector >= threshold | input_vector < 1)){
    if(percentage){
      return(
        format_num(
          (input_vector/total)*100, 
          big_mark_seperator = big_mark_seperator,
          rounding_digits = rounding_digits
        )
      )
    } else {
      return(formatC(input_vector, big.mark = big_mark_seperator))
    }
  }
  
  ##### 2. CREATE STRING TO SUBSTITUTE VALUES BELOW THRESHOLD
  # Create string to replace masked values below the threshold. String is different depending on whether result is
  # integers or percentages.
  if(percentage){
    below_threshold_pcn_lower <- format_num(
      (1/total)*100, 
      big_mark_seperator = big_mark_seperator,
      rounding_digits = rounding_digits
    )
    below_threshold_pcn_upper <- format_num(
      ((threshold-1)/total)*100, 
      big_mark_seperator = big_mark_seperator,
      rounding_digits = rounding_digits
    )
    
    below_threshold_sub <- paste0('[',below_threshold_pcn_lower, '-' , below_threshold_pcn_upper, ']')
  } else {
    below_threshold_sub <- paste0('[1-', threshold-1, ']')
  }
  
  
  ###### 3. HANDLE INPUTS OF LENGTH 1
  
  # Handle inputs of length 1: types TF or cat with one category
  ## If the input is an exception left as is
  if(length(input_vector) == 1 & is_exception(input_vector[1])) {
    return(formatC(input_vector, big.mark = big_mark_seperator))
  }
  
  ## Input may be of length >1, even if there is only one non-exception value
  ## In that case, apply proper masking to that count and leave other values as is
  if(length(counts) == 1 & is.null(total)){
    # Substitute count according to rule
    input_vector[positive_counts_indices] <- ifelse(
      counts < threshold, 
      below_threshold_sub, 
      formatC(counts, big.mark = big_mark_seperator)
    )
    return(input_vector)
  }
  
  ###### 4. APPLY MASKING TO NON-ZERO COUNTS
  
  ## 1. Identify masked and unmasked values 
  ## Masked values are: 1) 'naively' masked (below threshold) and 2) additionally interval masked value.
  
  ## Create logical vectors: IS or NOT naively masked
  naively_masked_logical <- ifelse(counts < threshold, TRUE, FALSE) # logical, values that will be naively masked
  not_naively_masked_logical <- !naively_masked_logical # logical, values that are not naively masked
  
  # For storage, create vector 'masked counts'. Masking is applied later.
  # Vector depends on output type (percentage or integers)
  if(percentage==TRUE){
    masked_counts <- round((counts/total)*100, digits = rounding_digits)
  } else {
    masked_counts <- counts
  }
  
  ###### 4.1. APPLY INTERVAL MASKING
  
  # Compute number of masked values, needed for both warnings and computing bounds of masked interval
  n_masked <- sum(naively_masked_logical) 
  
  # Interval masking is only applied when both conditions are true: 
  # 1) not all values are naively masked
  # 2) at least one value is naively masked, i.e., not all values are unmasked (in which case nothing is necessary)
  
  if(any(not_naively_masked_logical) & any(naively_masked_logical)) {
    
    ########## SELECT COUNT WHICH IS INTERVAL MASKED
    ## The count to be interval masked is the largest count that is not naively masked
    ## This might not be uniquely defined, as two or more categories could have the same max count.
    ## E.g., input is: 3,3, 18, 18. You only want to interval mask one of the 18s, not both
    ## (see tests for more examples)
    ## We account for this by selecting only one of multiple candidates
    
    # maximum value among not naively masked
    interval_masked_value <- max(counts[not_naively_masked_logical]) 
    # get index of first category count equal to maximum value 
    ## (also works if vector has length 1, i.e., the maximum value is uniquely identified)
    interval_masked_index <- which(counts == interval_masked_value)[1] 
    ## Generate logical indicating which value is the one interval masked 
    interval_mask_logical <- rep(FALSE, length(counts)) ## All FALSE as placehold
    interval_mask_logical[interval_masked_index] <- TRUE # Vector where the only TRUE corresponds to index of ONE interval masked value 
    
    
    
    ####### COMPUTE INTERVAL ########
    
    ## Formula for interval is in slides
    ## Note which values will be masked are already determined: all values below threshold
    ## and, from the values above threshold, the category with the largest value [interval-masked value]
    ## Both of this have been flagged already.
    ## This section simply computes the interval to use for the masking of the largest value above 
    ## the threshold
    
    ## Some intuition for the calculation:
    ## 1. Everybody that sees the table will know the total and the values that will not be masked.
    ## 2. Everybody can compute the sum all all the values that are neither naively not interval masked
    ## 3.  We want to create an interval that reveals nothing about the naively masked values, other
    ## than they are in the interval [1-(threshold-1)]
    
    ## Imagine two scenarios
    ## 1. All the naively masked values would, if unmasked, take the value 1
    ## For this to be possible, the sum of the all the 1s + the sum of the unmasked values 
    ## + the upper bound of the interval should be equal to the total.
    ## The upper bound of the interval is the value that makes possible that all naively 
    ## masked values are equal to 1
    
    ## 2. All the naively masked values would, if unmasked, the the value [threshold - 1] (i.e.,
    ## their maximum possible value)
    ## For this to be possible, the sum of all the [threshold-1] + the sum of the unmasked values
    ## + the lower bound of the interval should be equal to the total.
    ## The lower bound of the interval is the value that makes possible that all naively masked 
    ## values are equal to threshold-1.
    
    ## Note 1: Sometimes it is not possible to create such an interval. More below.
    ## Note 2: The formula does not use the actual value for the count that is interval masked.
    ## However, by construction, the actual value for the count will *always* fall in this interval
    
    ## Logical indicating which values are neither naively masked nor interval masked
    not_masked_logical <- not_naively_masked_logical & !interval_mask_logical
    ## Vector of values from non-masked categories
    not_masked_values <- counts[not_masked_logical]
    
    ## Now, we generate the interval
    theoretical.upper.int <- total - sum(not_masked_values) - n_masked*1
    theoretical.lower.int <- total - sum(not_masked_values) - n_masked*(threshold-1)
    
    #### IMPORTANT EXCEPTION:
    ## We know the lower bound of the interval needs to be higher than the threshold.
    ## If the lower value were below the threshold, it would have been naively masked.
    ## I.e., the masking of that value would be [1, threshold-1]
    ## Thus, the effective lower interval is:
    
    effective.lower.int <- max(theoretical.lower.int, threshold)
    
    ####### MASK INTERVAL ##########
    ### First, generate string for substitution.
    ## String depends on whether output is percentage or integers
    if(percentage == TRUE){
      interval_mask_sub_lower <- format_num(
        (effective.lower.int/total)*100, 
        big_mark_seperator = big_mark_seperator,
        rounding_digits = rounding_digits
      )
      interval_mask_sub_upper <- format_num(
        (theoretical.upper.int/total)*100, 
        big_mark_seperator = big_mark_seperator,
        rounding_digits = rounding_digits
      )
      interval_mask_sub <- paste0('[', interval_mask_sub_lower, '-', interval_mask_sub_upper, ']')
    } else {
      interval_mask_sub <- paste0(
        '[',
        formatC(effective.lower.int, big.mark = big_mark_seperator),
        '-', 
        formatC(theoretical.upper.int, big.mark = big_mark_seperator),
        ']'
      )
    }
    
    ### Then, substitute value in masked counts
    if(percentage){
      masked_counts <- ifelse(
        masked_counts %in% masked_counts[interval_masked_index],
        interval_mask_sub, 
        format_num(
          masked_counts, 
          big_mark_seperator =big_mark_seperator,
          rounding_digits = rounding_digits
        )
      )
    } else {
      masked_counts <- ifelse(
        # for the values to be masked
        seq_along(masked_counts) %in% interval_masked_index,
        # replace them with the matching ones saved in 'interval_masked_index'
        interval_mask_sub[match(seq_along(masked_counts), interval_masked_index)],
        masked_counts
      )
    }
  }
  
  ###### 4.2. APPLY 'NAIVE' MASKING
  ## Note that interval masking has already been applied
  masked_counts <- ifelse(counts < threshold, below_threshold_sub, masked_counts)
  
  ####### 5. SUBSTITUTE MASKED COUNTS IN OUPUT VECTOR
  ## (in this way, exceptions are left as is)
  input_vector[positive_counts_indices] <- masked_counts
  output_vector <- input_vector ## THIS IS THE RETURN VALUE
  
  ####### 6. (OPTIONAL) WARNING
  #### Why is the warning triggered?
  ## 1. Masking means that values lower than the threshold are mapped to [1,threshold-1]
  ## 2. If multiple values are masked, there're two desirable properties:
  ## 2.a) The masked values cannot be directly computed from the total
  ## 2.b) Any combination of original values is compatible with the masking.
  ##      E.g., for two masked values and threshold 5, the original values could be
  ##      all of: [1,1], [1,2], [1,3], [1,4], [2,1] ... until [4,4]
  ##      Knowing the totals, the interval masked count, and the values for unmasked counts,
  ##      one cannot conclude any of this original value combinations is incompatible with the table
  ## Requirement 2 b) is not possible in some instances, under the masking rules implemented in this 
  ## function.
  ## The warning is thrown when requirement 2b) is not satisfied
  ## Besides, the warning shows which value combinations of the original counts are not
  ## compatible with the reported masked table
  
  if(output_warnings == TRUE & length(counts)>1) {
    ## Warning is triggered if 
    ### a) the effective lower int and theoretical.lower.int are not equal OR
    ### b) all the values are naively masked. In this case, if the total is known, not all original value combinations are possible
    
    if ((exists("theoretical.lower.int") && theoretical.lower.int != effective.lower.int) | all(naively_masked_logical)) {
      
      ## How to determine which original value combinations are incompatible with 
      ## the masked table?
      ## 1. Known quantities: total, sum of not masked values & effective lower bound of the interval.
      ## 2. Unknown: original values.
      ## 3. Substract sum of not masked values + effetive lower bound from total.
      ## 4. This is the 'remaining quantity'. The original values need to add up to at MOST this.
      #     If they add up to something higher, this is incompatible with the information. 
      #     If they add up to something lower, this will be within the interval
      
      ## Note: if all values are naively masked, the original values need to add up exactly to the total. 
      
      if(any(not_naively_masked_logical)) {
        maximum_to_split <- total - sum(not_masked_values) - effective.lower.int}
      
      # 1. Create a grid of all the combinations of values in the interval [1, threshold-1]
      naive_interval <- 1:(threshold - 1) # interval to mask individual counts enforced externally
      combinations_grid <- do.call(expand.grid, replicate(n_masked, naive_interval, simplify = FALSE))
      # 2. Compute what each potential combination of naively masked values add up to
      combinations_grid$sums <- rowSums(combinations_grid)
      # 3. Compute which interval combinations are possible and which are impossible
      if(any(not_naively_masked_logical)) {
        combinations_grid$possible <- ifelse(combinations_grid$sums > maximum_to_split, 'Impossible', 'Possible')
      }
      if(all(naively_masked_logical)) {
        combinations_grid$possible <- ifelse(combinations_grid$sums == total, 'Impossible', 'Possible')
      }
      
      # 4. Compute percentage
      n_possible <- length(combinations_grid$possible[combinations_grid$possible == 'Possible'])
      n_impossible <- length(combinations_grid$possible[combinations_grid$possible == 'Impossible'])
      n <- length(combinations_grid$possible)
      
      perc_impossible <- paste0(
        format_num(
          (n_impossible/n)*100, 
          big_mark_seperator = big_mark_seperator,
          rounding_digits = rounding_digits
        ), 
        '%'
      )
      
      warning(paste0('This table may not be safe. Of all possible combinations of naively masked values, ', 
                     perc_impossible, ' original value combinations can be excluded'))
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
mask_vector_wrapper <- function(tableout, threshold = 5, 
                                output_warnings = FALSE,
                                rounding_digits = 2,
                                big_mark_seperator = "",
                                count.names = c('V1_CONTROL', 'V1_EXPOSED'),
                                pct.names = c('V2_CONTROL', 'V2_EXPOSED'),
                                table_metadata){
  
  tableout <- as.data.frame(tableout)
  table_metada <- as.data.frame(table_metadata)
  # Gen index in metadata table and output table
  table_metadata$index <- 1:nrow(table_metadata)
  tableout$index <- NA
  
  
  # Start searching from the first row of metadata
  metadata_row <- 1
  # Loop through tableout starting from row 2 (row 1 contains totals)
  for(i in 2:nrow(tableout)){
    
    # Skip rows where var is NA or empty string - leave index as NA
    if(is.na(tableout$var[i]) || tableout$var[i] == ""){
      next  # Skip to next iteration, leaving tableout$index[i] as NA. Means this rows will not be masked (they are omitted)
    }
    
    # Reset found flag for each new variable we're trying to match
    found <- FALSE
    # Keep searching until we find a match for the current variable
    while(found == FALSE){
      if(table_metadata$var[metadata_row] == tableout$var[i]){ # Check if current metadata variable matches current tableout variableå
        # Match found! Set flag to exit while loop
        found <- TRUE
        # Copy the corresponding index from metadata to tableout
        tableout$index[i] <- table_metadata$index[metadata_row]
      } else {
        # No match - move to next row in metadata table
        metadata_row <- metadata_row + 1
        # Note: This assumes every variable in tableout exists in table_metadata
        # If not, this will cause an infinite loop when metadata_row exceeds table length
      }
    }
  }
  
  # 2. Obtain unique variables.
  variables_index <- na.omit(base::unique(tableout$index)) # omits NA values, which correspond to rows that were skipped (e.g., empty var)
  
  # Iterate over variable index and mask each vector/variable
  for(i in seq_along(variables_index)){
    #  if(i == 4){browser()}
    var_index <- variables_index[i] # Get the variable name. Note some might be repeated (same value, but different index)
    
    # Filter table to only that category
    tableout_reduced <- tableout |> dplyr::filter(index == var_index)
    
    # Masking applies only to TF and CAT vectors
    type_should_be_masked <- tableout_reduced$type[1] %in% c('TF','CAT')
    
    # For TF or cat with one category, the total is not computed, but taken from row 1
    is_TF <- tableout_reduced$type[1] == 'TF'
    
    
    if(is_TF | nrow(tableout_reduced) == 1){
      total_count1 <- as.numeric(tableout[1, count.names[1]]) # because total is in row 1
      total_count2 <- as.numeric(tableout[1, count.names[2]])
    } else {
      total_count1 <- NULL
      total_count2 <- NULL
    }
    
    if(type_should_be_masked){
      # Extract vector of category couns for exposed and control groups, exposed
      counts.1 <- as.numeric(tableout_reduced[[count.names[1]]])
      counts.2 <- as.numeric(tableout_reduced[[count.names[2]]])
      
      # Apply interval mask function
      masked.counts.1 <- mask_vector(counts.1, threshold = threshold, 
                                     rounding_digits = rounding_digits,
                                     big_mark_seperator = big_mark_seperator,
                                     output_warnings = output_warnings,
                                     total = total_count1)
      masked.counts.2 <- mask_vector(counts.2, threshold = threshold, 
                                     rounding_digits = rounding_digits,
                                     big_mark_seperator = big_mark_seperator,
                                     output_warnings = output_warnings,
                                     total = total_count2)
      masked.pcts.1 <- mask_vector(counts.1, threshold = threshold, 
                                   rounding_digits = rounding_digits,
                                   big_mark_seperator = big_mark_seperator,
                                   output_warnings = output_warnings, 
                                   total = total_count1,
                                   percentage = TRUE)
      masked.pcts.2 <- mask_vector(counts.2, threshold = threshold, 
                                   output_warnings = output_warnings, 
                                   big_mark_seperator = big_mark_seperator,
                                   rounding_digits = rounding_digits,
                                   total = total_count2,
                                   percentage = TRUE)
      
      # Get row indices of first and last ocurrence of the variable in the 
      # original descriptibles table
      first_index <- which(tableout$index == var_index)[1]
      last_index <- utils::tail(which(tableout$index == var_index), n = 1)
      
      # Replace appropriate rows and columns, based on index of variable ocurrence and column names
      tableout[first_index:last_index, count.names[1]] <- masked.counts.1
      tableout[first_index:last_index, count.names[2]] <- masked.counts.2
      tableout[first_index:last_index, pct.names[1]] <- masked.pcts.1
      tableout[first_index:last_index, pct.names[2]] <- masked.pcts.2
    }
  }
  # drop index column
  tableout <- tableout |> dplyr::select(-index)
  return(tableout)
  
}



#' @description
#' A wrapper function for mask_vector() that allows a wide format data frame
#' or data table input and apply masking across a set of columns for every row.
#' The output returns the input data of the same format with the specified columns masked.
#' 
#' @param input_row_table a data set where each column contains a variable to be masked
#' @param maskcols a string vector with the column names for the variables that 
#' should be masked together for the secondary data disclosure

mask_row_table <- function(input_row_table,
                           maskcols = c(),
                           threshold = 5,
                           rounding_digits = 2,
                           big_mark_seperator = "",
                           output_warnings = TRUE,
                           percentage = FALSE,
                           total = NULL){
  
  # check the input format
  if(!(is.data.frame(input_row_table)|
       data.table::is.data.table(input_row_table))) {
    stop("Your input for masking is not a data.frame or data.table.")
  }
  
  # ensure data.table format
  thisdat <- data.table::data.table(input_row_table)
  
  # mask across the columns for each row
  thisdat[
    , (maskcols) :=
      data.table::as.data.table(
        t(apply(.SD, 1, function(row) 
          mask_vector(row,
                      threshold = threshold,
                      rounding_digits = rounding_digits,
                      big_mark_seperator = big_mark_seperator,
                      percentage = percentage,
                      output_warnings = output_warnings,
                      total = NULL)
        )
        )
      ), .SDcols = maskcols]
  
  # covert to data.frame if the input was not data.table.
  if(!data.table::is.data.table(input_row_table)){
    thisdat <- data.frame(thisdat)
  }
  
  return(print(thisdat))
}
