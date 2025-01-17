# function to round vectors
# nsmall is the number of digits for rounding, returns a character string
round_vec <- function(x, nsmall = 1){
  sapply(x, function(s){
    # check if exactly zero
    if(s %in% c("NE","NA",NA,NaN,"","NR")) return(s)
    s <- as.numeric(s)
    if(s == 0) return(paste0("0", ifelse(nsmall >0, ".",""), paste0(rep("0", nsmall), collapse = "")))
    # if not, round and then check if rounded value approx 0
    rounded_value <- round(s, nsmall)
    rounded_string <- formatC(rounded_value, format = "f", digits = nsmall)
    
    if (rounded_string == paste0("0", ifelse(nsmall >0, ".",""), paste0(rep("0", nsmall), collapse = "")) |
        rounded_string == paste0("-0", ifelse(nsmall >0, ".",""), paste0(rep("0", nsmall), collapse = ""))) {
      return(paste0("<0",ifelse(nsmall >0, ".",""),paste0(rep("0", nsmall-1),"1",collapse = "")))
    } else {
      return(rounded_string)
    }
  })
}

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

## function to interval_mask vectors

## input: 
## - x: numeric vector of counts (complete and without NAs)
## - threshold: number below which to mask counts
## - output warnings: logical whether warnings should or should not be in the output

## output: character vector with counts below threshold masked + one interval masked value

interval_mask <- function(counts, threshold = 5, output_warnings = TRUE){
  
  ## 1. Identify masked and unmasked values (naively masked and additional interval masked value)
  ## We determine values and positions, useful when masking the vector
  naively_masked_logical <- ifelse(counts < threshold, TRUE, FALSE) # logical, values that will be naively masked
  not_naively_masked_logical <- !naively_masked_logical # logical, values that are not naively masked
  
  # Create vector of masked counts (for storage, modified later)
  masked_counts <- counts
  # Compute total count and sums (also useful later)
  total <- sum(counts) # sum of each category countss
  n_masked <- sum(naively_masked_logical) # number of masked values
  
  if(any(not_naively_masked_logical)) {
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
    masked_counts[interval_masked_index] <-  paste0('[',effective.lower.int, '-', theoretical.upper.int, ']')
    
  }
  
  ## 3. Mask values lower than threshold
  masked_counts <- ifelse(counts < 5, paste0('[1-', (threshold-1), ']'), masked_counts)
  
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
  
  return(masked_counts)
  
}

interval_masking_wrapper <- function(tableout, threshold = 5, output_warnings = FALSE){
  # 1. Filter applicable variable types
  tableout_cat <- tableout %>% 
    as.data.frame() %>% 
    filter(type != 'NUM1',
           type != 'TF', # WARNING: WILL need to be handled
           var != 'Total')
  
  # 2. Obtain unique categories
  categories <- unique(tableout_cat$var)
  
  # This part will be iterated later on
  for(i in seq_along(categories)){
    cat <- categories[i]
    print(i)
    # Filter only one category
    tableout_reduced <- tableout_cat[tableout_cat$var == cat,]
    
    # Extract control and exposed vector
    v1_control <- as.numeric(tableout_reduced$V1_CONTROL)
    v1_exposed <- as.numeric(tableout_reduced$V1_EXPOSED)
    
    # Apply interval mask function
    mask_v1_control <- interval_mask(v1_control, threshold = threshold, output_warnings = output_warnings)
    mask_v1_exposed <- interval_mask(v1_exposed, threshold = threshold, output_warnings = output_warnings)
    
    # Get row indices of first and last ocurrence
    first_index <- which(tableout$var == cat)[1]
    last_index <- tail(which(tableout$var == cat), n = 1)
    
    # Replace appropriate rows and columns, based on index and column name
    tableout[first_index:last_index, 'V1_CONTROL'] <- mask_v1_control
    tableout[first_index:last_index, 'V1_EXPOSED'] <- mask_v1_exposed
  }
  
  return(tableout)
  
}

# overall function to mask baseline descriptives coming from DescriptivesTable
mask_round_descriptives <- function(raw_table, number =5, nsmall =1){
  
  raw_table <- as.data.frame(raw_table)
  # first, identify which rows contain counts
  count_rows <- which(!raw_table$cat %in% c("STATS1","STATS2"))
  stats_rows <- which(raw_table$cat %in% c("STATS1","STATS2"))
  
  # now identify which columns contain counts and percentages (always come in pairs)
  count_cols <- grep("V1", names(raw_table), value = T)
  count_perc <- grep("V2", names(raw_table), value = T)
  aux_cols <- grep("V3", names(raw_table), value = T)
  
  # loop over columns masking appropriately
  for(i in 1:length(count_cols)){
    # now identify which of these contain rows that should be masked
    to_be_masked <- raw_table[count_rows,count_cols[i]] <5 &
      raw_table[count_rows,count_cols[i]] >0 &
      !is.na(raw_table[count_rows,count_cols[i]])
    
    # grab row numbers
    to_be_masked <- count_rows[to_be_masked]
    # mask those counts
    raw_table[to_be_masked,count_cols[i]] <- mask_count(raw_table[to_be_masked,count_cols[i]])
    # mask percentage too
    raw_table[to_be_masked,count_perc[i]] <- "NR"
    rm(to_be_masked)
  }
  
  # now apply rounding as appropriate
  for(i in 1:length(count_cols)){
    # means and sds,etc., rounded to 2 decimal places
    raw_table[stats_rows,count_cols[i]] <-  round_vec(raw_table[stats_rows,count_cols[i]],2)
    
    # percentages rounded to 2 decimal places
    raw_table[count_rows,count_perc[i]] <-  round_vec(raw_table[count_rows,count_perc[i]],2)
  }
  
  # apply roudning to aux columns
  for(i in 1:length(aux_cols)){
    raw_table[stats_rows,aux_cols[i]] <- round_vec(raw_table[stats_rows,aux_cols[i]],2)
  }
  
  # if asd reported, round appropriately
  if(any(grepl("asd",colnames(raw_table)))){
    asd_col <- grep("asd",colnames(raw_table), value = T)
    raw_table[,asd_col] <- round_vec(raw_table[,asd_col],3)
    # for asd_1 margaret also makes a special request to never display 0.000, so let's hard-code that in
    recode_vals <- which(raw_table[,asd_col] == "0.000")
    raw_table[,asd_col][recode_vals] <- "<0.001"
  }
  
  return(raw_table)
  
}

