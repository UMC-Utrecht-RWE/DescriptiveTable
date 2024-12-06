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

interval_mask <- function(x,threshold = 5){
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
  
  masked_counts <- ifelse(counts <5, '[1-4]', counts)
  masked_counts <- ifelse(counts == is_interval_masked, 
                          paste0('[',lower.int, '-', upper.int, ']'),
                          masked_counts)
  return(masked_counts)
  
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

