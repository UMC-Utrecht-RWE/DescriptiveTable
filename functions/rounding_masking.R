# helper functions for masking counts, smart rounding to decomals with characer strings

# round to a given digit as a character string
  # if is, e.g., 0.00, return <0.01 for any arbitrary number of digits
round_to <- function(x, nsmall = 1){
  rounded_value <- round(x, nsmall)
  rounded_string <- formatC(rounded_value, format = "f", digits = nsmall)
  
  if (rounded_string == paste0("0.", paste0(rep("0", nsmall), collapse = "")) |
      rounded_string == paste0("-0.", paste0(rep("0", nsmall), collapse = ""))) {
    return(paste0("<0.",paste0(rep("0", nsmall-1),"1",collapse = "")))
  } else {
    return(trimws(rounded_string))
  }
}

# function to mask counts (possibly given as strings or numeric types)
mask_count <- function(x, number){
  unlist(sapply(x, function(s){
    if(s %in% c("NE","NA",NA,NaN) | grepl("<",s)) return(as.character(s))
    if(as.numeric(s) < number) return(paste0("<", number)) 
    if(!as.numeric(s) < number) return(sprintf("%1.0f", as.numeric(s)))
  }))
}
