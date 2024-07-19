# helper functions for masking counts, smart rounding to decomals with characer strings

# round to a given digit as a character string
  # if is, e.g., 0.00, return <0.01 for any arbitrary number of digits
round_to <- function(x, nsmall = 1){
  # check if exactly zero
  if(x %in% c("NE","NA",NA,NaN)) return(x)
  x <- as.numeric(x)
  if(x == 0) return(paste0("0", ifelse(nsmall >0, ".",""), paste0(rep("0", nsmall), collapse = "")))
  # if not, round and then check if rounded value approx 0
  rounded_value <- round(x, nsmall)
  rounded_string <- formatC(rounded_value, format = "f", digits = nsmall)
  
  if (rounded_string == paste0("0", ifelse(nsmall >0, ".",""), paste0(rep("0", nsmall), collapse = "")) |
      rounded_string == paste0("-0", ifelse(nsmall >0, ".",""), paste0(rep("0", nsmall), collapse = ""))) {
    return(paste0("<0",ifelse(nsmall >0, ".",""),paste0(rep("0", nsmall-1),"1",collapse = "")))
  } else {
    return(rounded_string)
  }
}

# function to mask counts (possibly given as strings or numeric types)
  # this is "vectorised" while round_to is not
mask_count <- function(x, number){
  unlist(sapply(x, function(s){
    if(s %in% c("NE","NA",0,NA,NaN) | grepl("<",s)) return(s)
    if(as.numeric(s) == 0) return(s)
    if(as.numeric(s) < number) return(paste0("<", number)) 
    if(!as.numeric(s) < number) return(sprintf("%1.0f", as.numeric(s)))
  }))
}

