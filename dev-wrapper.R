# Specify the file path
file_path <- "example_script.R"

# Read the first 40 lines of the file
lines_to_run <- readLines(file_path, n = 69)

# Evaluate the lines
eval(parse(text = lines_to_run))

library(dplyr)

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
  v1_control_raw <- as.numeric(tableout_reduced$V1_CONTROL)
  v1_exposed_raw <- as.numeric(tableout_reduced$V1_EXPOSED)

  # Apply interval mask function
  mask_v1_control_raw <- interval_mask(v1_control_raw, threshold = threshold, output_warnings = output_warnings)
  mask_v1_exposed_raw <- interval_mask(v1_exposed_raw, threshold = threshold, output_warnings = output_warnings)
  mask_v1_control_pcn <- interval_mask(v1_control_raw, threshold = threshold, output_warnings = output_warnings, percentage = TRUE)
  mask_v1_exposed_pcn <- interval_mask(v1_exposed_raw, threshold = threshold, output_warnings = output_warnings, percentage = TRUE)

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


tableout_masked <- interval_masking_wrapper(tableout)

waldo::compare(tableout_masked, tableout)

