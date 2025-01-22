# Specify the file path
file_path <- "example_script.R"
source('functions/masking-functions.R')
# Read the first 40 lines of the file
lines_to_run <- readLines(file_path, n = 69)

# Evaluate the lines
eval(parse(text = lines_to_run))

library(dplyr)

tableout_masked <- mask_vector_wrapper(tableout)

waldo::compare(tableout, tableout_masked)
