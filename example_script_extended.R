require(stddiff)
require(data.table)
require(stringr)
###############################################
# INPUT: D4_MSC_Baseline<Type>_T0, D4_MSC_PriorAESI
# OUTPUT: Table 2, 3, 4, and 5
###############################################

print(paste("[Tables 2-5] Initializing Baseline Tables"))


# Set options -------------------------------------------------------------
DIR_INPUT <- "input/example_extended/covariates/"
DIR_METADATA <- "input/example_extended/metadata/"
DIR_HELPER <- "input/example_extended/helper_functions/"
DIR_OUTPUT <- "output/example_extended/"

# specify DEAP name and masking threshold
this_dap <- "NHR"
min_n_cell <- 5 # minimum number of counts per cell
output_format <- "raw"

# laod function to process labels
source(file.path(DIR_HELPER, "helper_funs.R"))
source('functions/compute_asd.R')
source('functions/DescriptivesTable.R')
source('functions/masking-functions.R')
source('functions/asd_helper.R')


# Load data ---------------------------------------------------------------
metadata_files <- c(
  "Pfizer1052_BaselineDescriptives_Metadata.csv",
  "Pfizer1052_BaselineComorbidities_Metadata.csv",
  "Pfizer1052_BaselineComedications_Metadata.csv",
  "Pfizer1052_T4AESIMetadata.csv"
)

# Load dictionary for integer_value:category mapping
mapping_dictionary <- fst::read_fst(file.path(DIR_METADATA,"dictionary_matching_variables.fst"))

# Load label_lookup
label_lookup <- read.csv(file.path(DIR_METADATA, "Pfizer1052_label_report_lookup.csv"), header = TRUE)

# Get label_lookup in the correct format for the table
label_lookup <- getLabelLookup(label_lookup = label_lookup, mapping_dictionary = mapping_dictionary)

rm(mapping_dictionary)

# Load expected_missing
expected_missing <- read.csv(file.path(DIR_METADATA, "Pfizer1052_expected_missing_variables.csv"), header = TRUE, sep = ";") |>
  dplyr::filter(get(this_dap) != "0") |>
  dplyr::pull(variable_id)


# Loop over metadata files and generate tables -----------------------------
# Initialize ASD table
asd_table <- NULL

for (metadata_file in metadata_files) {
  
  print(paste("Processing", metadata_file))
  
  # Load metadata file
  table_metadata <- data.table::fread(file.path(DIR_METADATA, metadata_file))
  
  # Store metadata_type for naming
  metadata_type <- gsub(".*_(Baseline)?(Descriptives|Comedications|Comorbidities|T4AESI)_?.*\\.csv", "\\2", metadata_file)
  
  # Filter table_metadata for rows where expectedCat starts with "get "
  metadata_filtered <- table_metadata |>
    dplyr::filter(stringr::str_starts(expectedCat, "get ")) |>
    dplyr::mutate(category_name = stringr::str_remove(expectedCat, "get "))  # Extract name after "get "
  
  
  # Initialize a vector to keep track of created objects
  created_objects <- c()
  
  # Iterate over each row to create environment objects
  for (i in seq_len(nrow(metadata_filtered))) {
    var_name <- metadata_filtered$var[i]
    category_object <- metadata_filtered$category_name[i]
    
    # Get corresponding categories from label_lookup
    categories <- label_lookup |>
      dplyr::filter(VarName == var_name) |>
      dplyr::pull(integerVal)
    
    # Assign categories to a variable in the environment
    assign(category_object, categories, envir = .GlobalEnv)
    
    # Track the created object name
    created_objects <- c(created_objects, category_object)
  }
  
  # Load data  
  if (metadata_type %in% c("Descriptives","Comorbidities","Comedications")) {
    D4_MSC_Baseline <- fst::read_fst(file.path(DIR_INPUT, paste0("D4_MSC_Baseline",metadata_type,"_T0.fst")))
  } else if (metadata_type == "T4AESI") {
    D4_MSC_Baseline <- fst::read_fst(file.path(DIR_INPUT, paste0("D4_MSC_PriorAESI_T0.fst")))
    
    table_metadata <- table_metadata |>
      dplyr::filter(!var %in% c("COD_MYOCARDITIS_7DAYS","COD_MYOCARDITIS_14DAYS",
                                "COD_PERICARDITIS_7DAYS","COD_PERICARDITIS_14DAYS",
                                "COMP_MYOPERI_7DAYS","COMP_MYOPERI_14DAYS",
                                "COD_SUDDEN_DEATH","COD_HOSPAD_NCO")) |>
      dplyr::mutate(label = dplyr::case_when(!label %in% c("Myocarditis (1-21 days)", "Pericarditis (1-21 days)", "Myocarditis or Pericarditis (1-21 days)") ~ label,
                                             label == "Myocarditis (1-21 days)" ~ "Myocarditis",
                                             label == "Pericarditis (1-21 days)" ~ "Pericarditis",
                                             label == "Myocarditis or Pericarditis (1-21 days)" ~ "Myocarditis or Pericarditis"))
    
  }
  
  # Make sure columns have the right format
  D4_MSC_Baseline <- convert_types(D4_MSC_Baseline, data.table::as.data.table(table_metadata))
  
  # Create descriptive table with SV_REGION and SV_SES_STATUS
  tableout <- DescriptivesTable(popdf = data.table::as.data.table(D4_MSC_Baseline), # data object
                                table_metadata = data.table::as.data.table(table_metadata), # specification of table
                                groupcol = "group", # name of column in which group membership can be found
                                output_format = output_format ,# "processed" or "raw"; raw for debugging only, outputs at earlier step
                                calculate_asd = TRUE, # option; calculate asd and add to table or not
                                keep_varinfo = FALSE, # FALSE keeps only the end-labels, TRUE also outputs var names etc.
                                label_lookup = as.data.table(label_lookup),
                                control_types = FALSE, # FALSE keeps only the end-labels, TRUE also outputs var names etc.
                                missing_vars = expected_missing,
                                missing_flag = -99, # flag numeric for missing variable
                                round_decimals = 3,
                                use_weights = FALSE,
                                output_asd = TRUE
  )
  
  # Save ASD for calculation of propensity scores
  tableout$asd$type <- metadata_type
  asd_table <- rbind(asd_table, tableout$asd[tableout$asd$var!="Total",])
  
  # Filter out SV_REGION and SV_SES_STATUS
  table_metadata <- table_metadata |>
    dplyr::filter(!var %in% c("SV_REGION", "SV_SES_STATUS"))
  
  if (metadata_type == "Descriptives") {
    # Create descriptive table without SV_REGION and SV_SES_STATUS
    tableout <- DescriptivesTable(popdf = as.data.table(D4_MSC_Baseline), # data object
                                  table_metadata = as.data.table(table_metadata), # specification of table
                                  groupcol = "group", # name of column in which group membership can be found
                                  output_format = output_format ,# "processed" or "raw"; raw for debugging only, outputs at earlier step
                                  calculate_asd = TRUE, # option; calculate asd and add to table or not
                                  keep_varinfo = FALSE, # FALSE keeps only the end-labels, TRUE also outputs var names etc.
                                  label_lookup = as.data.table(label_lookup),
                                  control_types = FALSE, # FALSE keeps only the end-labels, TRUE also outputs var names etc.
                                  missing_vars = expected_missing,
                                  missing_flag = -99, # flag numeric for missing variable
                                  round_decimals = 3,
                                  use_weights = FALSE,
                                  output_asd = TRUE
    )
  }
  
  print(paste("Finished processing", metadata_file))
  
  # # Update names
  # if (output_format == "raw") {
  #   setnames(tableout$table, c("var","type","id","cat","parentv","parent_catv","m_id",paste0("V",1:3,"_EXPOSED"), paste0("V",1:3,"_CONTROL")),
  #            c("var","type","id","cat","parentv","parent_catv","m_id","N_exposed", "perc_exposed", "aux_exposed", "N_control", "perc_control", "aux_control"))
  # } else {
  #   setnames(tableout$table, c(paste0("V",1:3,"_EXPOSED"), paste0("V",1:3,"_CONTROL")),
  #            c("N_exposed", "perc_exposed", "aux_exposed", "N_control", "perc_control", "aux_control"))
  # }
  
  # Copy tableout$table to create a masked alternative
  table_raw <- tableout$table
  table_masked <- tableout$table
  
  # # Old makind (mask columns N_exposed and N_control) to compare
  # table_masked$N_control <- mask_count(table_masked$N_control, min_n_cell)
  # table_masked$N_exposed <- mask_count(table_masked$N_exposed, min_n_cell)
  
  # New masking
  table_masked <- mask_vector_wrapper(tableout = table_raw, threshold = min_n_cell,
                                      table_metadata = table_metadata)
  
  # Write to disk masked and unmasked options
  print(paste0("Writing Table",which(metadata_file == metadata_files)+1, "_",this_dap," to", DIR_OUTPUT))
  
  write.csv(table_raw, file.path(DIR_OUTPUT, paste0(this_dap,"_","Table",which(metadata_file == metadata_files)+1,".csv")), row.names = FALSE)
  write.csv(table_masked, file.path(DIR_OUTPUT, paste0(this_dap,"_","Table",which(metadata_file == metadata_files)+1, "_masked.csv")), row.names = FALSE)
  
  saveRDS(table_raw, file.path(DIR_OUTPUT, paste0(this_dap,"_","Table",which(metadata_file == metadata_files)+1, ".rds")))
  saveRDS(table_masked, file.path(DIR_OUTPUT, paste0(this_dap,"_","Table",which(metadata_file == metadata_files)+1, "_masked.rds")))
  
  print(paste0("Table",which(metadata_file == metadata_files)+1, "_",this_dap," saved to disk successfully"))
  
  # clean created objects
  rm(created_objects)
}


# Cleanup objects ---------------------------------------------------------
rm(D4_MSC_Baseline, label_lookup, table_metadata, tableout, table_masked,
   metadata_type, asd_table, DIR_OUTPUT, min_n_cell,
   expected_missing, getLabelLookup)
gc()
