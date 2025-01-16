
require(stddiff)
require(data.table)
require(stringr)


# settings
DAP <- "TEST1"

# global parameters (interact with get() in table metadata)
study_quarters_all <- c("2021 Q1",
"2021 Q2",
"2021 Q3",
"2021 Q4",
"2022 Q1",
"2022 Q2",
"2022 Q3",
"2022 Q4",
"2023 Q1")

# ---------------------------------------------------
# ------- Load Functions, Preliminaries -------------
# ---------------------------------------------------

# functions to compute counts, percentages (from M_SC files + metadata)
source("functions/count_functions.R")

# function to compute absolute standardized differences (from M_SC files + metadata)
source("functions/asd_functions.R")

# function to create the baseline descriptives
source("functions/DescriptivesTable.R")

# additional function to mask/round numeric vectors to strings
source("functions/rounding_masking.R")

# load helper information on what variables are expected to be missing in the DAP
ExpectedMissingVars <- data.table::fread("input/ExpectedMissingVariables.csv")
ExpectedMissingVars <- ExpectedMissingVars[get(DAP) %in% TRUE, VarName]

# load helper information value-labels of categorical variables
label_lookup <- readRDS("input/label_lookup.rds")

# load specification of the table
table_metadata <- fread("input/BaselineDescriptives_metadata.csv")
  # "parent" variables and categories determine the denominator used to compute percentages for that variable

# read dataset of interest
popdf <- readRDS("input/example_cohort_dataset.rds")

# ------------------------------------------------------------
# ------------------ Baseline Descriptives Table -------------
# ------------------------------------------------------------

# create table
tableout <- DescriptivesTable(popdf = popdf,# data object
                  table_metadata = table_metadata, # specification of table
                  groupcol = "group", # name of column in which group membership can be found
                  output_format = "raw" ,# "processed" or "raw"; raw for debugging only, outputs at earlier step
                  calculate_asd = TRUE, # option; calculate asd and add to table or not
                  keep_varinfo = FALSE, # FALSE keeps only the end-labels, TRUE also outputs var names etc.
                  label_lookup = label_lookup,
                  control_types = FALSE, # FALSE keeps only the end-labels, TRUE also outputs var names etc.
                  missing_vars = ExpectedMissingVars,
                  missing_flag = -99, # flag numeric for missing variable
                  round_decimals = 2,
                  use_weights = FALSE,
                  output_asd = FALSE)

# optional; re-arranging of columns to sensible order
tableout[,c("label",paste0("V",1:3,"_EXPOSED"), paste0("V",1:3,"_CONTROL"))]


# ----- basic post-processing -----

# optional masking rules implemented in seperate functions
 # round_to; round to decimal places as chr. string, return <0 if resulting numeric zero
sapply(tableout$perc_control, function(s) round_to(s,2))

 # mask_to ; mask cell counts
mask_count(tableout$N_control,5)

# all together
mask_round_descriptives(tableout)

# # for clarity, this is what V1-V3 represent
setnames(tableout,c(paste0("V",1:3,"_EXPOSED"), paste0("V",1:3,"_CONTROL")),
                    c("N_exposed","perc_exposed","aux_exposed",
                      "N_control","perc_control","aux_control")
)
