DescriptiveTable
================

**DescriptiveTable** is an R package for creating tables of baselines
descriptives (sometimes referred to as ‘Table 1’ in epidemiological
research settings). The package is developed and maintained by the Real
World Evidence (RWE) research group at UMCU.

The inputs required are:

1)  a dataset with rows equal to units from one or more groups, and
    columns equal to covariates (with a group identifier)

2)  a metadata file defining the output table; which variables should be
    included, if any nesting of counts should be done, what information
    should be displayed, what text labels each variable should have

Based on these inputs, `DescriptivesTable()` outputs a baseline
descriptives table: A table describing, for one or more groups,
descriptive statistics such as cell counts, means, medians and
quantiles, for a list of different covariates. These tables typically
are used to characterize the covariate distribution in a population, or
describe differences in covariate distributions between two groups.

<div class="figure">

<img src="inst/extdata/table1.png" alt="Table shell example of baseline descriptives" width="100%" />
<p class="caption">

Table shell example of baseline descriptives
</p>

</div>

Although multiple packages exist for creating such tables, this package
was developed to incorporate a variety of specific needs for RWE, such
as:

1)  The calculation of **absolute standardized differences** (ASDs) to
    compare variables of different types across groups (using the
    `stddiff` package)

2)  The calculation of **weighted** ASDs (using, e.g., IPT weights)
    using custom functions

3)  **Masking** of cell counts smaller than a pre-specific integer to
    comply with privacy regulations

4)  Specification of different nesting structures, headings and display
    names using a flexible **metadata** format

5)  Wrangling of large datasets using `data.table`

# Using the package

## Installation and Dependencies

To use the package the best way is to pull the last version, then open
the R project and run the following code in the console:

``` r
devtools::install()
```

This should install the package locally, so you can then load normally:

``` r
library(DescriptiveTable)
```

Alternatively, if you have set up an authenticator for your github
account in Rstudio, you can use:

``` r
devtools::install_github("UMC-Utrecht-RWE/DescriptiveTable")
```

There are currently three main packages on which `DescriptiveTable`
depends.

- `stddiff` provides functions for computing (unweighted) ASDs
- `data.table` is the basis for how the input datasets are manipulated
- `stringr` is used internally for regular expressions

``` r
require(stddiff)
#> Loading required package: stddiff
require(data.table)
#> Loading required package: data.table
require(stringr)
#> Loading required package: stringr
```

## Inputs

The inputs required are:

1)  a dataset with rows equal to units from one or more group, and
    columns equal to covariates (with a group identifier)

2)  a metadata file defining the output table; which variables should be
    included, if any nesting of counts should be done, what information
    should be displayed, what text labels each variable should have

In addition to these two required inputs, there are a number of optional
inputs which we will describe.

### Covariate Dataset

The main input is a dataset containing information on different
covariates. Each row of this dataset is treated as a separate unit. The
columns define the covariates potentially to be described in the output
table, and an identifier column for group membership. An example is
included in the R package for your use, with a large number of columns
for testing purposes

``` r
# read dataset of interest
popdf <- readRDS(
  system.file("extdata/example/", "example_cohort_dataset.rds", package = "DescriptiveTable"))
head(popdf[,1:7])
#> Key: <person_id, id>
#>         person_id     id   group LOOKBACK_DUR         T0 L_PREGNANT_COV_CAT
#>            <char> <char>  <char>        <num>     <Date>              <num>
#> 1: #ID-000000045#     19 CONTROL    14.948665 2022-01-12                  0
#> 2: #ID-000000276#     26 CONTROL     1.448323 2022-06-19                  0
#> 3: #ID-000000328#     10 CONTROL    69.960301 2021-09-14                  0
#> 4: #ID-000000333#      1 EXPOSED    45.952088 2021-04-29                  0
#> 5: #ID-000000347#     22 EXPOSED    52.807666 2022-05-06                  0
#> 6: #ID-000000391#     24 CONTROL    55.389459 2022-05-27                  0
#>    I_COVID19DX_COV
#>              <int>
#> 1:               0
#> 2:               0
#> 3:               0
#> 4:               0
#> 5:               0
#> 6:               0
```

### Metadata

`DescriptiveTable` uses a metadata file to specify the desired output.
The metadata file contains one row per variable to be included in the
table, in order of display, and defines the following columns:

- `var` the name of the variable (column) in the input covariate dataset
- `type` determines how the input variable is treated - how the ASD is
  computed, and what type of information is displayed. The options are:
  - `TF` (binary, true/false, count for TRUE displayed),
  - `CAT` (categorical, display count and percentage),
  - `NUM1` (numeric, display mean and SD on one row, median Q1 and Q3 on
    second) or
  - `NUM2` (numeric, display median Q1 Q3 on one row, min-max on
    second).
- `expectedCat` defines which category values should be displayed in the
  table. Can be a string listing category values of interest, or a
  string of format `get <object_name>` where `<object_name>` is a vector
  in the global environment containing all category values to be listed.
- `label` what display name to give this variable.
- `parent` and `parent_cat` defines the denominator for cell counts. By
  default, `parent = NA` means the overall table total is used as a
  denominator. Otherwise a parent `var` name must be specified, with a
  corresponding parent variable category.
- `header` defines an optional header string to be displayed before this
  variable, for instance, if a cluster of related variables are to be
  displayed

An example with a variety of input options specified is provided in the
package `extdata` folder.

``` r
# load specification of the table
# "parent" variables and categories determine the denominator used to compute percentages for that variable
table_metadata <- fread(
  system.file('extdata/example/', 'BaselineDescriptives_metadata.csv', package = 'DescriptiveTable'))
```

As we can see from the example metadata, we have an `expectedCat` entry
which points to a global object called `study_quarters_all`. To run the
example code, we need to specify any such objects in the global
environment

``` r
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
```

### Optional inputs

Two additional optional inputs, which can be passed as vectors to the
`DescriptivesTable` function, are of note. The first of these is a
lookup table or dictionary, which maps integer values of **categorical**
variables in the dataset to meaningful labels to be displayed in the
table. An example is supplied

``` r
# load helper information value-labels of categorical variables
label_lookup <- readRDS(
  system.file("extdata/example/", "label_lookup.rds", package = "DescriptiveTable"))

label_lookup[4:9,]
#>         VarName category integerVal
#> 7     L_SEX_COV   Female          1
#> 8     L_SEX_COV     Male          2
#> 19 PREG_QUALITY    green          1
#> 20 PREG_QUALITY   yellow          2
#> 21 PREG_QUALITY     blue          3
#> 22 PREG_QUALITY      red          4
```

A second piece of information which can be supplied is a vector which
defines which variables should be treated as missing. For this it is
important to remember that scripts may be deployed in different
data-access providers, some of whom may not have access to all of the
study variables of interest. For instance, one DAP may not have access
to smoking behaviour, while another does. For consistency sake, the
input dataset for both DAPs may contain a column reflecting the smoking
study variable, but this will be filled with dummy (0) values for the
DAP in which this variable is not measured. To work around this we may
load an additional metadata file and pass relevant information to the
function

``` r
# settings
DAP <- "TEST1"

# load helper information on what variables are expected to be missing in the DAP
ExpectedMissingVars <- data.table::fread(
    system.file("extdata/example/", "ExpectedMissingVariables.csv", package = "DescriptiveTable"))
ExpectedMissingVars <- ExpectedMissingVars[get(DAP) %in% TRUE, VarName]
```

## Create Baseline Descriptive tables

The function `DescriptivesTable()` creates a baseline descriptive table,
stratified by a grouping variable. Here, we illustrate its use with the
example inputs specified above. You can check function options by typing
?DescriptivesTable in the R console.

``` r
# create table
tableout <- DescriptivesTable(popdf = popdf,# data object
                  table_metadata = table_metadata, # specification of table
                  groupcol = "group", # name of column in which group membership (e.g. exposed vs control) can be found
                  output_format = "processed" ,# "processed" or "raw"; raw for debugging only, outputs at earlier step
                  calculate_asd = TRUE, # option; calculate ASD and add to table or not
                  keep_varinfo = TRUE, # FALSE keeps only the end-labels, TRUE also outputs var names etc.
                  label_lookup = label_lookup,
                  missing_vars = ExpectedMissingVars,
                  missing_flag = -99, # numeric flag for missing variables
                  round_decimals = 2, # to how many decimal places should be rounded
                  output_asd = FALSE
)

print(tableout[,c("label",paste0("V", 1:3,"_EXPOSED"), paste0("V",1:3,"_CONTROL"), "asd_1")])
#>                               label V1_EXPOSED V2_EXPOSED V3_EXPOSED V1_CONTROL
#>                              <char>      <num>      <num>      <num>      <num>
#>   1:                          Total         26         NA         NA         26
#>   2: Calendar quarter at index date         NA         NA         NA         NA
#>   3:                        2021 Q1          0       0.00         NA          0
#>   4:                        2021 Q2          0       0.00         NA          0
#>   5:                        2021 Q3          0       0.00         NA          0
#>  ---                                                                           
#>  97:         Frailty score category         NA         NA         NA         NA
#>  98:                        0 - < 1          0       0.00         NA          0
#>  99:                        1 - < 2         25      96.15         NA         25
#> 100:                        2 - < 3          1       3.85         NA          1
#> 101:                      3 or more          0       0.00         NA          0
#>      V2_CONTROL V3_CONTROL asd_1
#>           <num>      <num> <num>
#>   1:         NA         NA    NA
#>   2:         NA         NA     0
#>   3:       0.00         NA    NA
#>   4:       0.00         NA    NA
#>   5:       0.00         NA    NA
#>  ---                            
#>  97:         NA         NA     0
#>  98:       0.00         NA    NA
#>  99:      96.15         NA    NA
#> 100:       3.85         NA    NA
#> 101:       0.00         NA    NA
```

Notice that the table contains all of the numeric information we need,
but that the table would still need to be processed for aesthetic
reasons to be “report-ready”. For instance, the column names are not
particularly informative, nor are the row labels formatted (bold,
indented) as needed. We do not currently have functionality developed
for this post-processing (merging columns, changing column names, adding
indents, dropping unneeded columns etc.).

Note that currently columns can contain a mix of information depending
on what the metadata specifies. To guide interpretation, we could rename
the columns, which always follow the following scheme. By default, there
will be two columns specifying the N count (V1) and corresponding
percentage (V2) per group for a category or variable. If NUM1 or NUM2
type specified in the metadata, then these columns will contain, e.g.,
means and SDs, or median, Q1 and Q3 quartiles. When three pieces of
information are requested (e.g., Q3 being the third) this is stored in
Q3. A more informative naming scheme would be as follows

``` r
setnames(tableout,c(paste0("V",1:3,"_EXPOSED"), paste0("V",1:3,"_CONTROL")),
        c("N_exposed","perc_exposed","aux_exposed",
          "N_control","perc_control","aux_control")
)
```

The remainder of this readme will examine additional tools and
operations in the package.

## Apply masking

Masking refers to the process of replacing true cell values with some
alternative value. Typically we aim to mask sub-category counts below a
certain threshold, very often 5. The information which replaces cell
values less than 5 is an interval 1-4.

Counts are also masked if at least one such small value exists, then one
additional large count (the largest one) is also masked into an interval
to prevent back-calculating the small values from the total.

A wrapper function mask_vector_wrapper applies the masking function to
all relevant columns of the descriptive table. Here, we illustrate its
use with a threshold of 5. You can check function options by typing
?mask_vector_wrapper in the R console. The function takes care of
masking of subcategory values when the total is known. The input is the
table outputted above.

The wrapper is built on top of the function mask_vector, which can be
used to mask individual vectors. You can check function options by
typing ?mask_vector in the R console.

``` r
# Aplly masking
tableout_masked <- mask_vector_wrapper(tableout = tableout, threshold = 5,
                                table_metadata = table_metadata,
                                count.names = c('N_control', 'N_exposed'),
                                pct.names = c('perc_control', 'perc_exposed'))

print(tableout_masked[c(1:5, 97:101),c("label", c("N_exposed","perc_exposed","aux_exposed",
          "N_control","perc_control","aux_control"), "asd_1")])
#>                              label N_exposed  perc_exposed aux_exposed
#> 1                            Total        26          <NA>          NA
#> 2   Calendar quarter at index date      <NA>          <NA>          NA
#> 3                          2021 Q1         0             0          NA
#> 4                          2021 Q2         0             0          NA
#> 5                          2021 Q3         0             0          NA
#> 97          Frailty score category      <NA>          <NA>          NA
#> 98                         0 - < 1         0             0          NA
#> 99                         1 - < 2   [22-25] [84.62-96.15]          NA
#> 100                        2 - < 3     [1-4]  [3.85-15.38]          NA
#> 101                      3 or more         0             0          NA
#>     N_control  perc_control aux_control asd_1
#> 1          26          <NA>          NA    NA
#> 2        <NA>          <NA>          NA     0
#> 3           0             0          NA    NA
#> 4           0             0          NA    NA
#> 5           0             0          NA    NA
#> 97       <NA>          <NA>          NA     0
#> 98          0             0          NA    NA
#> 99    [22-25] [84.62-96.15]          NA    NA
#> 100     [1-4]  [3.85-15.38]          NA    NA
#> 101         0             0          NA    NA
```

We would always recommend to compare the original and masked tables to
ensure that masking has been applied correctly, and to check for
possible bugs. The function status should be considered experimental.

``` r
# Compare both tables
waldo::compare(tableout,tableout_masked)
```
