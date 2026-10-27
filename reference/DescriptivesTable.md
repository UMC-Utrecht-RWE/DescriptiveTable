# Title

Title

## Usage

``` r
DescriptivesTable(
  popdf,
  table_metadata,
  groupcol = "group",
  output_format = "processed",
  calculate_asd = TRUE,
  keep_varinfo = TRUE,
  label_lookup = NULL,
  control_types = TRUE,
  missing_vars = NULL,
  missing_flag = -99,
  round_decimals = FALSE,
  use_weights = FALSE,
  weighted_stats = FALSE,
  output_asd = TRUE,
  asd_per_level = FALSE
)
```

## Arguments

- popdf:

  input data frame / data.table containing the population of interest

- table_metadata:

  metadata describing the variables, their types and levels, headers
  etc.

- groupcol:

  name of column in which group membership can be found, defaults to
  "group"

- output_format:

  "processed" or "raw"; raw better for debugging

- calculate_asd:

  option; calculate asd and add to table or not

- keep_varinfo:

  display option; keep additional raw variable information or only
  labels

- label_lookup:

  optional table of lookup values to recode category labels

- control_types:

  optional to control resetting of types

- missing_vars:

  optional; check for variables which should be recoded as missing

- missing_flag:

  optional; flag for missing values

- round_decimals:

  defaults to FALSE; otherwise, an integer specifying rounding

- use_weights:

  defaults to FALSE; if weighted asd required, supply name of the column
  in popdf which defines these weights

- weighted_stats:

  defaults to FALSE; if TRUE, counts and descriptive statistics are
  weighted by the column named in use_weights. Weights are used as
  supplied

- output_asd:

  optional; return a list containing a table and an object specifying
  the ASDs only

- asd_per_level:

  defaults to FALSE; if TRUE, categorical variables get a per-category
  ASD on each of their category rows, in addition to the overall ASD on
  the header row

## Details

`use_weights` and `weighted_stats` can be combined as follows:

|               |                  |            |                       |
|---------------|------------------|------------|-----------------------|
| `use_weights` | `weighted_stats` | ASD column | counts and statistics |
| `FALSE`       | `FALSE`          | unweighted | unweighted            |
| `"iptw"`      | `FALSE`          | weighted   | unweighted            |
| `"iptw"`      | `TRUE`           | weighted   | weighted              |
| `FALSE`       | `TRUE`           | error      | error                 |
