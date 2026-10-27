# Interval mask a tableout

Interval mask a tableout

## Usage

``` r
mask_vector_wrapper(
  tableout,
  threshold = 5,
  output_warnings = FALSE,
  rounding_digits = 2,
  count.names = c("V1_CONTROL", "V1_EXPOSED"),
  pct.names = c("V2_CONTROL", "V2_EXPOSED"),
  table_metadata
)
```

## Arguments

- tableout:

  Table 1 output as outputted by the function DescriptivesTable

- threshold:

  Numeric. Threshold below which values need to be masked, determined
  externally

- output_warnings:

  Logical. Should warning be issued displaying which value combinations
  are not possible?

- rounding_digits:

  Integer. Number of digits to round percentages to.

- count.names:

  Character vector of length 2. Names of the count columns in table

- pct.names:

  Character vector of length 2. Names of the percentage columns in table

- table_metadata:

  Metadata table with variable names and types
