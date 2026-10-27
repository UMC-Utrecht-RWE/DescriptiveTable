# Mask one vector.

Inputs a vector of counts corresponding to one variable. Each element is
the count for each possible categories of that variable. Returns the
masked version of the input. If there are any, ignores exception values.
The following values are masked 1) all counts below the threshold are
mapped to '\<threshold', 2) if there is at least one count higher than
the threshold, the largest of those is mapped to an interval built
according to prespecified rules.

## Usage

``` r
mask_vector(
  input_vector,
  threshold = 5,
  rounding_digits = 2,
  output_warnings = TRUE,
  percentage = FALSE,
  total = NULL
)
```

## Arguments

- input_vector:

  Numeric or character vector of counts. Possibly including special
  values such as NA, NE, etc. Non-special values should be positive
  integer counts.

- threshold:

  Integer. Threshold below which values need to be masked. Determined
  externally.

- rounding_digits:

  Integer. Number of digits to round percentages to.

- output_warnings:

  Logical. Should warning be issued displaying which value combinations
  are not possible, if any?

- percentage:

  Logical. Should output be raw numbers or corresponding percentages?

- total:

  Integer. Defaults is NULL. If NULL, total will be computed each time
  as the sum of the category counts. Otherwise, provided value will be
  used. Relevant for interval masking.

## Value

If no masking is needed, the original vector or its percentage version.
If masking is needed, the masked vector or its percentage version.
