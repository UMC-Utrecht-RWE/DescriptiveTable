# Weighted ASD for categorical variables

Weighted ASD for categorical variables

## Usage

``` r
wtd.stddiff.category(
  data,
  gcol,
  vcol,
  var,
  group = "group",
  use_weights,
  group_names = c("CONTROL", "EXPOSED")
)
```

## Arguments

- data:

  input data frame, containing a group column (gcol),variable column
  (vcol) and weights column (use_weights)

- gcol:

  index of the group column

- vcol:

  index of the variable column for which the ASD is to be computed

- var:

  name of the variable. Note: should be replaced by vcol internally to
  remove redundancy

- group:

  levels of the group variable of interest for output display. Defaults
  to CONTROL and EXPOSED

- use_weights:

  name of the column in data containing the weights

- group_names:

  levels of the group variable of interest for output display. Defaults
  to CONTROL and EXPOSED

## Details

This function is based on the unstandardized differences in the
`stddiff` package, but uses the weighted table from `Hmisc::`
