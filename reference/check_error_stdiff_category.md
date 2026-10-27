# Internal function used to check if computation of categorical ASDs is possible

Internal function used to check if computation of categorical ASDs is
possible

## Usage

``` r
check_error_stdiff_category(data, gcol, vcol, group_names)
```

## Arguments

- data:

  input data frame, containing a group column (gcol),variable column
  (vcol)

- gcol:

  index of the group column

- vcol:

  index of the variable column for which the ASD is to be computed

- group_names:

  levels of the group variable of interest for output display. Defaults
  to CONTROL and EXPOSED
