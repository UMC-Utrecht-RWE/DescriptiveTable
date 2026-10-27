# Count function for numeric variables which returns mean, SD and median, Q1, Q3 (type 1)

Count function for numeric variables which returns mean, SD and median,
Q1, Q3 (type 1)

## Usage

``` r
count_NUM1(data, varName, popN = NA, expectedCat = NA)
```

## Arguments

- data:

  input data.table

- varName:

  name of the variable column in data of interest

- popN:

  denominator; population N, numeric

- expectedCat:

  vector of expected categories; NA, unused in this function

## Value

two rows containing; i) mean, sd, and ii) median, Q1, Q3
