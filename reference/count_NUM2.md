# Count function for numeric variables which returns median, Q1, Q3 and min, max (type 2)

Count function for numeric variables which returns median, Q1, Q3 and
min, max (type 2)

## Usage

``` r
count_NUM2(data, varName, popN = NA, expectedCat = NA)
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

two rows containing; i) median, Q1, Q3, ii) min, max
