# Weighted count function for numeric variables which returns median, Q1, Q3 and min, max (type 2)

Minimum and maximum are returned unweighted, as a weighted extreme is
not defined. They are taken directly rather than as the 0 and 1
quantiles of `wtd.quantile`.

## Usage

``` r
count_weighted_NUM2(data, varName, popN = NA, expectedCat = NA, weightName)
```

## Arguments

- data:

  input data.table containing the variable and the weights column

- varName:

  name of the variable column in data of interest

- popN:

  denominator; summed weights of the source population, numeric

- expectedCat:

  vector of expected categories; NA, unused in this function

- weightName:

  name of the weights column in data

## Value

two rows containing; i) weighted median, Q1, Q3, ii) min, max
