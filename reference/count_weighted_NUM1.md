# Weighted count function for numeric variables which returns mean, SD and median, Q1, Q3 (type 1)

Weighted count function for numeric variables which returns mean, SD and
median, Q1, Q3 (type 1)

## Usage

``` r
count_weighted_NUM1(data, varName, popN = NA, expectedCat = NA, weightName)
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

two rows containing; i) weighted mean, weighted sd, and ii) weighted
median, Q1, Q3
