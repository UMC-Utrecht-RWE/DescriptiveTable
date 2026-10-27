# Weighted count function for categorical variables

Weighted count function for categorical variables

## Usage

``` r
count_weighted_CAT(data, varName, popN, expectedCat, weightName)
```

## Arguments

- data:

  input data.table containing the variable and the weights column

- varName:

  name of the variable column in data of interest

- popN:

  denominator; summed weights of the source population, numeric

- expectedCat:

  vector of expected categories

- weightName:

  name of the weights column in data

## Value

one row for every value of expectedCat
