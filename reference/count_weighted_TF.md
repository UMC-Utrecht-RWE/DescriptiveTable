# Weighted count function for binary (true/false) variables

Weighted count function for binary (true/false) variables

## Usage

``` r
count_weighted_TF(data, varName, popN, expectedCat = NA, weightName)
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

one row containing the summed weights of TRUE units and their percentage
of popN
