# Wrapper function to compute all ASDs for a set of variables

Wrapper function to compute all ASDs for a set of variables

## Usage

``` r
compute_asd(
  CovariatesInformation,
  metadata,
  referenceExtension = "",
  use_weights = FALSE,
  group_names = c("CONTROL", "EXPOSED"),
  asd_per_level = FALSE
)
```

## Arguments

- CovariatesInformation:

  data.table file containing all variables of interest, group variable

- metadata:

  Meta-data supplying variable name (var), type and expectedCat for
  categorical variables

- referenceExtension:

  optional, extension to name of output column

- use_weights:

  if FALSE, computes unweighted ASDs; to compute weighted ASDs, supply
  column name for weights in CovariatesInformation

- group_names:

  levels of the group variable of interest for output display. Defaults
  to CONTROL and EXPOSED

- asd_per_level:

  if TRUE, categorical variables additionally get a per-category ASD
  below their overall ASD; comparing the proportion in that category
  between the two groups
