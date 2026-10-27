# Internal wrapper function to compute ASDs. Based on the stddiff package, this wrapper allows different ASD computations based on a variable-type specified by an input argument

Internal wrapper function to compute ASDs. Based on the stddiff package,
this wrapper allows different ASD computations based on a variable-type
specified by an input argument

## Usage

``` r
asd_helper(
  indf,
  y,
  x,
  met = "num",
  use_weights = FALSE,
  var = NULL,
  group_names = c("CONTROL", "EXPOSED")
)
```

## Arguments

- indf:

  input data frame, containing a group column (y) and variable
  column (x) and optional weights column

- y:

  index of the group column

- x:

  index of the variable column for which the ASD is to be computed

- met:

  method to compute the asd, can be NUM1/NUM2 for numerical, TF for
  binary, CAT for categorical

- use_weights:

  option to compute weighted ASDs. If FALSE weights are ignored.
  Otherwise supply the name of the column in the data frame in which
  weights are stored

- var:

  name of the variable; passed to function and used in weighted stddiff.
  Note: use should be deprecated, change var use to vcol

- group_names:

  levels of the group variable of interest for output display. Defaults
  to CONTROL and EXPOSED
