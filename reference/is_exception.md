# Helper: check if the value for the count is equal to one of the exception values (i.e., not a valid positive count)

Helper: check if the value for the count is equal to one of the
exception values (i.e., not a valid positive count)

## Usage

``` r
is_exception(value)
```

## Arguments

- value:

  Category count or corresponding exception value

## Value

Logical indicating whether the value is one of the exception values. If
TRUE, category count is not a positive integer, and thus is not subject
to masking. If FALSE, the value is a proper count that may be subject to
masking.
