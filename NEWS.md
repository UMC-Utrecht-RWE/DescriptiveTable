# DescriptiveTable (development version)

## Changed

- Removed project-specific example files (#35)

## Fixed

- DescriptiveTable does not fail with control_types = TRUE when the input data has date or date-time columns (#17)
- The SD of numeric variables is no longer blank when the variable has missing values (#28).

# DescriptiveTable 0.3.1

## Added

- `asd_per_level` in `compute_asd()` and `DescriptivesTable()` returns an ASD for each category of a categorical variable (#25).
