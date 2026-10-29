# Changelog

## DescriptiveTable (development version)

### Changed

- Removed project-specific example files (#35)

### Fixed

- The SD of numeric variables is no longer blank when the variable has
  missing values (#28).

## DescriptiveTable 0.3.1

### Added

- `asd_per_level` in
  [`compute_asd()`](https://umc-utrecht-rwe.github.io/DescriptiveTable/reference/compute_asd.md)
  and
  [`DescriptivesTable()`](https://umc-utrecht-rwe.github.io/DescriptiveTable/reference/DescriptivesTable.md)
  returns an ASD for each category of a categorical variable (#25).
