# DescriptiveTable
**DescriptiveTable** is a set of functions used to populate baseline characteristics tables (also known as "Table One" in Epi terms). 

The output is a table describing, for one or more groups, cell counts and/or descriptives such as means, medians quartiles, for a set of covariates. When more than one group is supplied, an additional column can be added specifying the Absolute Standardized Difference between groups on each covariate. 

The inputs required are:
1) a dataset with rows equal to units from one or more group, and columns equal to covariates
2) a metadata file defining the output table; which variables should be included, if any nesting of counts should be done, what information should be displayed, what labels each variable should have

Script includes example dataset, metadata, and input files. See `example_script.R`
