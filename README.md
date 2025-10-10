# DescriptiveTable

**DescriptiveTable** is an internal R package. 
It comprises set of functions used to populate baseline characteristics tables 
(also known as "Table One" in Epi terms). 
The output is a table describing, for one or more groups, cell counts and/or descriptives 
such as means, medians quartiles, for a set of covariates. When more than one group is supplied,
an additional column can be added specifying the Absolute Standardized Difference between groups on each covariate. 

It also comprises two functions to perform basic data-masking (i.e. statistical disclosure control)

The inputs required are:
1) a dataset with rows equal to units from one or more group, and columns equal to covariates
2) a metadata file defining the output table; which variables should be included, if any nesting of counts should be done, what information should be displayed, what labels each variable should have

# How to use

Besides browsing the documentation for every function, a a vignette shows how to use the package,

## Installation

To use the package the best way is to pull the last version, then open the R project and run the following code in the console:

```{r}
devtools::install()
```

This should install the package locally, so you can then load normally:

```{r}
library(devtools)
```

In the future we should aim for a call like:

```{r
devtools::install_github("UMC-Utrecht-RWE/DescriptiveTable")
```

But for that an authentication provider should be set in place, as this is currently a private package.
