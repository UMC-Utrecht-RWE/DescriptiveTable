# Weighted counterparts of the functions in count_functions.R.
# Returning the same shape as its unweighted (label column followed by V1, V2, V3)
#
# Using index `data` by column name instead of position

#' Weighted count function for binary (true/false) variables
#'
#' @param data input data.table containing the variable and the weights column
#' @param varName name of the variable column in data of interest
#' @param popN denominator; summed weights of the source population, numeric
#' @param expectedCat vector of expected categories; NA, unused in this function
#' @param weightName name of the weights column in data
#'
#' @return one row containing the summed weights of TRUE units and their
#'   percentage of popN
#' @export
#'
#' @examples
count_weighted_TF <- function(
  data,
  varName,
  popN,
  expectedCat = NA,
  weightName
) {
  x <- as.numeric(data[[varName]])
  w <- as.numeric(data[[weightName]])
  N <- sum(w[!is.na(x) & x == 1], na.rm = TRUE)
  PER <- (N / popN) * 100
  results <- as.data.table(list("TRUE", N, PER, NA))
  return(results)
}

#' Weighted count function for numeric variables which returns mean, SD and median, Q1, Q3 (type 1)
#'
#' @param data input data.table containing the variable and the weights column
#' @param varName name of the variable column in data of interest
#' @param popN denominator; summed weights of the source population, numeric
#' @param expectedCat vector of expected categories; NA, unused in this function
#' @param weightName name of the weights column in data
#'
#' @return two rows containing; i) weighted mean, weighted sd, and ii) weighted
#'   median, Q1, Q3
#' @export
#'
#' @examples
count_weighted_NUM1 <- function(
  data,
  varName,
  popN = NA,
  expectedCat = NA,
  weightName
) {
  x <- as.numeric(data[[varName]])
  w <- as.numeric(data[[weightName]])

  # If variable is all NA, Hmisc gives an error
  if (all(is.na(x))) {
    return(as.data.table(rbind(
      c("STATS1", NA, NA, NA),
      c("STATS2", NA, NA, NA)
    )))
  }

  info_mean <- Hmisc::wtd.mean(x, w, na.rm = TRUE)
  info_sd <- sqrt(Hmisc::wtd.var(x, w, na.rm = TRUE))
  quants <- Hmisc::wtd.quantile(x, w, probs = c(0.5, 0.25, 0.75), na.rm = TRUE)

  values <- as.data.table(rbind(
    c("STATS1", info_mean, info_sd, ""),
    c("STATS2", quants[[1]], quants[[2]], quants[[3]])
  ))
  return(values)
}

#' Weighted count function for numeric variables which returns median, Q1, Q3 and min, max (type 2)
#'
#' Minimum and maximum are returned unweighted, as a weighted extreme is not
#' defined. They are taken directly rather than as the 0 and 1 quantiles of
#' `wtd.quantile`.
#'
#' @param data input data.table containing the variable and the weights column
#' @param varName name of the variable column in data of interest
#' @param popN denominator; summed weights of the source population, numeric
#' @param expectedCat vector of expected categories; NA, unused in this function
#' @param weightName name of the weights column in data
#'
#' @return two rows containing; i) weighted median, Q1, Q3, ii) min, max
#' @export
#'
#' @examples
count_weighted_NUM2 <- function(
  data,
  varName,
  popN = NA,
  expectedCat = NA,
  weightName
) {
  x <- as.numeric(data[[varName]])
  w <- as.numeric(data[[weightName]])

  # If variable is all NA, Hmisc gives an error

  if (all(is.na(x))) {
    return(as.data.table(rbind(
      c("STATS1", NA, NA, NA),
      c("STATS2", NA, NA, NA)
    )))
  }

  quants <- Hmisc::wtd.quantile(x, w, probs = c(0.5, 0.25, 0.75), na.rm = TRUE)

  values <- as.data.table(rbind(
    c("STATS1", quants[[1]], quants[[2]], quants[[3]]),
    c("STATS2", min(x, na.rm = TRUE), max(x, na.rm = TRUE), NA)
  ))
  return(values)
}

#' Weighted count function for categorical variables
#'
#' @param data input data.table containing the variable and the weights column
#' @param varName name of the variable column in data of interest
#' @param popN denominator; summed weights of the source population, numeric
#' @param expectedCat vector of expected categories
#' @param weightName name of the weights column in data
#'
#' @return one row for every value of expectedCat
#' @export
#'
#' @examples
count_weighted_CAT <- function(data, varName, popN, expectedCat, weightName) {
  expectedCatDf <- as.data.table(as.character(expectedCat))
  expectedCatDf <- expectedCatDf[, orderCol := .I]
  empty_population <- popN == 0 | all(is.na(data[[varName]]))
  # if the parent population is empty, write NAs for counts
  if (empty_population) {
    counts <- as.data.table(cbind(
      V1 = expectedCat,
      N = rep(NA, length(expectedCat))
    ))
  } else {
    # otherwise sum the weights within each observed level
    wtd <- Hmisc::wtd.table(data[[varName]], as.numeric(data[[weightName]]))
    counts <- data.table(
      V1 = as.character(wtd$x),
      N = as.numeric(wtd$sum.of.weights)
    )
  }
  results <- merge(expectedCatDf, counts, all.x = TRUE, sort = TRUE)
  results[is.na(results)] <- 0
  setorder(results, "orderCol")
  results[, orderCol := NULL]
  if (empty_population) {
    results[, PER := NA]
  } else {
    results[, PER := 100 * (N / popN)]
  }
  results <- cbind(results, rep(NA, nrow(results)))
  return(results)
}
