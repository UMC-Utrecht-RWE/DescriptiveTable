# A series of functions which compute counts and descriptive statistics for different variable types

#' Count function for binary (true/false) variables
#'
#' @param data input data.table
#' @param varName name of the variable column in data of interest
#' @param popN denominator; population N, numeric
#' @param expectedCat vector of expected categories; NA, unused in this function
#'
#' @return
#' @export
#'
#' @examples
count_TF <- function(data, varName ,popN, expectedCat = NA){
  data[, eval(varName) := as.numeric(get(varName))]
  N <- data[get(varName) == TRUE | get(varName) == 1, .N]
  PER <- (N/popN)*100
  results <- as.data.table(list('TRUE',N,PER,NA))
  return(results)
}

#' Count function for numeric variables which returns mean, SD and median, Q1, Q3 (type 1)
#'
#' @param data input data.table
#' @param varName name of the variable column in data of interest
#' @param popN denominator; population N, numeric
#' @param expectedCat vector of expected categories; NA, unused in this function
#'
#' @return two rows containing; i) mean, sd, and ii) median, Q1, Q3
#' @export
#'
#' @examples
count_NUM1 <- function(data, varName, popN = NA ,expectedCat = NA){
  data[, eval(varName) := as.numeric(get(varName))]
  info <- summary(data[[1]])

  info_mean = info[[4]]
  info_sd = sd(data[[1]])
  info_median = info[[3]]
  info_q1 = info[[2]]
  info_q3 = info[[5]]

  values <- as.data.table(rbind(c('STATS1',info_mean, info_sd,''),c('STATS2',info_median,info_q1,info_q3)))
  return(values)
}

#' Count function for numeric variables which returns median, Q1, Q3  and min, max (type 2)
#'
#' @param data input data.table
#' @param varName name of the variable column in data of interest
#' @param popN denominator; population N, numeric
#' @param expectedCat vector of expected categories; NA, unused in this function
#'
#' @return two rows containing; i) median, Q1, Q3, ii) min, max
#' @export
#'
#' @examples
count_NUM2 <- function(data, varName, popN = NA ,expectedCat = NA){
  data[, eval(varName) := as.numeric(get(varName))]

  info <- summary(data[[1]])

  info_median = info[[3]]
  info_q1 = info[[2]]
  info_q3 = info[[5]]
  info_min = info[[1]]
  info_max = info[[6]]
  values <- as.data.table(rbind(c('STATS1',info_median,info_q1,info_q3),
                                c('STATS2',info_min,info_max,NA)))
  return(values)
}


#' Count function for categorical variables 
#'
#' @param data input data.table
#' @param varName name of the variable column in data of interest
#' @param popN denominator; population N, numeric
#' @param expectedCat vector of expected categories
#'
#' @return one row for every value of expectedCat
#' @export
#'
#' @examples
count_CAT <- function(data, varName, popN, expectedCat){
  expectedCatDf <- as.data.table(as.character(expectedCat))
  expectedCatDf <- expectedCatDf[, orderCol := .I]
  # if the parent population is empty, write NAs for counts
  if(popN == 0 | all(is.na(data[[1]]))){
    counts <- as.data.table(cbind(V1 =expectedCat, N =rep(NA,length(expectedCat))))
  }else{
    # otherwise compute counts using table
    counts <- as.data.table(table(data[[1]]))
  }
  results <- merge(expectedCatDf, counts, all.x = TRUE, sort = TRUE)
  results[is.na(results)] <- 0
  setorder(results, 'orderCol')
  results[, orderCol := NULL ]
  if(popN == 0 | all(is.na(data[[1]]))){
    results[,PER := NA]
  }else{
    results[, PER := 100*(N/popN)]
  }
  results <- cbind(results,rep(NA,nrow(results)))
  return(results)
}

