# A series of functions which compute counts for different variable types
# these functions are called when creating baseline descriptive tables
# different functions are called depending on the variable type assigned in a meta-data file

count_TF <- function(data, varName ,popN, expectedCat = NA){
  data[, eval(varName) := as.numeric(get(varName))]
  N <- data[get(varName) == TRUE | get(varName) == 1, .N]
  PER <- (N/popN)*100
  results <- as.data.table(list('TRUE',N,PER,NA))
  return(results)
}

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

count_QUART <- function(data,varName, popN, expectedCat){
  percentile_99 <- quantile(data[,get(varName)],probs = 0.99)
  quart <- quantile(data[get(varName) <= percentile_99,get(varName)])
  
  if(quart[2] == 0){
    quart[1] <- -1
  }else{
    quart[1] <- min(data[,get(varName)])
  }
  quart[5] <- max(data[,get(varName)])
  
  values <- as.data.table(table(cut(data[,get(varName)], breaks = quart, include.lowest	= FALSE, ordered_result = TRUE)))
  values[, per := 100*(N/popN)][, empty := NA][,V1 := NULL]
  values <- cbind(expectedCat,values)
  return(values)
}
