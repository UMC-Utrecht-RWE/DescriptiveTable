## functions which help compute absolute standardized differences (ASDs)
# sometimes referred to as a standardized mean difference (smd)

# this is a wrapper function for stddiff from the stddiff package which allows
# different ASD computations depending on a variable-type specified by meta-data
# this serves as a helper function for the lar
# the function takes as input
# indf - a data frame
# gcol - a column number for the binary grouping variable
# vcol - one or more column numbers of different types variables in the data
# met - a specification of the variable type (numeric type 1 or 2, TF, cat)
# weights = logical T/F
# if weights = FALSE the default functions from stddiff are used
# if weights = TRUE, the function uses a custom function wtd.stddiff.XXX
# this assumes that data contains a column name specified in use_weights that is used for weighting

# indf = CovariatesInformation
# y = indexGroup
# x = indexCol
# met = metadata[i]$type
# use_weights = use_weights
# var = metadata$var[1]

# indf = CovariatesInformation
# x = indexCol
# y = indexGroup
# met = metadata[i]$type
# use_weights = use_weights
# var = metadata$var[2]

asd_helper <- function(indf,y,x,met="num", use_weights = FALSE, var = NULL,
                       group_names = c("CONTROL","EXPOSED")){
  
  if (met=="NUM1" | met=="NUM2") {
    if(use_weights == FALSE){
      outdf <- stddiff.numeric(data = indf, gcol = y, vcol = x)
    }else{
      outdf <- wtd.stddiff.numeric(data = indf, gcol = y, vcol = x,
                                   var = var, group = "group",
                                   use_weights = use_weights,
                                   group_names = group_names)
    }
  }
  if (met=="TF") {
    if(use_weights == FALSE){
      outdf <- stddiff.binary(data = indf, gcol = y, vcol = x)
    }else{
      outdf <- wtd.stddiff.binary(data = indf, gcol = y, vcol = x,
                                  var = var, group = "group",
                                  use_weights = use_weights,
                                  group_names = group_names)
    }
  }
  if (met=="CAT") {
    if(use_weights == FALSE){
      outdf <- stddiff.category(data = indf, gcol = y, vcol = x)
    }else{
      outdf <- wtd.stddiff.category(data = indf, gcol = y, vcol = x,
                                    var = var, group = "group",
                                    use_weights = use_weights,
                                    group_names = group_names)
    }
  }
  return(outdf)
}

# this is the main function which computes ASDs for a set of variables
# using a file of type M_StudyCohort_Covariates and metadata
# and the asd_helper() function defined above
# CovariatesInformation = popdf
# metadata = table_metadata
# referenceExtension ="1"
# use_weights = use_weights
compute_asd <- function(CovariatesInformation, # file of type M_StudyCohort_Covariates, binary categorical grouping variable named "group"
                        metadata, # Meta-data supplying variable name (var), type and expectedCat for categorical variables
                        referenceExtension = '', # optional, extension to name of output column
                        use_weights = FALSE,
                        group_names = c("CONTROL","EXPOSED")){
  # lapply(names(CovariatesInformation),function(x)
  #   if(! ( is.integer(CovariatesInformation[,get(x)]) == TRUE | is.numeric(CovariatesInformation[,get(x)]) == TRUE) ){
  #     CovariatesInformation[,eval(x) := as.character(get(x))]
  #   }
  # )
  
  CovariatesInformation <- as.data.frame(CovariatesInformation)
  smd <- as.numeric()
  i <- 0
  for (var in metadata$var){
    i <- i +1
    if (!is.na(var) & !is.null(var)){
      if (var %in% names(CovariatesInformation)){
        indexGroup <- grep("group", colnames(CovariatesInformation))
        indexCol <-  grep(paste0('\\b',var,'\\b'), colnames(CovariatesInformation))
        # compute table of values to check type to use
        dim_tabvals <- dim(table(CovariatesInformation[,indexCol]))[1]
        if (dim_tabvals > 1){ # if only one value, SMD = 0
          if(dim_tabvals > 2){ # check if categorical/continuous variable has more than binary levels
            if (metadata[i]$type == "NUM1" | metadata[i]$type == "NUM2"){
              smd <- rbind(smd,  c(var,metadata[i]$type, asd_helper(CovariatesInformation,indexGroup,indexCol,met = metadata[i]$type,
                                                                    use_weights = use_weights, var = var,group_names)[1,7]))
            }else{
              smd <- rbind(smd, c(var,metadata[i]$type,asd_helper(CovariatesInformation,indexGroup,indexCol,met = metadata[i]$type,
                                                                  use_weights = use_weights, var = var,group_names)[1,5]))
            }
          }else{ # if only two levels, then calculate as binary
            smd <- rbind(smd, c(var,metadata[i]$type,asd_helper(CovariatesInformation,indexGroup,indexCol,met = "TF",
                                                                use_weights = use_weights, var = var,group_names)[1,5]))
          }
          
        }else{# if no differing levels calculate as 0
          smd <- rbind(smd, c(var,metadata[i]$type,
                              0))
        }
      }else{
        print(paste0(var,' not found either in M_studyCohort_Covariates'))
        smd <- rbind(smd,c(NA,NA,
                           NA))
      }
    }else{
      smd <- rbind(smd,c(NA,NA,
                         NA))
    }
  }
  #result <- cbind(metadata$var,as.data.table(smd))
  smd <- as.data.table(smd)
  setnames(smd,names(smd),c('var','type',paste0('asd_',referenceExtension)))
  return(smd)
}

# ------------------------------------------------------------------------------
# ----------------------------- Weighted ASD functions -------------------------
# ------------------------------------------------------------------------------

# here we define custom weighted versions of the stddiff functions
# these are made to be very similar to the stdiff functions except they rely on
# Hmisc:: implementations of the weighted mean, weighted variance/sd and weighted cross-tables

# functionality here is not particularly flexible, in that we assume:
# 1. the cohort dataset has groups EXPOSED and CONTROL
# 2. the weights are stored in a column called iptw
# Hmisc::wtd.var(temp[temp$group == "CONTROL",var], weights = temp[temp$group == "CONTROL", use_weights])
# numeric weighted difference version
wtd.stddiff.numeric <- function (data, gcol, vcol, var,
                                 group = "group", use_weights,
                                 group_names = c("CONTROL","EXPOSED")) {
  data[, gcol] <- as.factor(data[, gcol])
  rst <- matrix(rep(0, 9 * length(vcol)), ncol = 9)
  dimnames(rst) <- list(names(data)[vcol], c("mean.c", "sd.c",
                                             "mean.t", "sd.t", "missing.c", "missing.t", "stddiff",
                                             "stddiff.l", "stddiff.u"))
  for (i in 1:length(vcol)) {
    data[, vcol[i]] <- as.numeric(data[, vcol[i]])
    na.c <- length(which(is.na(data[, vcol[i]][which(data[,
                                                          gcol] == levels(data[, gcol])[1])])))
    na.t <- length(which(is.na(data[, vcol[i]][which(data[,
                                                          gcol] == levels(data[, gcol])[2])])))
    wcol <- which(colnames(data) == use_weights)[1]
    
    # omit missing values if relevant
    temp <- na.omit(data[, c(gcol, vcol[i], wcol)])
    
    # use weighted mean and weighted sd
    m <- sapply(group_names, function(s) Hmisc::wtd.mean(temp[temp$group==s,var],
                                                         weights = temp[temp$group==s,use_weights]))
    s <- suppressWarnings(sapply(group_names, function(s) sqrt(Hmisc::wtd.var(temp[temp$group==s,var],
                                                                              weights = temp[temp$group==s,use_weights])))
    )
    # edge case can occur where weighted sd is negative
    # if this happens, try an alternative weighting method
    if(any(is.nan(s))){
      log_print(paste0("For variable ",var," negative weighted variance, switching method to ML"))
      s <- sapply(group_names, function(s) sqrt(Hmisc::wtd.var(temp[temp$group==s,var],
                                                               weights = temp[temp$group==s,use_weights],
                                                               method = "ML")))
    }
    
    stddiff <- abs(m[2] - m[1])/sqrt((s[2]^2 + s[1]^2)/2)
    n <- table(temp[, 1])
    se <- sqrt((nrow(temp)/n[1]/n[2]) + stddiff^2/(2 *
                                                     nrow(temp)))
    stddiff.l <- stddiff - 1.96 * se
    stddiff.u <- stddiff + 1.96 * se
    rst[i, ] <- c(m[1], s[1], m[2], s[2], na.c,
                  na.t, stddiff, stddiff.l, stddiff.u)
  }
  return(rst)
}

# binary weighted difference

# == for binary variables
wtd.stddiff.binary <- function (data, gcol, vcol, var,
                                group = "group", use_weights,
                                group_names = c("CONTROL","EXPOSED")) {
  for (i in 1:length(c(gcol, vcol))) {
    data[, c(gcol, vcol)[i]] <- as.factor(data[, c(gcol, vcol)[i]])
  }
  rst <- matrix(rep(0, 7 * length(vcol)), ncol = 7)
  dimnames(rst) <- list(names(data)[vcol], c("p.c", "p.t",
                                             "missing.c", "missing.t", "stddiff", "stddiff.l", "stddiff.u"))
  for (i in 1:length(vcol)) {
    na.c <- length(which(is.na(data[, vcol[i]][which(data[, gcol] == levels(data[, gcol])[1])])))
    na.t <- length(which(is.na(data[, vcol[i]][which(data[, gcol] == levels(data[, gcol])[2])])))
    
    wcol <- which(colnames(data) == use_weights)[1]
    temp <- na.omit(data[, c(gcol, vcol[i], wcol)])
    temp[, 2] <- as.numeric(temp[, 2])
    temp[, var] <- temp[, var] - 1
    # use weighted mean
    p <- sapply(c("CONTROL","EXPOSED"), function(s) Hmisc::wtd.mean(temp[temp$group==s,var],
                                                                    weights = temp[temp$group==s,use_weights]))
    
    stddiff <- abs(p[2] - p[1])/sqrt((p[2] * (1 - p[2]) + p[1] * (1 - p[1]))/2)
    n <- table(temp[, 1])
    se <- sqrt(nrow(temp)/(n[1] * n[2]) + stddiff^2/(2 * nrow(temp)))
    stddiff.l <- stddiff - 1.96 * se
    stddiff.u <- stddiff + 1.96 * se
    rst[i, ] <- c(p[1], p[2], na.c, na.t, stddiff, stddiff.l, stddiff.u)
  }
  if(any(rst %in% c(Inf, -Inf))) rst[,] <- -88
  
  return(rst)
}

# weighted categorical variable difference
# relies on weighted cross tables
# data = indf; gcol = y; vcol = 89
# var
wtd.stddiff.category <- function (data, gcol, vcol, var,
                                  group = "group", use_weights,
                                  group_names = c("CONTROL","EXPOSED")) {
  for (i in 1:length(c(gcol, vcol))) {
    data[, c(gcol, vcol)[i]] <- as.factor(data[, c(gcol, vcol)[i]])
  }
  nr <- NA
  for (i in 1:length(vcol)) {
    nr[i] <- length(levels(data[, vcol[i]]))
  }
  rst <- matrix(rep(0, 7 * sum(nr)), ncol = 7)
  rname <- NA
  for (i in 1:length(nr)) {
    rname <- c(rname, paste(names(data)[vcol[i]], levels(data[, vcol[i]])))
  }
  dimnames(rst) <- list(rname[-1], c("p.c", "p.t", "missing.c",
                                     "missing.t", "stddiff", "stddiff.l", "stddiff.u"))
  
  for (i in 1:length(vcol)) {
    na.c <- length(which(is.na(data[, vcol[i]][which(data[, gcol] == levels(data[, gcol])[1])])))
    na.t <- length(which(is.na(data[, vcol[i]][which(data[, gcol] == levels(data[, gcol])[2])])))
    wcol <- which(colnames(data) == use_weights)[1]
    temp <- na.omit(data[, c(gcol, vcol[i], wcol)])
    
    temp_exp <- temp[temp[, group] == group_names[2], ]
    temp_con <- temp[temp[, group] == group_names[1], ]
    
    tbl_exp <- as.data.frame(Hmisc::wtd.table(temp_exp[, var], w = temp_exp[, use_weights]))
    tbl_con <- as.data.frame(Hmisc::wtd.table(temp_con[, var], w = temp_con[, use_weights]))
    colnames(tbl_exp)[2] <- group_names[2]
    colnames(tbl_con)[2] <- group_names[1]
    
    tbl <- merge(tbl_exp, by.x = "x", tbl_con, all.x = TRUE, all.y = TRUE, sort = TRUE)
    tbl[is.na(tbl)] <- 0
    rownames(tbl) <- tbl[,1]
    tbl[,1] <- NULL
    
    #table(temp[, 2], temp[, 1])
    prop <- prop.table(data.matrix(tbl), 2)
    t <- prop[-1, 2]
    c <- prop[-1, 1]
    k <- nr[i] - 1
    l <- k
    s <- matrix(rep(0, k * l), ncol = l)
    for (ii in 1:k) {
      for (j in 1:l) {
        if (ii == j) {
          s[ii, j] <- 0.5 * (t[ii] * (1 - t[ii]) + c[ii] *
                               (1 - c[ii]))
        }
        if (ii != j) {
          s[ii, j] <- -0.5 * (t[ii] * t[j] + c[ii] *
                                c[j])
        }
      }
    }
    e <- rep(1, k)
    e <- diag(e)
    s <- tryCatch(solve(s, e), error = function(err) -88)
    
    if(-88 %in% s){
      rst[,] <- -88
    } else {
      
      tc1 <- t - c
      tc2 <- t - c
      stddiff <- sqrt(t(tc1) %*% s %*% tc2)
      n <- table(temp[, 1])
      se <- sqrt(nrow(temp)/(n[1] * n[2]) + stddiff^2/(2 * nrow(temp)))
      stddiff.l <- stddiff - 1.96 * se
      stddiff.u <- stddiff + 1.96 * se
      if (i == 1) {
        rst[1:nr[i], ] <- cbind(prop, c(na.c, rep(NA, k)),
                                c(na.t, rep(NA, k)), c(stddiff, rep(NA, k)),
                                c(stddiff.l, rep(NA, k)), c(stddiff.u, rep(NA,
                                                                           k)))
      }
      if (i > 1) {
        rst[(sum(nr[1:(i - 1)]) + 1):(sum(nr[1:(i - 1)]) + nr[i]), ] <- cbind(prop, c(na.c, rep(NA, k)),
                                                                              c(na.t, rep(NA, k)), c(stddiff, rep(NA, k)),
                                                                              c(stddiff.l, rep(NA, k)), c(stddiff.u, rep(NA, k)))
      }
    }
  }
  if(any(rst %in% c(Inf, -Inf))) rst[,] <- -88
  
  return(rst)
}
