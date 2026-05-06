# file containing weighted standardized difference functions
#' Weighted ASD for categorical variables
#'
#' @param data input data frame, containing a group column (gcol),variable column (vcol) and weights column (use_weights)
#' @param gcol index of the group column
#' @param vcol  index of the variable column for which the ASD is to be computed
#' @param var name of the variable. Note: should be replaced by vcol internally to remove redundancy
#' @param group levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#' @param use_weights name of the column in data containing the weights
#' @param group_names levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#'
#' @details
#' This function is based on the unstandardized differences in the `stddiff` package, but uses the weighted table from `Hmisc::`
#'
#' @return
#' @import Hmisc
#' @export
#'
#' @examples
wtd.stddiff.category <- function(data, gcol, vcol, var,
                                 group = "group", use_weights,
                                 group_names = c("CONTROL", "EXPOSED")) {
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
  dimnames(rst) <- list(rname[-1], c(
    "p.c", "p.t", "missing.c",
    "missing.t", "stddiff", "stddiff.l", "stddiff.u"
  ))

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
    rownames(tbl) <- tbl[, 1]
    tbl[, 1] <- NULL

    # table(temp[, 2], temp[, 1])
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

    if (-88 %in% s) {
      rst[, ] <- -88
    } else {
      tc1 <- t - c
      tc2 <- t - c
      stddiff <- sqrt(t(tc1) %*% s %*% tc2)
      n <- table(temp[, 1])
      se <- sqrt(nrow(temp) / (n[1] * n[2]) + stddiff^2 / (2 * nrow(temp)))
      stddiff.l <- stddiff - 1.96 * se
      stddiff.u <- stddiff + 1.96 * se
      if (i == 1) {
        rst[1:nr[i], ] <- cbind(
          prop, c(na.c, rep(NA, k)),
          c(na.t, rep(NA, k)), c(stddiff, rep(NA, k)),
          c(stddiff.l, rep(NA, k)), c(stddiff.u, rep(
            NA,
            k
          ))
        )
      }
      if (i > 1) {
        rst[(sum(nr[1:(i - 1)]) + 1):(sum(nr[1:(i - 1)]) + nr[i]), ] <- cbind(
          prop, c(na.c, rep(NA, k)),
          c(na.t, rep(NA, k)), c(stddiff, rep(NA, k)),
          c(stddiff.l, rep(NA, k)), c(stddiff.u, rep(NA, k))
        )
      }
    }
  }
  if (any(rst %in% c(Inf, -Inf))) rst[, ] <- -88

  return(rst)
}

#' Weighted ASD for binary variables
#'
#' @param data input data frame, containing a group column (gcol),variable column (vcol) and weights column (use_weights)
#' @param gcol index of the group column
#' @param vcol  index of the variable column for which the ASD is to be computed
#' @param var name of the variable. Note: should be replaced by vcol internally to remove redundancy
#' @param group levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#' @param use_weights name of the column in data containing the weights
#' @param group_names levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#'
#' @details
#' This function is based on the unstandardized differences in the `stddiff` package, but uses the weighted mean from `Hmisc::`
#'
#' @return
#' @export
#'
#' @examples
wtd.stddiff.binary <- function(data, gcol, vcol, var,
                               group = "group", use_weights,
                               group_names = c("CONTROL", "EXPOSED")) {
  for (i in 1:length(c(gcol, vcol))) {
    data[, c(gcol, vcol)[i]] <- as.factor(data[, c(gcol, vcol)[i]])
  }
  rst <- matrix(rep(0, 7 * length(vcol)), ncol = 7)
  dimnames(rst) <- list(names(data)[vcol], c(
    "p.c", "p.t",
    "missing.c", "missing.t", "stddiff", "stddiff.l", "stddiff.u"
  ))
  for (i in 1:length(vcol)) {
    na.c <- length(which(is.na(data[, vcol[i]][which(data[, gcol] == levels(data[, gcol])[1])])))
    na.t <- length(which(is.na(data[, vcol[i]][which(data[, gcol] == levels(data[, gcol])[2])])))

    wcol <- which(colnames(data) == use_weights)[1]
    temp <- na.omit(data[, c(gcol, vcol[i], wcol)])
    temp[, 2] <- as.numeric(temp[, 2])
    temp[, var] <- temp[, var] - 1
    # use weighted mean
    p <- sapply(c("CONTROL", "EXPOSED"), function(s) {
      Hmisc::wtd.mean(temp[temp$group == s, var],
        weights = temp[temp$group == s, use_weights]
      )
    })

    stddiff <- base::abs(p[2] - p[1]) / sqrt((p[2] * (1 - p[2]) + p[1] * (1 - p[1])) / 2)
    n <- table(temp[, 1])
    se <- sqrt(nrow(temp) / (n[1] * n[2]) + stddiff^2 / (2 * nrow(temp)))
    stddiff.l <- stddiff - 1.96 * se
    stddiff.u <- stddiff + 1.96 * se
    rst[i, ] <- c(p[1], p[2], na.c, na.t, stddiff, stddiff.l, stddiff.u)
  }
  if (any(rst %in% c(Inf, -Inf))) rst[, ] <- -88

  return(rst)
}

#' Weighted ASD for numeric type variables
#'
#' @param data input data frame, containing a group column (gcol),variable column (vcol) and weights column (use_weights)
#' @param gcol index of the group column
#' @param vcol  index of the variable column for which the ASD is to be computed
#' @param var name of the variable. Note: should be replaced by vcol internally to remove redundancy
#' @param group levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#' @param use_weights name of the column in data containing the weights
#' @param group_names levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#'
#' @details
#' This function is based on the unstandardized differences in the `stddiff` package, but uses the weighted variance from `Hmisc::`
#'
#' @return
#' @export
#'
#' @examples
wtd.stddiff.numeric <- function(data, gcol, vcol, var,
                                group = "group", use_weights,
                                group_names = c("CONTROL", "EXPOSED")) {
  data[, gcol] <- as.factor(data[, gcol])
  rst <- matrix(rep(0, 9 * length(vcol)), ncol = 9)
  dimnames(rst) <- list(names(data)[vcol], c(
    "mean.c", "sd.c",
    "mean.t", "sd.t", "missing.c", "missing.t", "stddiff",
    "stddiff.l", "stddiff.u"
  ))
  for (i in 1:length(vcol)) {
    data[, vcol[i]] <- as.numeric(data[, vcol[i]])
    na.c <- length(which(is.na(data[, vcol[i]][which(data[
      ,
      gcol
    ] == levels(data[, gcol])[1])])))
    na.t <- length(which(is.na(data[, vcol[i]][which(data[
      ,
      gcol
    ] == levels(data[, gcol])[2])])))
    wcol <- which(colnames(data) == use_weights)[1]

    # omit missing values if relevant
    temp <- na.omit(data[, c(gcol, vcol[i], wcol)])

    # use weighted mean and weighted sd
    m <- sapply(group_names, function(s) {
      Hmisc::wtd.mean(temp[temp$group == s, var],
        weights = temp[temp$group == s, use_weights]
      )
    })
    s <- suppressWarnings(sapply(group_names, function(s) {
      sqrt(Hmisc::wtd.var(temp[temp$group == s, var],
        weights = temp[temp$group == s, use_weights]
      ))
    }))
    # edge case can occur where weighted sd is negative
    # if this happens, try an alternative weighting method
    if (any(is.nan(s))) {
      log_print(paste0("For variable ", var, " negative weighted variance, switching method to ML"))
      s <- sapply(group_names, function(s) {
        sqrt(Hmisc::wtd.var(temp[temp$group == s, var],
          weights = temp[temp$group == s, use_weights],
          method = "ML"
        ))
      })
    }

    stddiff <- base::abs(m[2] - m[1]) / sqrt((s[2]^2 + s[1]^2) / 2)
    n <- table(temp[, 1])
    se <- sqrt((nrow(temp) / n[1] / n[2]) + stddiff^2 / (2 *
      nrow(temp)))
    stddiff.l <- stddiff - 1.96 * se
    stddiff.u <- stddiff + 1.96 * se
    rst[i, ] <- c(
      m[1], s[1], m[2], s[2], na.c,
      na.t, stddiff, stddiff.l, stddiff.u
    )
  }
  return(rst)
}
