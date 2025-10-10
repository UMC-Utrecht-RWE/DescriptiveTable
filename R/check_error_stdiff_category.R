# helper function to check errors of stddiff category
# check if categorical variable with exact equals
#' Internal function used to check if computation of categorical ASDs is possible
#'
#' @param data input data frame, containing a group column (gcol),variable column (vcol)
#' @param gcol index of the group column
#' @param vcol index of the variable column for which the ASD is to be computed
#' @param group_names levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#'
#' @return
#' @export
#'
#' @examples
check_error_stdiff_category <- function(data, gcol, vcol, group_names){
  # get table of counts across groups
  tab1 <- table(data[, vcol], data[, gcol])
  # recode to percentages
  for (j in 1:length(group_names)) {
    tab1[, j] <- round(tab1[, j] / sum(data[, gcol] == group_names[j]))
  }
  # add rest of the columns usually found in stddiff.category output
  colnames(tab1) <- paste0("p.",group_names)
  filler <- matrix(data = NA,
                   nrow = nrow(tab1),
                   ncol  = 5)
  colnames(filler) <- c("missing.c",
                        "missing.t",
                        "stddiff",
                        "stddiff.l",
                        "stddiff.u")
  # if categories are all equal, return 0
  if(ncol(tab1) > 1){
    if (all(tab1[, 1] == tab1[, 2])) {
      filler[, "stddiff"] <- 0
    }
  } else{
    # otherwise return NA flag
    filler[, "stddiff"] <- -88
  }
  rownames(tab1) <- paste(names(data)[vcol], rownames(tab1))
  outdf <- cbind(tab1, filler)
  return(outdf)
}