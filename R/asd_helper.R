#' Internal wrapper function to compute ASDs. Based on the stddiff package, this wrapper allows different
#' ASD computations based on a variable-type specified by an input argument
#'
#' @param indf input data frame, containing a group column (y) and variable column (x) and optional weights column
#' @param y index of the group column
#' @param x index of the variable column for which the ASD is to be computed
#' @param met method to compute the asd, can be NUM1/NUM2 for numerical, TF for binary, CAT for categorical
#' @param use_weights option to compute weighted ASDs. If FALSE weights are ignored. Otherwise supply the name of the column in the data frame in which weights are stored
#' @param group_names levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#'
#' @return
#' @export
#'
#' @examples
asd_helper <- function(indf,y,x,met="num", use_weights = FALSE,
                       group_names = c("CONTROL","EXPOSED")){
  
  if (met=="NUM1" | met=="NUM2") {
    outdf <- wtd.stddiff.numeric(data = indf, gcol = y, vcol = x,
                                 use_weights = use_weights,
                                 group_names = group_names)
  }
  if (met=="TF") {
    outdf <- wtd.stddiff.binary(data = indf, gcol = y, vcol = x,
                                use_weights = use_weights,
                                group_names = group_names)
  }
  if (met=="CAT") {
    outdf <- wtd.stddiff.category(data = indf, gcol = y, vcol = x,
                                  use_weights = use_weights,
                                  group_names = group_names)
  }
  return(outdf)
}
