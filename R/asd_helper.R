#' Internal wrapper function to compute ASDs. Based on the stddiff package, this wrapper allows different
#' ASD computations based on a variable-type specified by an input argument
#'
#' @param indf input data frame, containing a group column (y) and variable column (x) and optional weights column
#' @param y index of the group column
#' @param x index of the variable column for which the ASD is to be computed
#' @param met method to compute the asd, can be NUM1/NUM2 for numerical, TF for binary, CAT for categorical
#' @param use_weights option to compute weighted ASDs. If FALSE weights are ignored. Otherwise supply the name of the column in the data frame in which weights are stored
#' @param var name of the variable; passed to function and used in weighted stddiff. Note: use should be deprecated, change var use to vcol
#' @param group_names levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#'
#' @return
#' @export
#'
#' @examples
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
      outdf <- tryCatch(
        stddiff.category(data = indf, gcol = y, vcol = x),
        error = function(e) check_error_stdiff_category(data = indf, gcol = y, vcol = x, group_names)
      )
    }else{
      outdf <- wtd.stddiff.category(data = indf, gcol = y, vcol = x,
                                    var = var, group = "group",
                                    use_weights = use_weights,
                                    group_names = group_names)
    }
  }
  return(outdf)
}
