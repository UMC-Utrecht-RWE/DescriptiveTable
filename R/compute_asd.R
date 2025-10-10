#' Wrapper function to compute all ASDs for a set of variables
#'
#' @param CovariatesInformation data.table file containing all variables of interest, group variable
#' @param metadata Meta-data supplying variable name (var), type and expectedCat for categorical variables
#' @param referenceExtension # optional, extension to name of output column
#' @param use_weights if FALSE, computes unweighted ASDs; to compute weighted ASDs, supply column name for weights in CovariatesInformation
#' @param group_names levels of the group variable of interest for output display. Defaults to CONTROL and EXPOSED
#'
#' @return
#' @export
#'
#' @examples
compute_asd <- function(CovariatesInformation, # file of type M_StudyCohort_Covariates, binary categorical grouping variable named "group"
                        metadata, # 
                        referenceExtension = '', 
                        use_weights = FALSE,
                        group_names = c("CONTROL","EXPOSED")){
  
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
            if (metadata$type[i] == "NUM1" | metadata$type[i] == "NUM2"){
              smd <- rbind(smd,  c(var,metadata[i]$type, asd_helper(CovariatesInformation,indexGroup,indexCol,met = metadata$type[i],
                                                                    use_weights = use_weights, var = var,group_names)[1,7]))
            }else{
              smd <- rbind(smd, c(var,metadata[i]$type,asd_helper(CovariatesInformation,indexGroup,indexCol,met = metadata$type[i],
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