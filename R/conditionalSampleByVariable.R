#' Conditional sampling by specific variable
#'
#' @description
#' This function creates n sets of conditional resamplings where the column 
#' entries of a data frame are sampled based on a specific selector variable.
#' Within each sample, only m column entries for each unique value of the
#' selector variable are selected.
#' 
#' @param NONE
#'
#' @return NONE
#'
#' @export conditionalSampleByVariable
#' 
#' @details NONE
#' 
#' @references  NONE
#' 
#' @seealso NONE
#' 
#' @examples
#' # Not run
#' 
conditionalSampleByVariable <- function(df, selector, grabs = 1, resample = 100){
  smpl <- lapply(seq(resample), function(x){
    act_smpl <- do.call("rbind", lapply(unique(selector), function(y){
      set.seed(y)
      act_sel <- sample(which(selector == y), size = grabs)
      df[act_sel, ]
    }))
  })
  return(smpl)
}