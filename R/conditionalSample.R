#' Conditional sampling
#'
#' @description
#' Compute 
#' 
#' @param NONE
#'
#' @return NONE
#'
#' @export conditionalSample
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
conditionalSample <- function(df, selector, resample = 100, seedid){
  smpl <- lapply(seq(resample), function(x){
    act_smpl <- do.call("rbind", lapply(unique(selector), function(y){
      set.seed(y)
      act_sel <- sample(which(selector == y), size = 1)
      df[act_sel, ]
    }))
  })
  return(smpl)
}