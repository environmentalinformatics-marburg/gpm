if ( !isGeneric("resamplingsByVariable") ) {
  setGeneric("resamplingsByVariable", function(x, ...)
    standardGeneric("resamplingsByVariable"))
}
#' Resampling by specific variable
#'
#' @description
#' This function creates n sets of resamplings where the column 
#' entries of a data frame are sampled based on a specific selector variable.
#' Within each sample, only m column entries for each unique value of the
#' selector variable are selected.
#' 
#' @param NONE
#'
#' @return NONE
#'
#' @name resamplingsByVariable
#' 
#' @export resamplingsByVariable
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
NULL


# Function using gpm object ----------------------------------------------------
#' 
#' @return List with n resampled sets of row numbers from which the column 
#' entries should be taken.
#' 
#' @rdname resamplingsByVariable
#'
setMethod("resamplingsByVariable", 
          signature(x = "GPM"), 
          function(x, grabs = 1, resample = 100){
            return("TODO")
          })


# Function using data frame ----------------------------------------------------
#' 
#' @return List with n resampled sets of row numbers from which the column 
#' entries should be taken.
#' 
#' @rdname resamplingsByVariable
#'
setMethod("resamplingsByVariable", 
          signature(x = "data.frame"), 
          function(x, selector, grabs = 1, resample = 100){
            smpl <- lapply(seq(resample), function(i){
              act_smpl <- do.call("rbind", lapply(unique(selector), function(j){
                if(length(which(selector == j)) == 1){
                  act_sel <- which(selector == j)
                } else {
                  set.seed(i+j)
                  act_sel <- sample(c(which(selector == j)), size = grabs)
                }
                return(act_sel)
              }))
            })
            return(smpl)
          })