if ( !isGeneric("resamplingsByVariable") ) {
  setGeneric("resamplingsByVariable", function(x, ...)
    standardGeneric("resamplingsByVariable"))
}
#' Create identical samples or resample by specific selector variable
#'
#' @description 
#' This function creates n sets of identical resamples or resamplings where the
#' rows of a data frame are sampled based on a specific selector variable. The 
#' selector variable is defined in the meta information layer of the gpm object. 
#' Within each sample, only m samples for each unique value of the selector 
#' variable are selected.
#' 
#' @param x An object of class gpm or data.frame
#' @param resample The number n of resamplings computed
#' @param use_selector Use the selector variable for splitting the samples into
#' training or testing (default FALSE).
#' @param selector The column name of the selector variable. Only 
#' relevant if use_selector is TRUE.
#' @param grabs The number m of samples per unique selector value
#' 
#' @name resamplingsByVariable
#' 
#' @export resamplingsByVariable
#' 
#' @details The resamplings do not contain the actual data values but the row 
#' numbers indicating which rows should be selected from the supplied data layer
#' during later processing steps.
#' 
#' @references  \code{\link{splitMultResp}} for splitting the samples into 
#' training and testing subsets.
#' 
#' @seealso NONE
#' 
#' @examples
#' \dontrun{
#' #Not run
#' }
#' 
NULL


# Function using gpm object ----------------------------------------------------
#' 
#' @return A layer within the gpm object with the information on the n 
#' individual resamplings.
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
#' @return A list of length n with the individual resamplings.
#' 
#' @rdname resamplingsByVariable
#'
setMethod("resamplingsByVariable", 
          signature(x = "data.frame"), 
          function(x, resample = 100, use_selector = "FALSE", selector = NULL, 
                   grabs = 1){
            if(use_selector == TRUE){
              smpl <- lapply(seq(resample), function(i){
                nj = 0
                act_smpl <- do.call("rbind", lapply(unique(selector), function(j){
                  if(length(which(selector == j)) == 1){
                    act_sel <- which(selector == j)
                  } else {
                    nj <- nj + 1
                    set.seed(i+nj)
                    act_sel <- sample(c(which(selector == j)), size = grabs)
                  }
                  return(act_sel)
                }))
              })
            } else {
              smpl <- lapply(seq(resample), function(i){
                set.seed(i)
                act_sel <- sample(seq(nrow(x)), size = nrow(x))
              })
            }
            return(smpl)
          })