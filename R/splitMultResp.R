if ( !isGeneric("splitMultResp") ) {
  setGeneric("splitMultResp", function(x, ...)
    standardGeneric("splitMultResp"))
}
#' Split dataset into testing and training samples for each response variable
#'
#' @description
#' Split a data set into testing and training samples separate for each response
#' variable while preserving the frequency distribution of the response variable.
#' 
#' @param NONE
#'
#' @return NONE
#'
#' @name splitMultResp
#'
#' @export splitMultResp
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
#' @rdname splitMultResp
#'
setMethod("splitMultResp", 
          signature(x = "GPM"), 
          function(x, grabs = 1, resample = 100){
            return("TODO")
          })


# Function using data frame ----------------------------------------------------
#' 
#' @return List with n resampled sets of row numbers from which the column 
#' entries should be taken.
#' 
#' @rdname splitMultResp
#'
setMethod("splitMultResp", 
          signature(x = "data.frame"),
          function(x, response, resamples, p = 0.75){
            fs <- lapply(response, function(i){
              idv <- lapply(resamples, function(j){
                smpl <- caret::createDataPartition(x[j, i], p = p, list = FALSE, times = 1)
                list(training = x[smpl, -response[-which(response %in% i)]], 
                     test = x[-smpl, -response[-which(response %in% i)]])
              })
            })
            return(fs)
          })
