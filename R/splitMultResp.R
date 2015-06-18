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
splitMultResp <- function(csl, response, p = 0.75){
  fs <- lapply(response, function(x){
    idv <- lapply(csl, function(y){
      smpl <- caret::createDataPartition(y[, x], p = p, list = FALSE, times = 1)
      list(training = y[smpl, -response[-which(response %in% x)]], 
           test = y[-smpl, -response[-which(response %in% x)]])
    })
  })
  return(fs)
}
