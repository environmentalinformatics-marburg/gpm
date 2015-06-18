#' Split dataset into testing and training samples
#'
#' @description
#' Compute 
#' 
#' @param NONE
#'
#' @return NONE
#'
#' @export splitByFrequency
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
splitByFrequency <- function(csl, response, p = 0.75){
  cmpl <- lapply(response, function(x){
    idv <- lapply(csl, function(y){
      smpl <- caret::createDataPartition(y[, x], p = p, list = FALSE, times = 1)
      list(training = y[smpl, -response[-which(response %in% x)]], 
           test = y[-smpl, -response[-which(response %in% x)]])
    })
  })
  return(cmpl)
}