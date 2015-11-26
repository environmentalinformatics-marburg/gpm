#' Compute regression tests over all model responses
#'
#' @description
#' Compute regression tests over all model responses
#' 
#' @param NONE
#'
#' @name compRegrTests
#' 
#' @export compRegrTests
#' 
#' @details NONE
#' 
#' @references
#' NONE
#' 
#' @seealso NONE
#' 
#' @examples
#' # Not run
#' 
compRegrTests <- function(models){
  lapply(models, function(x){
    mod <- lapply(x, function(y){
      
      # R2, RMSE
      
      summary(lm(y$testing$PREDICTED ~ y$testing$RESPONSE))
      
      data.frame(R2 = ...,
                 RMSE = ...)
    })
    mod <- do.call("rbind", mod)
  })
}