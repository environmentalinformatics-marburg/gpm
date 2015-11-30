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
      
      smmry <- summary(lm(y$testing$PREDICTED ~ y$testing$RESPONSE))
      #alz: y kommt aus compVarImp.R
      #x <- models[[1]]
      #y <- x[[1]]
      R2 <- smmry$r.squared
      
      
      data.frame(R2 = R2)
    })
    mod <- do.call("rbind", mod)
  })
}
