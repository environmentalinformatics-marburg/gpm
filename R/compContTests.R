#' Compute contingency tests over all model responses
#'
#' @description
#' Compute contingency tests over all model responses
#' 
#' @param NONE
#'
#' @name compContTests
#' 
#' @export compContTests
#' 
#' @details NONE
#' 
#' @references
#' Contingency tests are taken from CAWCR (http://goo.gl/BEPWOI)
#' 
#' @seealso NONE
#' 
#' @examples
#' # Not run
#' 
compContTests <- function(models){
  lapply(models, function(x){
    mod <- lapply(x, function(y){
      cont_table <- ftable(y$testing$PREDICTED[,1], 
                           y$testing$RESPONSE)
      
      corneg <- cont_table[1,1]
      misses <- cont_table[1,2]
      falarm <- cont_table[2,1]
      hits <- cont_table[2,2]
      total <- sum(cont_table)
      
      accuracy <- (hits + corneg) / total
      
      bias <- (hits + falarm) / (hits + misses)
      
      pod <- hits / (hits + misses)
      
      far <- falarm / (hits + falarm)
      
      pofd <- falarm / (corneg + falarm)
      
      sr <- hits / (hits + falarm)
      
      ts <-  hits / (hits + misses + falarm)
      
      hits_r <- (hits + misses) * (hits + falarm) / total
      ets <- (hits - hits_r) / (hits + misses + falarm + hits_r)
      
      hk <- hits / (hits + misses) - falarm / (falarm + corneg)
      
      or <- hits * corneg / (misses * falarm)
      
      orss = (hits * corneg - misses * falarm) / 
        (hits * corneg + misses * falarm)
      
      kappa <- helpCalcKappa(cont_table)
      
      cbind(data.frame(RESPONSE = y$response,
                       ACCURACY = accuracy,
                       BIAS = bias,
                       POD = pod,
                       FAR = far,
                       POFD = pofd,
                       SR = sr,
                       TS = ts,
                       ETS = ets,
                       HK = hk,
                       OR = or,
                       ORSS = orss), data.frame(t(kappa)))
    })
    mod <- do.call("rbind", mod)
  })
}