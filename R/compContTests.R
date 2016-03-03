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
compContTests <- function(models, mean = FALSE){
  cont_test <- lapply(models, function(x){
    act_cont_test <- lapply(x, function(y){
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
    act_cont_test <- do.call("rbind", act_cont_test)
  })
  
  if(mean == TRUE){
    cont_test <- lapply(cont_test, function(x){
      data.frame(RESPONSE = x$RESPONSE[1], 
                 KAPPA_MEAN = mean(x$Kappa, na.rm = TRUE),
                 KAPPA_LOCATION_MEAN = mean(x$Kappa.of.location, na.rm = TRUE),
                 KAPPA_HISTOGRAM_MEAN = mean(x$Kappa.of.histogram, na.rm = TRUE),
                 KAPPA_CHANCE_AGRM_MEAN = mean(x$Chance.agreement, na.rm = TRUE),
                 KAPPA_QUANTITY_AGRM_MEAN = mean(x$Quantity.agreement, na.rm = TRUE),
                 KAPPA_QUANTITY_DISAGRM_MEAN = mean(x$Quantity.disagreement, na.rm = TRUE),
                 KAPPA_ALLOCATION_AGRM_MEAN = mean(x$Allocation.agreement, na.rm = TRUE),
                 KAPPA_ALLOCATION_DISAGRM_MEAN = mean(x$Allocation.disagreement, na.rm = TRUE),
                 POD_MEAN = mean(x$POD, na.rm = TRUE),
                 FAR_MEAN = mean(x$FAR, na.rm = TRUE), 
                 POFD_MEAN = mean(x$POFD, na.rm = TRUE),
                 ACCURACY_MEAN = mean(x$ACCURACY, na.rm = TRUE),
                 SR_MEAN = mean(x$SR, na.rm = TRUE),
                 TS_MEAN = mean(x$TS, na.rm = TRUE),
                 ETS_MEAN = mean(x$ETS, na.rm = TRUE),
                 HK_MEAN = mean(x$HK, na.rm = TRUE))
    })
    cont_test <- do.call("rbind", cont_test)
  }
  return(cont_test)
}