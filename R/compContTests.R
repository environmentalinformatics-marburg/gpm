#' Compute contingency tests for all model responses
#'
#' @description
#' Compute individual or averaged contingency tests for all predicted variables
#' 
#' @param models The trained model for each response variable and all 
#' resamplings resulting from \code{\link{trainModel}}
#' @param mean Compute the mean contingengy test results over all response 
#' variables or return an indivual result for each one of them
#' 
#' @return A data frame containing the contingency test information.
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
#' \dontrun{
#' #Not run
#' }
#' 
compContTests <- function(models, mean = TRUE){
  cont_test <- lapply(models, function(x){
    act_cont_test <- lapply(x, function(y){
      if(inherits(y$model, "try-error")){
        NULL
      } else {
        cont_table <- ftable(y$testing$PREDICTED[,which(colnames(y$testing$PREDICTED) == "pred")], 
                             y$testing$RESPONSE)
        n <- length(y$testing$RESPONSE)
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
        
        expct_corr <- 1/n * ((hits + misses) * (hits + falarm) + (corneg + misses) * (corneg + falarm))
        hss <- (hits + corneg - expct_corr) / (n - expct_corr)
        
        or <- hits * corneg / (misses * falarm)
        
        orss = (hits * corneg - misses * falarm) / 
          (hits * corneg + misses * falarm)
        
        kappa <- helpCalcKappa(cont_table)
        
        cbind(data.frame(Response = y$response,
                         Accuracy = accuracy,
                         Bias = bias,
                         POD = pod,
                         FAR = far,
                         POFD = pofd,
                         SR = sr,
                         TS = ts,
                         ETS = ets,
                         HK = hk,
                         HSS = hss,
                         OR = or,
                         ORSS = orss), data.frame(t(kappa)))
      }
    })
    act_cont_test <- do.call("rbind", act_cont_test)
  })

  if(mean == TRUE){
    cont_test_mean <- lapply(cont_test, function(x){
      data.frame(Response = x$Response[1], 
                 Kappa_mean = mean(x$Kappa, na.rm = TRUE),
                 Kappa_location_mean = mean(x$Kappa_location, na.rm = TRUE),
                 Kappa_histogram_mean = mean(x$Kappa_histogram, na.rm = TRUE),
                 Kappa_change_agreement_mean = mean(x$Kappa_change_agreement, na.rm = TRUE),
                 Kappa_quantity_agreement_mean = mean(x$Kappa_quantity_agreement, na.rm = TRUE),
                 Kappa_quantity_disagreement_mean = mean(x$Kappa_quantity_disagreement, na.rm = TRUE),
                 Kappa_allocation_agreement_mean = mean(x$Kappa_allocation_agreement, na.rm = TRUE),
                 Kappa_allocation_disagreement_mean = mean(x$Kappa_allocation_disagreement, na.rm = TRUE),
                 Bias_mean = mean(x$Bias, na.rm = TRUE),
                 POD_mean = mean(x$POD, na.rm = TRUE),
                 FAR_mean = mean(x$FAR, na.rm = TRUE), 
                 POFD_mean = mean(x$POFD, na.rm = TRUE),
                 Accuracy_mean = mean(x$Accuracy, na.rm = TRUE),
                 SR_mean = mean(x$SR, na.rm = TRUE),
                 TS_mean = mean(x$TS, na.rm = TRUE),
                 ETS_mean = mean(x$ETS, na.rm = TRUE),
                 HK_mean = mean(x$HK, na.rm = TRUE),
                 HSS_mean = mean(x$HSS, na.rm = TRUE))
    })
    cont_test <- list(do.call("rbind", cont_test_mean), do.call("rbind", cont_test))
  }
  return(cont_test)
}