#' Compute regression tests over all model responses
#'
#' @description
#' Compute regression tests over all model responses.
#' 
#' @param models The trained model for each response variable and all 
#' resamplings resulting from \code{\link{trainModel}}
#' @param per_model Compute the test statistics per model or averaged over all
#' models.
#' @param per_selector Compute the test statistics per selector (see 
#' \code{\link{resamplingsByVariable}})
#' @param sub_selectors Average over a given set of selectors (NULL otherwise)
#' @param details Return test details (TRUE/FALSE)
#' 
#' @return A data frame containing the test information.
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
#' \dontrun{
#' #Not run
#' }
#' 
compRegrTests <- function(models, per_model = TRUE, per_selector = FALSE,
                          sub_selectors = NULL, details = TRUE){
  #lapply(models, function(x){
  if(per_model == TRUE){
    lst_models <- lapply(models, function(m){
      lst_per_predictor <- lapply(m, function(r){
        if(is.na(r$testing[[1]])){
          lpp <- NULL
        } else {
          if(class(r$testing$PREDICTED) == "data.frame"){
            r$testing$PREDICTED <- r$testing$PREDICTED[,1]
          }
          lpp <- data.frame(model_response = r$response,
                            model_selector = r$testing$SELECTOR,
                            testing_response = r$testing$RESPONSE,
                            testing_predicted = as.numeric(r$testing$PREDICTED))
        }
        return(lpp)
      })
      
      if(is.null(unlist(lst_per_predictor))){
        lst <- NULL
      } else {
        if(per_selector == TRUE){
          if(is.null(sub_selectors)){
            sub_selectors <- c(1, 1000000L)
          }
          
          lst_per_predictor <- do.call("rbind", lst_per_predictor)
          selectors <- unique(substr(lst_per_predictor$model_selector, 
                                     sub_selectors[1], sub_selectors[2]))
          
          lst_per_selector <- lapply(selectors, function(s){
            pred <- lst_per_predictor$testing_predicted[
              substr(lst_per_predictor$model_selector, 
                     sub_selectors[1], sub_selectors[2]) == 
                substr(s, sub_selectors[1], sub_selectors[2])]
            resp <- lst_per_predictor$testing_response[
              substr(lst_per_predictor$model_selector, 
                     sub_selectors[1], sub_selectors[2]) == 
                substr(s, sub_selectors[1], sub_selectors[2])]
            
            smmry <- summary(lm(pred~resp))
            data.frame(model_response = 
                         lst_per_predictor$model_response[
                           substr(lst_per_predictor$model_selector,
                                  sub_selectors[1], sub_selectors[2]) == 
                             substr(s, sub_selectors[1], sub_selectors[2])],
                       model_selector = s,
                       testing_response = resp,
                       testing_predicted = pred,
                       pairs = length(pred),
                       r_squared = smmry$r.squared,
                       adj_r_squared = smmry$adj.r.squared,
                       residuals = smmry$residuals)
          })
          lst <- do.call("rbind", lst_per_selector)
        } else {
          lst <- lapply(seq(length(lst_per_predictor)), function(s){
            if(!is.null(lst_per_predictor[[s]])){
              smmry <- summary(lm(lst_per_predictor[[s]]$testing_predicted ~ 
                                    lst_per_predictor[[s]]$testing_response))
              lst_per_predictor[[s]]$r_squared <- smmry$r.squared
              lst_per_predictor[[s]]$adj_r_squared <- smmry$adj.r.squared
              lst_per_predictor[[s]]$residuals <- smmry$residuals
              lst_per_predictor[[s]]$sample <- s
            }
            return(lst_per_predictor[[s]])
          })
          lst <- (do.call("rbind", lst))
        }  
      }
      return(lst)
    })
    lst_models <- do.call("rbind", lst_models)
    
    if(details == FALSE){
      if(per_selector == TRUE){
        lst_models <- aggregate(lst_models$r_squared, 
                                by = list(lst_models$model_response,
                                          lst_models$model_selector),
                                FUN = mean)
        colnames(lst_models) <- c("model_response", "model_selector", "R2")
      } else {
        lst_models <- aggregate(lst_models$r_squared, 
                                by = list(lst_models$model_response),
                                FUN = mean)
        colnames(lst_models) <- c("model_response", "R2")
      }
    }
    
  } else {
    lst_models <- lapply(models, function(m){
      lst_subruns <- lapply(m, function(r){
        if(class(r$testing$PREDICTED) == "data.frame"){
          r$testing$PREDICTED <- r$testing$PREDICTED[,1]
        }
        smmry <- summary(lm(r$testing$PREDICTED ~ r$testing$RESPONSE))
        plot(r$testing$PREDICTED ~ r$testing$RESPONSE)
        R2 <- smmry$r.squared
        data.frame(R2 = R2)
      })
      return(do.call("rbind", lst_subruns))
    })
    lst_models <- do.call("rbind", lst_models)
  }
  
  return(lst_models)
}

