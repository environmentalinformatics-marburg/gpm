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
compRegrTests <- function(models, avrg = TRUE){
  #lapply(models, function(x){
    if(avrg == TRUE){
      lst_models <- lapply(models, function(m){
        lst_per_predictor <- lapply(m, function(r){
          data.frame(model_response = r$response,
                     testing_response = r$testing$RESPONSE,
                     testing_predicted = as.numeric(r$testing$PREDICTED),
                     #r2: for each resample with about 4 values a r2 is 
                     #calculated - those values are averaged
                     #gives a far better r2 because it is easier to put a trend 
                     #line in just a few points. => not the real r2 we are 
                     #looking for
                     r2_each_resample = summary(lm(r$testing$PREDICTED ~ 
                                       r$testing$RESPONSE))$r.squared
                     
                     )
        })
        lst_per_predictor <- do.call("rbind", lst_per_predictor)
        smmry <- summary(lm(lst_per_predictor$testing_predicted ~ 
                              lst_per_predictor$testing_response))
        lst_per_predictor$r_squared <- smmry$r.squared
        lst_per_predictor$adj_r_squared <- smmry$adj.r.squared
        lst_per_predictor$residuals <- smmry$residuals
        return(lst_per_predictor)
      })
      lst_models <- do.call("rbind", lst_models)
      
    } else {
      mod <- lapply(x, function(y){
        # R2, RMSE
        smmry <- summary(lm(y$testing$PREDICTED ~ y$testing$RESPONSE))
        plot(y$testing$PREDICTED ~ y$testing$RESPONSE)
        R2 <- smmry$r.squared
        data.frame(R2 = R2)
      })
      mod <- do.call("rbind", mod)
    }
    #lst_models <- do.call("rbind", lst_models)
  #})
}


