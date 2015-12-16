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
  lapply(models, function(x){
    if(avrg == TRUE){
      lst_models <- lapply(models, function(m){
        lst_per_predictor <- lapply(m, function(r){
          data.frame(model_response = r$response,
                     testing_response = r$testing$RESPONSE,
                     testing_predicted = as.numeric(r$testing$PREDICTED),
                     smmry = summary(lm(r$testing$PREDICTED ~ 
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
        #alz: y kommt aus compVarImp.R
        #x <- models[[1]]
        #y <- x[[1]]
        R2 <- smmry$r.squared
        data.frame(R2 = R2)
      })
      mod <- do.call("rbind", mod)
    }
  })
}


ggplot(lst_models, aes(x = testing_response, y = testing_predicted, color = model_response)) +
  geom_point()

ggplot(lst_models[lst_models$model_response == "Acari",], aes(x = testing_response, y = testing_predicted, color = model_response)) +
  geom_point()

plot(testing_predicted ~ testing_response, lst_models[lst_models$model_response == "total_insct",])
abline(lm(testing_predicted ~ testing_response, lst_models[lst_models$model_response == "total_insct",]))
summary(lm(testing_predicted ~ testing_response, lst_models[lst_models$model_response == "total_insct",]))
