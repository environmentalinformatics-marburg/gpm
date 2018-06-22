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
compRegrTests = function(models, per_model = TRUE, mids = 1){
  #lapply(models, function(x){
  if(per_model == TRUE){
    mids = seq(length(models))
  }
  lst_models = lapply(mids, function(mi){
    m = models[[mi]]
    lst_per_response = lapply(m, function(r){
      lst_per_sample = lapply(r, function(s){
        if(inherits(s$model, "try-error")){
          lpp = NULL
        } else if(!is.null(s$testing[[1]])){
          lpp = "Implement case in compRegTests."
        } else {
          model_response = s$response
          
          #rfe
          if(!is.null(s$model$fit)){
            lpp = list(data.frame(model_response = s$response,
                                  s$model$fit$results[s$model$fit$bestTune$ncomp, ]))
            lpp[[2]] = data.frame(model_response = s$response,
                                  s$model$fit$resample) 
          } else {
            lpp = list(data.frame(model_response = s$response,
                                  s$model$results[s$model$bestTune$ncomp, ]))
            lpp[[2]] = data.frame(model_response = s$response,
                                  s$model$resample[s$model$resample$ncomp==s$model$bestTune$ncomp,])    
          }
          rownames(lpp[[1]]) = NULL
          rownames(lpp[[2]]) = NULL
        }
        return(lpp)
      })
      return(unlist(lst_per_sample,recursive=FALSE))
    })
    return(list(do.call("rbind", sapply(lst_per_response, function(x) x[[1]])),
                do.call("rbind", sapply(lst_per_response, function(x) x[[2]]))))
  })
  names(lst_models) = names(models)
  return(lst_models)
}