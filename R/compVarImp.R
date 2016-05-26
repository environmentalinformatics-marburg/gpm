#' Compute variable importance over all model responses and resamplings
#'
#' @description
#' Compute mean variable importance over all model responses and resamplings
#'
#' @param NONE
#'
#' @name compVarImp
#'
#' @export compVarImp
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
compVarImp <- function(models, scale = FALSE){
  lapply(models, function(x){
    vi_species1 <- lapply(x, function(y){
      # vi <- varImp(y$model$fit, scale = FALSE)   #war: var_Imp(y$model$fit, scale = FALSE)
      
      if("finalModel" %in% names(models[[1]][[1]]$model)){
        vi <- y$model$finalModel$importance
        if(scale == TRUE){
          vi <- vi / max(vi)
        }
        if(length(rownames(vi)) == 1){
          variables <- predictors(y$model$finalModel)
        } else {
          variables <- rownames(vi)
        }
      } else {
        vi <- y$model$fit$importance
        if(scale == TRUE){
          vi <- vi / max(vi)
        }
        if(length(rownames(vi)) == 1){
          variables <- predictors(y$model$fit)
        } else {
          variables <- rownames(vi)
        }
      }
      
#       vi <- data.frame(RESPONSE = y$response,
#                        VARIABLE = variables,
#                        IMPORTANCE = vi$Overall)
      vi <- data.frame(RESPONSE = y$response,
                       VARIABLE = variables,
                       IMPORTANCE = as.data.frame(vi)[,1])
    })
    
    n <- length(vi_species1)
    vi_species <- do.call("rbind", vi_species1)
    # max_imp <- max(vi_species$IMPORTANCE)
    # vi_species$IMPORTANCE <- vi_species$IMPORTANCE / max_imp
    vi_count <- vi_species %>% count(VARIABLE)
    vi_mean <- vi_species %>% group_by(VARIABLE) %>% summarise(mean = mean(IMPORTANCE))
    vi <- merge(vi_count, vi_mean)
    vi$RESPONSE <- vi_species$RESPONSE[1]
    vi <- vi[order(vi$mean, decreasing = TRUE), ,drop = FALSE]
    return(vi)
  })
}
