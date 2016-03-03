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
    vi_species <- lapply(x, function(y){
      vi <- varImp(y$model$fit, scale = FALSE)
      if(scale == TRUE){
        vi <- vi / max(vi)
      }
      if(length(rownames(vi)) == 1){
        variables <- predictors(y$model$fit)
      } else {
        variables <- rownames(vi)
      }
      vi <- data.frame(RESPONSE = y$response,
                       VARIABLE = variables,
                       IMPORTANCE = vi$Overall)
    })
    n <- length(vi_species)
    vi_species <- do.call("rbind", vi_species)
    vi_count <- vi_species %>% count(VARIABLE)
    vi_mean <- vi_species %>% group_by(VARIABLE) %>% summarise(mean = mean(IMPORTANCE))
    vi <- merge(vi_count, vi_mean)
    vi$RESPONSE <- vi_species$RESPONSE[1]
    vi <- vi[order(vi$mean, decreasing = TRUE), ,drop = FALSE]
    return(vi)
  })
}
