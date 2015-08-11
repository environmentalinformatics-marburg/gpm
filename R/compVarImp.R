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
compVarImp <- function(models){
  lapply(models, function(x){
    vi_species <- lapply(x, function(y){
      vi <- varImp(y$model,scale=FALSE)
      vi <- vi[order(vi$Overall, decreasing = TRUE), ,drop = FALSE]
      vi <- data.frame(RESPONSE = y$response,
                       VARIABLE = row.names(vi),
                       IMPORTANCE = vi$Overall / max(vi$Overall) * 100)
    })
    n <- length(vi_species)
    vi_species <- do.call("rbind", vi_species)
    vi_count <- vi_species %>% count(VARIABLE)
    vi_mean <- vi_species %>% group_by(VARIABLE) %>% summarise(mean = mean(IMPORTANCE))
    vi <- merge(vi_count, vi_mean)
    vi$RESPONSE <- vi_species$RESPONSE[1]
    return(vi)
  })
}


