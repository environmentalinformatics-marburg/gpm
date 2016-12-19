#' Compute variable importance over all model responses and resamplings
#'
#' @description
#' Compute mean variable importance over all model responses and resamplings.
#'
#' @param models The trained model for each response variable and all 
#' resamplings resulting from \code{\link{trainModel}}
#' @param scale Scale variable importance over all resamplings or use individual
#' values (TRUE/FALSE)
#' 
#' @return A data frame containing the variable importance over for each
#' response variable and all resamplings.
#' 
#' @name compVarImp
#'
#' @export compVarImp
#'
#' @details The variable importance is extracted from the model training dataset
#' based on functions supplied by the caret package.
#'
#' @references  The function uses functions from:
#'  Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, 
#'  Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton Kenkel, 
#'  the R Core Team, Michael Benesty, Reynald Lescarbeau, Andrew Ziem, 
#'  Luca Scrucca, Yuan Tang and Can Candan. (2016). caret: Classification and 
#'  Regression Training. https://CRAN.R-project.org/package=caret
#'
#' @seealso NONE
#'
#' @examples
#' \dontrun{
#' #Not run
#' }
#'
compVarImp <- function(models, scale = FALSE){
  lapply(models, function(x){
    vi_species1 <- lapply(x, function(y){
      # vi <- varImp(y$model$fit, scale = FALSE)   #war: var_Imp(y$model$fit, scale = FALSE)
      if(inherits(y$model, "try-error")){
        NULL
      } else {
        if("finalModel" %in% names(y$model)){
          vi <- y$model$finalModel$importance
          if(scale == TRUE){
            vi <- vi / max(vi)
          }
          if(length(rownames(vi)) == 1){
            variables <- predictors(y$model$finalModel)
          } else {
            variables <- rownames(vi)
          }
        } else if("importance" %in% names(y$model)){
          vi <- y$model$fit$importance
          if(scale == TRUE){
            vi <- vi / max(vi)
          }
          if(length(rownames(vi)) == 1){
            variables <- predictors(y$model$fit)
          } else {
            variables <- rownames(vi)
          }
        } else {
          vi <- caret::varImp(y$model)
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
      }
    })
    
    # n <- length(vi_species1)
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
