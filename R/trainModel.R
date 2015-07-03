if ( !isGeneric("trainModel") ) {
  setGeneric("trainModel", function(x, ...)
    standardGeneric("trainModel"))
}
#' Train machine learning model
#'
#' @description
#' Train machine learning model.
#' 
#' @param NONE
#'
#' @return NONE
#'
#' @name trainModel
#' 
#' @export trainModel
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
NULL


# Function using gpm object ----------------------------------------------------
#' 
#' @return Trained model for each sample.
#' 
#' @rdname trainModel
#'
setMethod("trainModel", 
          signature(x = "GPM"), 
          function(x, grabs = 1, resample = 100){
            return("TODO")
          })


# Function using data frame ----------------------------------------------------
#' 
#' @return Trained model for each sample.
#' 
#' @rdname trainModel
#'
setMethod("trainModel", 
          signature(x = "data.frame"),
          trainModel <- function(x, response, independent, resamples,
                                 n_var = NULL, response_nbr = NULL, 
                                 resample_nbr = NULL){
            if(is.null(response_nbr)){
              response_nbr <- seq(length(response))
            }
            if(is.null(resample_nbr)){
              resample_nbr <- seq(length(resamples))
            }
            response_instances <- lapply(response_nbr, function(i){
              model_instances <- lapply(resample_nbr, function(j){
                print(paste0("Computing resample instance ", j, 
                             " of response instance ", i, "..."))
                
                act_resample <- resamples[[i]][[j]]
                
                resp <- x[act_resample$training$SAMPLES, 
                          act_resample$training$RESPONSE]
                indp <- x[act_resample$training$SAMPLES, independent]
                
                set.seed(10)
                cv_splits <- caret::createFolds(resp, k=2, returnTrain = TRUE)
                
                rfeCntrl <- caret::rfeControl(functions = caret::rfFuncs,
                                              method="cv", index = cv_splits,
                                              returnResamp = "all",
                                              verbose = FALSE,
                                              rerank=FALSE)
                
                trCntr <- caret::trainControl(method="cv", number = 2, 
                                              repeats = 1, verbose = FALSE)
                if(is.null(n_var)){
                  n_var_rfe <- seq(2, ncol(indp), 10)
                } else {
                  n_var_rfe <- n_var
                }
                
                if(class(resp) == "factor"){
                  metric = "Accuracy"
                } else {
                  metric = "RMSE"
                }
                
                rfe_model <- caret::rfe(indp, resp,
                                        metric = metric, method = "rf", 
                                        sizes = n_var_rfe,
                                        rfeControl = rfeCntrl,
                                        trControl = trCntr, verbose = FALSE,
                                        tuneGrid = expand.grid(mtry = n_var_rfe))
                
                test_resp <- x[act_resample$testing$SAMPLES, 
                               act_resample$testing$RESPONSE]
                test_indp <- x[act_resample$testing$SAMPLES, independent]
                test_pred <- predict(rfe_model, test_indp)

                testing <-  list(RESPONSE = test_resp, INDEPENDENT = test_indp,
                                 PREDICTED = test_pred)
                
                return(list(response = act_resample$testing$RESPONSE, 
                            model = rfe_model, testing = testing))
              })
            })
            return(response_instances)
          })

