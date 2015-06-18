#' Train machine learning model
#'
#' @description
#' Train machine learning model.
#' 
#' @param NONE
#'
#' @return NONE
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
trainModel <- function(lodf, response_column, independent_columns,
                       response_nbr = NULL, model_instc = NULL){
  if(is.null(response_nbr)){
    response_nbr <- seq(length(lodf))
  }
  response_instances <- lapply(response_nbr, function(x){
    if(is.null(model_instc)){
      model_instc <- seq(length(lodf[[x]]))
    }
    model_instances <- lapply(model_instc, function(y){
      print(paste0("Computing model instance ", y, " of response instance ", x))
      act <- lodf[[x]][[y]]
      
      resp <- act$training[, response_column]
      indp <- act$training[, independent_columns]
      act_test <- act$test
      
      set.seed(10)
      cv_splits <- caret::createFolds(resp, k=5, returnTrain = TRUE)
      
      # thresholds=c(seq(0.0, 0.40, 0.02),seq(0.50,1,0.1))
      # summaryFunction = "fourStats"
      rfeCntrl <- caret::rfeControl(functions = caret::caretFuncs,
                                    method="cv", index = cv_splits,
                                    returnResamp = "all",
                                    verbose = FALSE,
                                    rerank=FALSE)
      
      trCntr <- caret::trainControl(method="cv", number = 5, 
                                    repeats = 1, verbose = FALSE)
      n_var <- seq(2, ncol(indp), 8)
      
      
      # method = rf_thvs
      # 
      #   ctrl <- trainControl(index=cvSplits,
      #                        method="cv",
      #                        summaryFunction = eval(parse(text=summaryFunction)),
      #                        classProbs = classProbs)
      
      rfe_model <- caret::rfe(indp, resp,
                              metric = "Accuracy", method = "rf", 
                              sizes = n_var,
                              rfeControl = rfeCntrl,
                              trControl = trCntr, verbose = FALSE,
                              tuneGrid = expand.grid(mtry = n_var))
      return(list(model = rfe_model, testdata = act_test))
    })
  })
  return(response_instances)
}

