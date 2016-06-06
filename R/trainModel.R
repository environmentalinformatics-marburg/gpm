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
                                 mode = c("rfe", "ffs"),
                                 n_var = NULL, response_nbr = NULL, 
                                 resample_nbr = NULL, mthd = "rf",
                                 seed_nbr = 11, cv_nbr = 2,
                                 var_selection = c("sd", "indv"),
                                 filepath_tmp = NULL){
            mode <- mode[1]
            var_selection <- var_selection[1]
            independent_best <- independent
            
            if(is.null(response_nbr)){
              response_nbr <- seq(length(response))
            }
            if(is.null(resample_nbr)){
              resample_nbr <- seq(length(resamples[[1]]))
            }
            
            response_instances <- lapply(response_nbr, function(i){
              model_instances <- lapply(resample_nbr, function(j){
                print(paste0("Computing resample instance ", j, 
                             " of response instance ", i, "..."))
                
                act_resample <- resamples[[i]][[j]]
                
                resp <- x@data$input[act_resample$training$SAMPLES, 
                                     act_resample$training$RESPONSE]
                indp <- x@data$input[act_resample$training$SAMPLES, independent]
                
                if(class(resp) == "factor"){ 
                  metric = "Accuracy"
                } else {
                  #metric = "RMSE"
                  metric = "Rsquared"
                }
                
                if(mode == "rfe"){
                  model <- try(trainModelrfe(resp = resp, indp = indp, n_var = n_var, 
                                             mthd = mthd, seed_nbr = seed_nbr, 
                                             cv_nbr = cv_nbr, metric = metric))
                  
                  
                  if(!inherits(model, "try-error") & var_selection == "sd"){
                    independent_best <- gpm:::trainModelVarSelSD(model, 
                                                               metric = model$metric, 
                                                               maximize=FALSE,
                                                               sderror=TRUE)
                    if(class(independent_best) != "character"){
                      independent_best <- as.character(names(independent_best))
                    }
                    cv_splits <- caret::createFolds(resp, k=cv_nbr, returnTrain = TRUE)
                    
                    trCntr <- caret::trainControl(method="cv", number = cv_nbr, 
                                                  index = cv_splits,
                                                  returnResamp = "all",
                                                  repeats = 1, verbose = FALSE)
                    # savePredictions = TRUE
                    
                    model <- try(train(indp[, independent_best], resp,  
                                   metric = metric, method = mthd,
                                   trControl = trCntr,
                                   # tuneLength = tuneLength,
                                   tuneGrid = lut$MTHD_DEF_LST[[mthd]]$tunegr,
                                   verbose = TRUE))
                  }
                  
                } else if (mode == "ffs"){
                  model <- try(trainModelffs(resp = resp, indp = indp, n_var = n_var, 
                                             mthd = mthd, seed_nbr = seed_nbr, 
                                             cv_nbr = cv_nbr, metric = metric, 
                                             withinSD = TRUE, runParallel = TRUE))
                }
                
                train_selector <- x@data$input[act_resample$training$SAMPLES, 
                                               x@meta$input$SELECTOR]
                train_meta <- x@data$input[act_resample$training$SAMPLES, 
                                           x@meta$input$META]
                training <- list(RESPONSE = resp, INDEPENDENT = indp[, independent_best],
                                 SELECTOR = train_selector, META = train_meta)
                
                if(inherits(model, "try-error")){
                  testing <- list(NA)
                } else {
                  
                  test_resp <- x@data$input[act_resample$testing$SAMPLES, 
                                            act_resample$testing$RESPONSE]
                  test_indp <- x@data$input[act_resample$testing$SAMPLES, independent_best]
                  
                  if(class(model) == "train"){
                    test_pred <- data.frame(pred = predict(model, test_indp, type = "raw"))
                    test_pred <- cbind(test_pred, predict(model, test_indp, type = "prob"))
                  } else {
                    test_pred <- predict(model, test_indp)
                  }

                  test_selector <- x@data$input[act_resample$testing$SAMPLES, 
                                                x@meta$input$SELECTOR]
                  test_meta <- x@data$input[act_resample$testing$SAMPLES, 
                                            x@meta$input$META]
                  testing <-  list(RESPONSE = test_resp, INDEPENDENT = test_indp,
                                   PREDICTED = test_pred, 
                                   SELECTOR = test_selector, META = test_meta)
                }

                return(list(response = act_resample$testing$RESPONSE, 
                            model = model, training = training,
                            testing = testing))
              })
              if(!is.null(filepath_tmp)){
                save(model_instances, 
                     file = 
                       paste0(filepath_tmp, 
                              sprintf("gpm_trainModel_model_instances_%03d", i),
                              ".RData"))              
              }
              return(model_instances)
            })
            return(response_instances)
          })

