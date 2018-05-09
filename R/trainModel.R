if ( !isGeneric("trainModel") ) {
  setGeneric("trainModel", function(x, ...)
    standardGeneric("trainModel"))
}
#' Model training and performance using cross-validation
#'
#' @description
#' Train a model and estimate the model performance using multiple resamplings 
#' each devided into training and independent subsets. Training subsets are 
#' further divided into k-fold cross-validation samples for model tuing. Testing
#' sampels are used for the independent validation of the final model. This
#' procedure is repeated for each resampling provided.
#' 
#' @param x An object of class gpm or data.frame
#' @param response The column name(s) of the response variable(s)
#' @param predictor The column ID of the predictor, i.e. independent 
#' variable(s) in the dataset
#' @param selector Selector id
#' @param meta Meta information of the gpm object
#' @param resamples The list of the resamples containing the individual row 
#' numbers (resulting from function \code{\link{resamplingsByVariable}})
#' @param mode Variable selection mode, either recursive feature elimination 
#' ("rfe") or forward feature selection ("ffs)
#' @param n_var Vector holding the number of variables used for the recursive 
#' feature elimination iterations; must not be continous (e.g. c(1:10, 20, 30))
#' @param response_nbr Response ID to be computed; only relevant if more than
#' one response variable is present and a model should not be built for each of
#' them
#' @param resample_nbr Resample ID to be computed; only relevant if the model
#' training should not run over all resamples 
#' @param mthd Core method used for the model (e.g. "rf" for random forest)
#' @param seed_nbr Specific seed to be to ensure reproducability
#' @param tune_length Tune length to be used in recursive feature elimination
#' (if NULL, the fixed default grid taken from the GPM LUT will be used).
#' @param metric The metric to be used to compute the model performance.
#' @param cv_nbr Specific cross validation folds to be used for model tuning 
#' within each forward or backward feature selection/elimination step
#' @param var_selection Select final number of variables based on a standard
#' deviation statistic ("sd", more conservative) or by the actual best number
#' ("indv")
#' @param filepath_tmp If set, intermediate model results during the variable
#' selection are writen to disc; if the procedure stops for some reason, the
#' already computed results can be read in again which saves computation time
#' (e.g. after an accidential shutdown etc.)
#' @param ... Additional arguments passed to \code{\link{trainModelffs}}.
#' 
#' @return NONE
#'
#' @name trainModel
#' 
#' @export trainModel
#' 
#' @details The backfard feature selection is based on the implementation of
#' the caret::rfe function. The forward feature selection is implemented from
#' scratch. The latter stops if the error statistics get worse after a first
#' optimum is reached. For model training, the respective caret functions are
#' used, too.
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
NULL


# Function using gpm object ----------------------------------------------------
#' 
#' @return A layer within the gpm object with the model training information for
#' each response variable and all resamplings.
#' 
#' @rdname trainModel
#'
setMethod("trainModel", 
          signature(x = "GPM"), 
          function(x, n_var = NULL, 
                   mthd = "rf", mode = c("rfe", "ffs"),
                   seed_nbr = 11, cv_nbr = NULL,
                   var_selection = c("sd", "indv"), 
                   metric = NULL, tune_length = NULL,
                   response_nbr = NULL, resample_nbr = NULL,
                   filepath_tmp = NULL,
                   rerank = FALSE, ...){
            x@model[[paste0(mthd, "_", mode)]] <- trainModel(x = x@data$input,
                                                             response = x@meta$input$RESPONSE_FINAL,
                                                             predictor = x@meta$input$PREDICTOR_FINAL,
                                                             selector = x@meta$input$SELECTOR,
                                                             meta = x@meta$input$META,
                                                             resamples = x@meta$input$TRAIN_TEST,
                                                             n_var = n_var,
                                                             mthd = mthd, 
                                                             mode = mode,
                                                             seed_nbr = seed_nbr, 
                                                             cv_nbr = cv_nbr,
                                                             var_selection = var_selection,
                                                             metric = metric,
                                                             tune_length = tune_length,
                                                             response_nbr = response_nbr,
                                                             resample_nbr = resample_nbr,
                                                             filepath_tmp = filepath_tmp, 
                                                             ...)
            return(x)
          })


# Function using data frame ----------------------------------------------------
#' 
#' @return Trained model for each response variable and all resamplings.
#' 
#' @rdname trainModel
#'
setMethod("trainModel", 
          signature(x = "data.frame"),
          function(x, response, predictor, selector, meta, resamples,
                   n_var = NULL, mthd = "rf", 
                   mode = c("rfe", "ffs"), seed_nbr = 11, 
                   cv_nbr = 2, var_selection = c("sd", "indv"),
                   metric = NULL, tune_length = NULL,
                   response_nbr = NULL, resample_nbr = NULL, 
                   filepath_tmp = NULL, rerank = FALSE,...){
            mode <- mode[1]
            var_selection <- var_selection[1]
            predictor_best <- predictor
            
            if (!is.null(filepath_tmp))
              filepath_tmp <- file.path(filepath_tmp, "")
            
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
                
                if(!is.null(act_resample)){
                  resp <- x[act_resample$training$SAMPLES, 
                            act_resample$training$RESPONSE]
                  indp <- x[act_resample$training$SAMPLES, predictor]
                  
                  if(is.null(metric)){
                    if(class(resp) == "factor"){ 
                      # metric = "Accuracy"
                      metric = "Kappa"
                    } else {
                      metric = "RMSE"
                      # metric = "Rsquared"
                    }
                  }
                  
                  if(mode == "rfe"){
                    model <- try(trainModelrfe(resp = resp, indp = indp, n_var = n_var, 
                                               mthd = mthd, seed_nbr = seed_nbr, 
                                               cv_nbr = cv_nbr, metric = metric,
                                               tune_length = tune_length,
                                               rerank = rerank))
                    
                    
                    if(!inherits(model, "try-error") & var_selection == "sd"){
                      predictor_best <- gpm:::trainModelVarSelSD(model, 
                                                                 metric = model$metric, 
                                                                 maximize=FALSE,
                                                                 sderror=TRUE)
                      if(class(predictor_best) != "character"){
                        predictor_best <- as.character(names(predictor_best))
                      }
                      cv_splits <- caret::createFolds(resp, k=cv_nbr, returnTrain = TRUE)
                      
                      trCntr <- caret::trainControl(method="cv", number = cv_nbr, 
                                                    index = cv_splits,
                                                    returnResamp = "all",
                                                    repeats = 1, verbose = FALSE)
                      # savePredictions = TRUE
                      
                      model <- try(train(indp[, predictor_best], resp,  
                                         metric = metric, method = mthd,
                                         trControl = trCntr,
                                         # tuneLength = tuneLength,
                                         tuneGrid = lut$MTHD_DEF_LST[[mthd]]$tunegr,
                                         verbose = FALSE))
                    }
                    
                  } else if (mode == "ffs"){
                    #model <- try(trainModelffs(resp = resp, indp = indp, n_var = n_var, 
                    #                           mthd = mthd, seed_nbr = seed_nbr, 
                    #                           cv_nbr = cv_nbr, metric = metric, 
                    #                           withinSD = TRUE, ...))
                    # cv_splits <- caret::createFolds(resp, k=cv_nbr, returnTrain = TRUE)
                    
                    trCntr <- caret::trainControl(method="cv",
                                                  index = act_resample$training_index,
                                                  indexOut = act_resample$training_indexOut,
                                                  returnResamp = "all",
                                                  repeats = 1, verbose = FALSE)
                    
                    
                    model <- try(CAST::ffs(predictors = indp[, predictor_best], 
                                           response = resp,  
                                           metric = metric, method = mthd,
                                           trControl = trCntr,
                                           # tuneLength = tuneLength,
                                           tuneGrid = lut$MTHD_DEF_LST[[mthd]]$tunegr, ...))
                    
                    
                  }
                  
                  train_selector <- x[act_resample$training$SAMPLES, selector]
                  if(length(meta) == 0){
                    train_meta = NULL
                  } else {
                    train_meta <- x[act_resample$training$SAMPLES, meta]  
                  }
                  training <- list(RESPONSE = resp, PREDICTOR = indp[, predictor_best],
                                   SELECTOR = train_selector, META = train_meta)
                  
                  if(inherits(model, "try-error")){
                    testing <- list(NA)
                  } else if(is.null(act_resample$testing$SAMPLES)){
                    testing <- list(NULL)
                  } else {
                    
                    test_resp <- x[act_resample$testing$SAMPLES, 
                                   act_resample$testing$RESPONSE]
                    test_indp <- x[act_resample$testing$SAMPLES, predictor_best]
                    
                    test_resp <- test_resp[complete.cases(test_indp)]
                    test_indp <- test_indp[complete.cases(test_indp),]
                    
                    if(class(model) == "train"){
                      test_pred <- data.frame(pred = predict(model, test_indp, type = "raw"))
                      if(class(resp) == "factor"){
                        test_pred <- cbind(test_pred, predict(model, test_indp, type = "prob"))
                      }
                      
                      # if(lut$MTHD_DEF_LST[[mthd]]$type == "prob"){
                      #   test_pred <- cbind(test_pred, predict(model, test_indp, type = "prob"))
                      # }
                    } else {
                      test_pred <- predict(model, test_indp)
                    }
                    
                    test_selector <- x[act_resample$testing$SAMPLES, selector]
                    if(length(meta) == 0){
                      test_meta = NULL
                    } else {
                      test_meta <- x[act_resample$testing$SAMPLES, meta]
                    }
                    testing <-  list(RESPONSE = test_resp, PREDICTOR = test_indp,
                                     PREDICTED = test_pred, 
                                     SELECTOR = test_selector, META = test_meta)
                  }
                  model_instances <- list(response = act_resample$testing$RESPONSE, 
                                          model = model, training = training,
                                          testing = testing)
                  if(!is.null(filepath_tmp)){
                    save(model_instances, 
                         file = 
                           paste0(filepath_tmp, 
                                  sprintf("gpm_trainModel_model_instances_%03d_%03d", i, j),
                                  ".RData"))              
                  }
                  return(model_instances)
                } else {
                  return(NULL)
                }
                

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

