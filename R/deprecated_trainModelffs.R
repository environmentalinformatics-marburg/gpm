#' Train machine learning model using forward feature selection
#'
#' @description
#' A simple forward feature selection algorithm.
#' 
#' @param predictors see \code{\link{train}}
#' @param response see \code{\link{train}}
#' @param method see \code{\link{train}}
#' @param metric see \code{\link{train}}
#' @param maximize see \code{\link{train}}
#' @param withinSD Logical Models are only selected if they are better than the 
#' currently best models SD
#' @param trControl see \code{\link{train}}
#' @param tuneLength see \code{\link{train}}
#' @param tuneGrid see \code{\link{train}}
#' @param seed A random number
#' @param runParallel Logical
#' @param ... arguments passed to the classification or regression routine 
#' (such as randomForest). Errors will occur if values for tuning parameters are 
#' passed here.
#' @return A list of class train
#' 
#' @name deprecated_trainModelffs
#' 
#' @export deprecated_trainModelffs
#' 
#' @details Models with two predictors are first trained using all possible 
#' pairs of predictor variables. The best model of these initial models is kept.
#' On the basis of this best model the predictor variables are iteratively
#' increased and each of the remaining variables is tested for its improvement
#' of the currently best model. The process stops if none of the remaining 
#' variables increases the model performance when added to the current best model.
#' 
#' The internal cross validation can be run in parallel. See information
#' on parallel processing of carets train functions for details.
#' 
#' Using withinSE will favour models with less variables and
#' probably shorten the calculation time 
#' 
#' @references  NONE
#' 
#' @note This validation is particulary suitable for 
#' leave-one-station-out cross validations where variable selection
#' MUST be based on the performance of the model on the hold out station.
#' A computational time expesnsive alternative is the best subset
#' selection (\code{\link{bss}}).#'
#' 
#' @seealso NONE
#' 
#' @examples
#' # Not run
#' 
#' 
#' @rdname deprecated_trainModelffs
#'
deprecated_trainModelffs <- function(resp, indp, n_var, mthd, seed_nbr, cv_nbr, metric,
                          withinSD = TRUE, runParallel = TRUE){

  if(is.null(n_var)){
    n_var_ffe <- seq(2, ncol(indp), 10)
  } else {
    n_var_ffe <- n_var
  }
  
  if (metric=="Rsquared"||metric=="ROC"||metric=="Accuracy"){
    maximize = TRUE
    evalfunc <- function(x){max(x,na.rm=T)}
  } else {
    maximize = FALSE
    evalfunc <- function(x){min(x,na.rm=T)}
  }

  isBetter <- function (actmodelperf,bestmodelperf,bestmodelperfSD=NULL,maximize=maximize,
                        withinSD=withinSD){
    if(withinSD){
      ifelse (!maximize, return(actmodelperf < bestmodelperf-bestmodelperfSD),
              return(actmodelperf > bestmodelperf+bestmodelperfSD))
    }else{
      ifelse (!maximize, return(actmodelperf < bestmodelperf),
              return(actmodelperf > bestmodelperf))
    }
  }


  trCntr <- caret::trainControl(method="cv", number = cv_nbr, 
                                returnResamp="all", savePredictions = TRUE,
                                repeats = 1, verbose = FALSE)
  
  
  # trainControl(method="cv", index=cvindices,returnResamp="all",savePredictions = TRUE)
  
  n <- length(names(indp))
  acc <- 0

  #### chose initial best model from all combinations of two variables
  twogrid <- t(data.frame(combn(names(indp),2)))
  for (i in seq(nrow(twogrid))){
    if(runParallel){
      require(doParallel)
      cl <- makeCluster(detectCores()-1)
      registerDoParallel(cl)
    }
    set.seed(seed_nbr)
    model <- try(train(indp[,twogrid[i,]],
                   resp,
                   method=mthd,
                   trControl=trCntr,
                   tuneGrid = lut$MTHD_DEF_LST[[mthd]]$tunegr))
    if(runParallel){
      stopCluster(cl)
    }
    
    ### compare the model with the currently best model
    if(inherits(model, "try-error")){
      NULL
    } else {
    actmodelperf <- evalfunc(model$results[,names(model$results)==metric])
    if(withinSD){
      actmodelperfSD <- model$results[,names(model$results)==paste0(metric,"SD")][
        which(model$results[,names(model$results)==metric]==actmodelperf)]
    }
    if (i == 1){
      bestmodelperf <- actmodelperf
      if(withinSD){
        bestmodelperfSD <- actmodelperfSD
      }
      bestmodel <- model
    } else{
      if (isBetter(actmodelperf,bestmodelperf,maximize=maximize,withinSD=FALSE)){
        bestmodelperf <- actmodelperf 
        if(withinSD){
          bestmodelperfSD <- actmodelperfSD
        }
        bestmodel <- model
      }
    }
    }
    acc <- acc+1
    print(paste0("maxmimum number of models that still need to be trained: ",
                 (((n-1)^2)+n-1)/2 + (((n-2)^2)+n-2)/2 - acc))
  }
  
  #### increase the number of predictors by one (try all combinations) 
  #and test if model performance increases
  for (k in seq(length(names(indp))-2)){
    startvars <- names(bestmodel$trainingData)[-which(
      names(bestmodel$trainingData)==".outcome")]
    nextvars <- names(indp)[-which(
      names(indp)%in%startvars)]
    if (length(startvars)<(k+1)){
      message(paste0("Note: No increase in performance found using more than ",
                     length(startvars), " variables"))
      return(bestmodel)
      break()
    }
    for (i in seq(length(nextvars))){
      if(runParallel){
        require(doParallel)
        cl <- makeCluster(detectCores()-1)
        registerDoParallel(cl)
      }
      set.seed(seed_nbr)
      model <- try(train(indp[,c(startvars,nextvars[i])],
                     resp,
                     method = mthd,
                     trControl = trCntr,
                     # tuneLength = tuneLength,
                     tuneGrid = lut$MTHD_DEF_LST[[mthd]]$tunegr))
      if(runParallel){
        stopCluster(cl)
      }
      
      if(inherits(model, "try-error")){
        NULL
      } else {
      actmodelperf <- evalfunc(model$results[,names(model$results)==metric])
      if(withinSD){
        actmodelperfSD <- model$results[,names(model$results)==paste0(metric,"SD")][
          which(model$results[,names(model$results)==metric]==actmodelperf)]
      }
      if(isBetter(actmodelperf,bestmodelperf,bestmodelperfSD,
                  maximize=maximize,withinSD=withinSD)){
        bestmodelperf <- actmodelperf 
        if(withinSD){
          bestmodelperfSD <- actmodelperfSD
        }
        bestmodel <- model
      }
      }
      acc <- acc+1
      print(paste0("maxmimum number of models that still need to be trained: ",
                   (((n-1)^2)+n-1)/2 + (((n-2)^2)+n-2)/2 - acc))
    }
  }
  return(bestmodel)
}
  