#' Train machine learning model
#'
#' @description
#' Train machine learning model.
#' 
#' @param NONE
#'
#' @return NONE
#'
#' @name trainModelrfe
#' 
#' @export trainModelrfe
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
#' 
#' @rdname trainModelrfe
#'
trainModelrfe <- function(resp, indp, n_var, mthd, seed_nbr, cv_nbr, metric,
                          tune_length = NULL, rerank = FALSE){
  # set.seed(seed_nbr)
  # cv_splits <- caret::createFolds(resp, k=cv_nbr, returnTrain = TRUE)
  
  
  rfecntrl_functions <-  lut$MTHD_DEF_LST[[mthd]]$fncs
  if(mthd == "rf"){
    rfecntrl_functions$fit <- function (x, y, first, last, ...) train(x, y, importance = TRUE, ...)
  }
  
  rfeCntrl <- caret::rfeControl(functions = rfecntrl_functions,
                                method="cv", 
                                index = act_resample$training_index,
                                indexOut = act_resample$training_indexOut,
                                returnResamp = "all",
                                verbose = FALSE,
                                rerank=rerank)
  
  set.seed(seed_nbr)
  trCntr <- caret::trainControl(method="repeatedcv", number = cv_nbr, 
                                repeats = 2, verbose = FALSE)
  
  if(is.null(n_var)){
    n_var_rfe <- seq(2, ncol(indp))
  } else {
    n_var_rfe <- n_var
  }
  
  if(is.null(tune_length)){
    set.seed(seed_nbr)
    rfe_model <- caret::rfe(indp, resp,
                            metric = metric, method = mthd, 
                            sizes = n_var_rfe,
                            rfeControl = rfeCntrl,
                            trControl = trCntr, verbose = TRUE,
                            tuneGrid = lut$MTHD_DEF_LST[[mthd]]$tunegr)
  } else {
    set.seed(seed_nbr)
    rfe_model <- caret::rfe(indp, resp,
                            metric = metric, method = mthd, 
                            sizes = n_var_rfe,
                            rfeControl = rfeCntrl,
                            trControl = trCntr, verbose = TRUE,
                            tuneLength = tune_length)
  }
  
  return(rfe_model)
}

