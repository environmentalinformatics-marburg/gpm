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
trainModelrfe <- function(resp, indp, n_var, mthd, seed_nbr, cv_nbr, metric){
  set.seed(seed_nbr)
  cv_splits <- caret::createFolds(resp, k=cv_nbr, returnTrain = TRUE)
  
  rfeCntrl <- caret::rfeControl(functions = lut$MTHD_DEF_LST[[mthd]]$fncs,
                                method="boot", index = cv_splits,
                                returnResamp = "all",
                                verbose = FALSE,
                                rerank=FALSE)
  
  trCntr <- caret::trainControl(method="cv", number = cv_nbr, 
                                repeats = 1, verbose = FALSE)
  if(is.null(n_var)){
    n_var_rfe <- seq(2, ncol(indp))
  } else {
    n_var_rfe <- n_var
  }
  
  rfe_model <- caret::rfe(indp, resp,
                          metric = metric, method = mthd, 
                          sizes = n_var_rfe,
                          rfeControl = rfeCntrl,
                          trControl = trCntr, verbose = TRUE,
                          tuneGrid = lut$MTHD_DEF_LST[[mthd]]$tunegr)
  return(rfe_model)
}

