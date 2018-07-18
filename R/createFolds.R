if ( !isGeneric("createFolds") ) {
  setGeneric("createFolds", function(x, ...)
    standardGeneric("createFolds"))
}
#' Create testing and training folds for each response variable.
#'
#' @description TODO
#' Split a data set into testing and training samples by leaving selectors out.
#' If more than one response (i.e. dependent) variable is supplied, a different set of
#' testing/training pairs is created for each of them.
#' 
#' @param x An object of class gpm or data.frame
#' @param response The column name(s) of the response variable(s)
#' @param resamples The list of the resamples containing the individual row 
#' numbers (resulting from function \code{\link{resamplingsByVariable}})
#' @param use_selector Use the selector variable for splitting the samples into
#' training or testing (default FALSE).
#' @param selector The column name of the selector variable. Only 
#' relevant if use_selector is TRUE.
#'
#' @name createFolds
#'
#' @export createFolds
#' 
#' @details The split into training and testing samples is realized by using 
#' the caret::createDataPartition function which preserves the frequency
#' distribution of the individual response variable(s).
#' 
#' @references  The function uses functions from:
#'  Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, 
#'  Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton Kenkel, 
#'  the R Core Team, Michael Benesty, Reynald Lescarbeau, Andrew Ziem, 
#'  Luca Scrucca, Yuan Tang and Can Candan. (2016). caret: Classification and 
#'  Regression Training. https://CRAN.R-project.org/package=caret
#' 
#' @seealso \code{\link{resamplingsByVariable}} for creating n resamplings from
#' the original dataset.
#' 
#' @examples
#' \dontrun{
#' #Not run
#' }
#' 
NULL


# Function using gpm object ----------------------------------------------------
#' 
#' @return A layer within the gpm object with the information on the n 
#' individual resamplings.
#' 
#' @rdname createFolds
#'
setMethod("createFolds", 
          signature(x = "GPM"), 
          function(x, nbr = 10, nested_cv = FALSE){
            smr <- createFolds(x = x@data$input, 
                               response = x@meta$input$RESPONSE_FINAL,
                               selector = x@meta$input$SELECTOR,
                               nested_cv = nested_cv,
                               nbr = nbr)
            x@meta$input$TRAIN_TEST <- smr[[1]]
            x@meta$input$TRAIN_TEST_NSMPLS <- smr[[2]]
            return(x)
          })


# Function using data frame ----------------------------------------------------
#' 
#' @return A nested list with training and testing samples for each of the n
#' resamplings.
#' 
#' @rdname createFolds
#'
setMethod("createFolds", 
          signature(x = "data.frame"),
          function(x, response, resamples, selector, nested_cv = FALSE, nbr = 1){
            
            responses <- lapply(response, function(r){
              if(nested_cv){
                resamples = compIndexFolds(x = x, selector = selector, nbr = nbr)
              } else {
                resamples = list(list(trainsmpls = seq(nrow(x)), testsmpls = NULL))
              }
              resamples = lapply(resamples, function(s){
                
                cv = caret::createFolds(x[s$trainsmpls, which(colnames(x) == selector)], k = nbr)
                training = list(SAMPLES = s$trainsmpls, RESPONSE = r)
                training_index = lapply(cv, function(c){s$trainsmpls})
                training_indexOut = lapply(cv, function(c){c})
                testing = list(SAMPLES = s$testsmpls, RESPONSE = r)
                return(list(training = training, 
                            training_index = training_index,
                            training_indexOut = training_indexOut, 
                            testing = testing))
              })
              return(resamples)
            })
            
            training_smpls <- lapply(seq(length(responses)), function(i){
              idv <- lapply(responses[[i]], function(j){
                if(is.null(j)){
                  x <- 0
                } else {
                  x <- 1
                }
              })
              data.frame(RESPONSE = response[[i]],
                         TRAINTESTSMPLS = sum(unlist(idv)))
            })
            training_smpls <- do.call("rbind", training_smpls)
            return(list(responses, training_smpls))
          })
