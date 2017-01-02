if ( !isGeneric("splitMultResp") ) {
  setGeneric("splitMultResp", function(x, ...)
    standardGeneric("splitMultResp"))
}
#' Split dataset into testing and training samples individually for each 
#' response variable
#'
#' @description
#' Split a data set into testing and training samples. If more than one
#' response (i.e. dependent) variable is supplied, a different set of
#' testing/training pairs is created for each of them. The training/testing
#' samples preserve the frequency distribution of the individual response 
#' variable.
#' 
#' @param x An object of class gpm or data.frame
#' @param response The column name(s) of the response variable(s)
#' @param resamples The list of the resamples containing the individual row 
#' numbers (resulting from function \code{\link{resamplingsByVariable}})
#' @param p The fraction of each sample to be used for model training 
#' (default 0.75)
#' @param use_selector Use the selector variable for splitting the samples into
#' training or testing (default FALSE).
#' @param selector The column name of the selector variable. Only 
#' relevant if use_selector is TRUE.
#'
#' @name splitMultResp
#'
#' @export splitMultResp
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
#' @rdname splitMultResp
#'
setMethod("splitMultResp", 
          signature(x = "GPM"), 
          function(x, p = 0.75, use_selector = FALSE){
            x@meta$input$TRAIN_TEST <- splitMultResp(x = x@data$input, 
                                             response = x@meta$input$RESPONSE,
                                             resamples = x@meta$input$RESAMPLES,
                                             p = p, 
                                             use_selector = use_selector,
                                             selector = x@meta$input$SELECTOR)
            return(x)
          })


# Function using data frame ----------------------------------------------------
#' 
#' @return A nested list with training and testing samples for each of the n
#' resamplings.
#' 
#' @rdname splitMultResp
#'
setMethod("splitMultResp", 
          signature(x = "data.frame"),
          function(x, response, resamples, p = 0.75, use_selector = FALSE,
                   selector = NULL){
            if(use_selector == FALSE){
              fs <- lapply(response, function(i){
                idv <- lapply(resamples, function(j){
                  smpl <- caret::createDataPartition(x[j, i], p = p, 
                                                     list = FALSE, times = 1)
                  training = list(SAMPLES = as.numeric(j[smpl]), RESPONSE = i)
                  testing = list(SAMPLES = as.numeric(j[-smpl]), RESPONSE = i)
                  # list(training = x[smpl, -response[-which(response %in% i)]], 
                  #      test = x[-smpl, -response[-which(response %in% i)]])
                  # x[training$SAMPLES, training$RESPONSE]
                  # x[testing$SAMPLES, testing$RESPONSE]
                  # list(training = x[smpl, -response[-which(response %in% i)]], 
                  #     test = x[-smpl, -response[-which(response %in% i)]])
                  list(training = training, testing = testing)
                })
              })
            } else {
              fs <- lapply(response, function(i){
                idv <- lapply(resamples, function(j){
                  smpl <- which(x[, selector] == unique(x[,selector])[1])
                  training = list(SAMPLES = as.numeric(j[smpl]), RESPONSE = i)
                  testing = list(SAMPLES = as.numeric(j[-smpl]), RESPONSE = i)
                  list(training = training, testing = testing)
                })
              })
            }
            return(fs)
          })
