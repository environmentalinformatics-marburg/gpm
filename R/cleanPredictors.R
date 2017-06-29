if ( !isGeneric("cleanPredictors") ) {
  setGeneric("cleanPredictors", function(x, ...)
    standardGeneric("cleanPredictors"))
}
#' Clean predictor variables regarding corelation and variance
#'
#' @description Clean predictor variables regarding zero or near-zero variability
#' or highly correlated variables.
#' 
#' @param x An object of class gpm
#' @param nzv Remove (near) zero variability predictor variables
#' @param highcor Remove highly correlated predictor variables
#' @param cutoff Correlation cutoff value for highcor
#' @param rmvna Remove predictors with na values
#' 
#' @name cleanPredictors
#' 
#' @export cleanPredictors
#' 
#' @details The function is realized by using the caret::nearZeroVar and
#' caret::findCorrelation functions.
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
#' @return A layer within the gpm object with the information on the n 
#' individual resamplings.
#' 
#' @rdname cleanPredictors
#'
setMethod("cleanPredictors", 
          signature(x = "GPM"), 
          function(x, nzv = TRUE, highcor = TRUE, cutoff = 0.90, rmvna = TRUE){
            
            if(rmvna == TRUE){
              x@meta$input$PREDICTOR_RMVNA <- 
                names(which(colSums(is.na(x@data$input[, x@meta$input$PREDICTOR_FINAL])) > 0))
              
              na_id <- which(x@meta$input$PREDICTOR_FINAL %in% x@meta$input$PREDICTOR_RMVNA)
              if(length(na_id) > 0){
                x@meta$input$PREDICTOR_FINAL <- x@meta$input$PREDICTOR_FINAL[-na_id]  
              }
            }            
            
            if(nzv == TRUE){
              x@meta$input$PREDICTOR_ZEROVAR <-
                nearZeroVar(x@data$input[, x@meta$input$PREDICTOR_FINAL], 
                            saveMetrics= TRUE)
              
              rmv <- which(x@meta$input$PREDICTOR_ZEROVAR$zeroVar == TRUE | 
                             x@meta$input$PREDICTOR_ZEROVAR$nzv == TRUE)
              
              if(length(rmv) > 0){
                x@meta$input$PREDICTOR_FINAL <- x@meta$input$PREDICTOR_FINAL[-rmv]  
              }
            }

            if(highcor == TRUE){
              hc <- 
                findCorrelation(cor(
                  x@data$input[, x@meta$input$PREDICTOR_FINAL],  
                               use = "pairwise.complete.obs"),  
                  cutoff = cutoff)  
              x@meta$input$PREDICTOR_HIGHCOR <- 
                x@meta$input$PREDICTOR_FINAL[hc]
              
              x@meta$input$PREDICTOR_FINAL <- x@meta$input$PREDICTOR_FINAL[-hc]
            }

            return(x)
          })

