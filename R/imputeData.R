if ( !isGeneric("imputeData") ) {
  setGeneric("imputeData", function(x, ...)
    standardGeneric("imputeData"))
}
#' Impute missing data
#'
#' @description
#' Impute missing data
#' 
#' @param NONE
#'
#' @name imputeData
#' 
#' @export imputeData
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


# Function using gpm object ----------------------------------------------
#' 
#' @return Imputed GPM object
#' 
#' @rdname imputeData
#'
setMethod("imputeData", 
          signature(x = "GPM"), 
          function(x){
            
            
            getGPMDataLayers(x)
            getGPMDataLayer(x, 2)
            
            return("TODO")
          })


# Function using data frame ----------------------------------------------------
#' 
#' @return Imputed data frame
#' 
#' @rdname imputeData
#'
setMethod("imputeData", 
          signature(x = "data.frame"), 
          function(x){
            
          })