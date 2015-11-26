 if ( !isGeneric("minimumOccurence") ) {
  setGeneric("minimumOccurence", function(x, ...)
    standardGeneric("minimumOccurence"))
}
#' Select features based on minimum occurence across unique locations
#'
#' @description
#' Select features (e.g. species) based on their minimum occurence across unique
#' locations with multiple samples per location and return the feature names 
#' which occure at least on n locations on average within multiple resamples 
#' withour replacement.
#' 
#' @param NONE
#'
#' @name minimumOccurence
#' 
#' @export minimumOccurence
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
#' @return Columnnames of the features occuring at least on n locations on 
#' average as part of the meta data section of the gpm object
#' 
#' @rdname minimumOccurence
#'
setMethod("minimumOccurence", 
          signature(x = "GPM"), 
          function(x, resample = 100, thv = 20){
            return("TODO")
          })


# Function using data frame ----------------------------------------------------
#' 
#' @return Columnnames of the features occuring at least on n locations on 
#' average.
#' 
#' @rdname minimumOccurence
#'
setMethod("minimumOccurence", 
          signature(x = "data.frame"), 
          function(x, selector, occurence = "yes", resample = 100, thv = 20){
            si <- 0
            mo <- do.call("rbind", lapply(seq(resample), function(i){
              si <- si + 1
              sj <- 0
              act_smpl <- do.call("rbind", lapply(unique(selector), function(j){
                sj <- sj + 1
                set.seed(si+sj)
                act_plot <- sample(which(selector == j), size = 1)
                data.frame(selector = j,
                           x[act_plot, ])
              }))
              if(class(occurence) == "character"){
                occ <- as.data.frame(t(colSums(occurence == act_smpl[, 2:ncol(act_smpl)], na.rm = TRUE)))  
              } else{
                occ <- as.data.frame(t(colSums(occurence < act_smpl[, 2:ncol(act_smpl)], na.rm = TRUE))) 
              }
              return(occ)
            }))
            mo_mean <- colMeans(mo)
            return(list(names = names(mo_mean[mo_mean >= thv]),
                        mo_mean = mo_mean[mo_mean >= thv]))
          })