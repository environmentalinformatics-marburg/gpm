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
#' @param x An object of class gpm or data.frame
#' @param selector The column name of the selector variable which is used to
#' compute the minimum occurence distribution
#' @param occurence Value indicating occurence (default "yes")
#' @param resample Number of resamples used to estimate the occurence 
#' (default 100)
#' @param thv Thresholdvalue which should be exceed in the occurence in order to
#' include the respective variable in the result list.
#' 
#' @name minimumOccurence
#' 
#' @export minimumOccurence
#' 
#' @details The occurence is not counted but estimated based on samples which
#' are drawn from the dataset with one sample per selector ID.
#' 
#' @references  NONE
#' 
#' @seealso NONE
#' 
#' @examples
#' \dontrun{
#' #Not run
#' }
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
          function(x, occurence = "yes", resample = 100, thv = 20){
          x@meta$input$MIN_OCCURENCE <- 
              minimumOccurence(x = x@data$input[, x@meta$input$RESPONSE],
                               selector = x@data$input[, x@meta$input$SELECTOR],
                               occurence = occurence, 
                               resample = resample, 
                               thv = thv)
            x@meta$input$RESPONSE_FINAL <- as.character(x@meta$input$MIN_OCCURENCE$names)
             return(x)
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
            seed <- 0
            mo <- do.call("rbind", lapply(seq(resample), function(i){
              if(i %% 10 == 0) print(paste0("Processing sample ", i, " of ", resample))
              act_smpl <- do.call("rbind", lapply(unique(selector), function(j){
                seed <<-seed + 1
                set.seed(seed)
                act_plot <- sample(which(selector == j), size = 1)
                data.frame(selector = j,
                           x[act_plot, ])
              }))
              if(class(occurence) == "character"){
                occ <- as.data.frame(t(colSums(occurence == act_smpl[, 2:ncol(act_smpl)], na.rm = TRUE)))  
              } else{
                occ <- as.data.frame(t(colSums(occurence < act_smpl[, 2:ncol(act_smpl)], na.rm = TRUE))) 
              }
              #print(max(occ))
              return(occ)
            }))
            mo_mean <- colMeans(mo)
            return(data.frame(names = names(mo_mean[mo_mean >= thv]),
                              mo_mean = mo_mean[mo_mean >= thv]))
          })