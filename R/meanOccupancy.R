#' Select features based on mean occupancy across unique locations
#'
#' @description
#' Compute occupancy of a feature x across y unique locations (but with 
#' multiple samples per location) and return the feature names which occure at 
#' least on n locations on average.
#' 
#' @param NONE
#'
#' @return Columnnames of the selected features which occure at least on n
#' locations on average.
#'
#' @export meanOccupancy
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
meanOccupancy <- function(plotid, observations, resample = 100, thv = 20, seedid){
  mo <- do.call("rbind", lapply(seq(resample), function(x){
    act_smpl <- do.call("rbind", lapply(unique(plotid), function(y){
      set.seed(y)
      act_plot <- sample(which(plotid == y), size = 1)
      data.frame(PLOTID = y,
                 observations[act_plot, ])
    }))
    as.data.frame(t(colSums(act_smpl[, 2:ncol(act_smpl)])))
  }))
  mo_mean <- colMeans(mo)
  return(names(mo_mean[mo_mean >= thv]))
}