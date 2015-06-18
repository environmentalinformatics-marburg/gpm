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
#' @return Columnnames of the features occuring at least on n locations on 
#' average.
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
minimumOccurence <- function(plot_ids, observations, resample = 100, thv = 20){
  mo <- do.call("rbind", lapply(seq(resample), function(x){
    act_smpl <- do.call("rbind", lapply(unique(plot_ids), function(y){
      set.seed(y)
      act_plot <- sample(which(plot_ids == y), size = 1)
      data.frame(plot_ids = y,
                 observations[act_plot, ])
    }))
    as.data.frame(t(colSums(act_smpl[, 2:ncol(act_smpl)])))
  }))
  mo_mean <- colMeans(mo)
  return(names(mo_mean[mo_mean >= thv]))
}