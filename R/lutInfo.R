#' Get or access internal LUT values used by various functions
#' 
#' @description
#' Get internal look-up table (LUT) values from sysdata.rda which have been 
#' compiled using data-raw/lut_data.R. Metadata is stored in \code{lut$meta}.
#' 
#' @param None
#'
#' @return List containing several \code{data.frame} objects with LUT values.
#'
#' @export lutInfo
#' 
#' @details None
#' 
#' @examples None
#' 
lutInfo <- function(){
  return(lut)
}
