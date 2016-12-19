#' An S4 class to represent gpm metadata information
#' 
#' @slot meta a list object containing meta information concenring data variables
#' 
#' @exportClass GPMMeta
#' 
setClass("GPMMeta",
         representation(
           meta = "list"
         )
)


#' An S4 class to represent gpm data layers
#'
#' @slot data a list of data frames containing independent, dependent and meta variables
#' 
#' @exportClass GPMData
#' 
setClass("GPMData",
         representation(
           data = "list"
         )
)


#' An S4 class to represent gpm log data
#'
#' @slot log a list object containing information on individual processing steps
#' 
#' @exportClass GPMLog
#' 
setClass("GPMLog", 
         representation(
           log = "list"
         )
)

#' An S4 class to represent a complete gmp dataset
#' 
#' @exportClass GPM
#' 
setClass("GPM", 
         contains = c("GPMData", "GPMMeta", "GPMLog")
)         
