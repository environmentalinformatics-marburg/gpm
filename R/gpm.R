if ( !isGeneric("gpm") ) {
  setGeneric("gpm", function(x, ...)
    standardGeneric("gpm"))
}
#' Create a gpm object
#'
#' @description
#' Method to create a gpm Object
#' 
#' @param x a data frame
#' @param meta meta information regarding the content of the columns, e.g. 
#' created using \code{\link{createGPMMeta}}
#' 
#' @return gpm object
#' 
#' @export gpm
#' 
#' @details A gpm object consists of three data sections:
#' (i) a data section which holds the actual dataset, (ii) a meta data grid 
#' which holds meta information for each of the columns in the data set and 
#' (iii) a list of log information which records the processing history of the 
#' data set.
#' 
#' @name gpm
#' 
#' @examples
#' data(abies_alba)
#' meta <- createGPMMeta(dataset = abies_alba, 
#'                       selector = 1, response = c(16:481), meta = c(2: 15))
#' alb <- gpm(abies_alba, meta)
#' 
NULL


# Function using a data frame --------------------------------------------------
#' 
#' @rdname gpm
#' 
setMethod("gpm", 
          signature(x = "data.frame"), 
          function(x, meta, log, scale = FALSE, maxnew = 1, minnew = 0){
            if(missing(meta)){
              print("Please provide meta information")
              stop
            }
            if(scale == TRUE){
              for(i in meta$PREDICTOR){
                x[,i] = scale(x[,i], center = TRUE, scale = TRUE)
                # a = (maxnew - minnew)/(max(x[,i], na.rm = TRUE) - min(x[,i], na.rm = TRUE))
                # b <- maxnew - a * max(x[,i], na.rm = TRUE)
                # x[,i] <- a * x[,i] + b
              }
            }
            
            #Create row index
            x$rowID = seq(nrow(x))
            meta$META = c(meta$META, "rowID")
            
            data <- list(x)
            names(data) <- meta$TYPE
            meta <- list(meta)
            names(meta) <- names(data)
            
            
            if(missing(log)){
              ps <- list(time = Sys.time(), info = "Initial import")
              log <- list(ps0001 = ps)
            }
            return(new("GPM", data = data, meta = meta, log = log))
          })
