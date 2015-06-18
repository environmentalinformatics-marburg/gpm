#' sample a Spatial*DataFrame by according to a factorial column 
#'
#' @description
#' This function samples a Spatial*DataFrame according to a specified
#' column so that all factor levels are sampled \code{n} times. If there
#' are less or equal entries in any levels these are sampled completely.
#' 
#' @param x a Spatial*DataFrame
#' @param col the column used for the sampling
#' @param n the amount of samples to be taken from each level of col
#' @param seed an optional seed to be used for the 
#' random \code{\link{sample}}-ing.
#' @param ... additional arguments passed to \code{\link{sample}}, 
#' e.g. \code{drop = ...}
#'
#' @return a list with entries \emph{sample_set} (the samples) 
#' and \emph{remainder} (the remainder).
#'
#' @export sampleBy
#' 
#' @seealso \code{\link{sample}} for details on the random sampling
#' 
#' @examples
#' # Not run
#' library(sp)
#' 
#' data(meuse)
#' coordinates(meuse) <- ~ x + y
#' 
#' sampleBy(meuse, col = "landuse", n = 1)
#' 
sampleBy <- function(x, col, n = 1, seed = NULL, ...) {
  
  if(class(x) == "data.frame"){
    s <- split(x, x[, col], ...)
  } else {
    s <- sp::split(x, x@data[, col], ...)
  }
  
  indx <- unlist(lapply(seq(s), function(l) {
    if (nrow(s[[l]]) > n) {
      v <- rep(FALSE, nrow(s[[l]]))
      if (is.null(seed)) set.seed(l) else set.seed(seed)
      v[sample(seq(v), n)] <- TRUE
      return(v)
    } else {
      if (nrow(s[[l]]) == n) return(TRUE)
    }
  }))
  
  if(class(x) == "data.frame"){
    ind <- order(x[, col])
  } else {
    ind <- order(x@data[, col])
  }
  x <- x[ind, ]
  
  set <- x[indx, ]
  rest <- x[!indx, ]
  
  return(list(sample_set = set,
              remainder = rest))
  
}
