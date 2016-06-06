#' Calculate the standard error of the mean
#' 
#' @description
#' This function calculates the standard error of the mean.
#' 
#' @param x a numeric vector
#' 
#' @examples
#' a <- rnorm(1000, 20, 5)
#' mean(a)
#' se(a)
helpStandardError <- function(x) sd(x, na.rm = TRUE)/sqrt(length(na.exclude(x)))
