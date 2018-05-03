#' Create testing and training folds for each response variable following a 
#' grouping by index strategy.
#'
#' @description
#' TODO
#' Split a data set into testing and training samples by leaving selectors out.
#' If more than one response (i.e. dependent) variable is supplied, a different set of
#' testing/training pairs is created for each of them. The training/testing
#' samples leave one or more selectors completely out. Selectors can be something
#' like individual locations, a certain time step or a combination of both. 
#' Basically, it can be anything since the selector variable is defined by the
#' user and hence it can be compiled in such a way that it leaves out whatever
#' the user wants.
#' 
#' @param x An object of class gpm or data.frame
#' @param response The column name(s) of the response variable(s)
#' @param resamples The list of the resamples containing the individual row 
#' numbers (resulting from function \code{\link{resamplingsByVariable}})
#' @param p The fraction of each sample to be used for model training 
#' (default 0.75)
#' @param use_selector Use the selector variable for splitting the samples into
#' training or testing (default FALSE).
#' @param selector The column name of the selector variable. Only 
#' relevant if use_selector is TRUE.
#' @return A nested list with training and testing samples for each of the n
#' resamplings.
#' 
#' @export compIndexFolds
#' 
#' @details The split into training and testing samples is realized by using 
#' the caret::createDataPartition function which preserves the frequency
#' distribution of the individual response variable(s).
#' 
#' @references  The function uses functions from:
#'  Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, 
#'  Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton Kenkel, 
#'  the R Core Team, Michael Benesty, Reynald Lescarbeau, Andrew Ziem, 
#'  Luca Scrucca, Yuan Tang and Can Candan. (2016). caret: Classification and 
#'  Regression Training. https://CRAN.R-project.org/package=caret
#' 
#' @seealso \code{\link{resamplingsByVariable}} for creating n resamplings from
#' the original dataset.
#' 
#' @examples
#' \dontrun{
#' #Not run
#' }
#' 
compIndexFolds = function(x, selector, nbr = 1){
  
  scat = selector[1]
  snbr = selector[2]
  ucat = unique(x[, scat])
  unbr = unique(x[, snbr])
  min_idx = min(unbr)
  max_idx = max(unbr)
  
  resamples = lapply(seq(min_idx, max_idx), function(c){
    
    actsel = x[x[, snbr] == c, "rowID"]
    # actsel = which(x[, snbr] == c)
    misscat = unique(x[, scat][!x[, scat] %in% x[actsel, scat]])
    out_misscat = unlist(lapply(misscat, function(m){
      set.seed(c)
      sample(x[x[, scat] == m, "rowID"], nbr)
      # sample(which(x[, scat] == m), nbr)
    }))
    testsmpls = c(actsel, out_misscat)
    trainsmpls = which(!seq(nrow(x)) %in% testsmpls)
    return(list(trainsmpls = trainsmpls, testsmpls = testsmpls))
  })
}
