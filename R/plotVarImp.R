#' Plot mean variable importance over all model responses
#'
#' @description
#' Plot mean variable importance over all model responses as horizontal bar plot.
#' 
#' @param var_imp The resulting variable importance data rame from 
#' \code{\link{var_imp}}
#' 
#' @return A list of visualizations with one visualization per response variable.
#' 
#' @name plotVarImp
#' 
#' @export plotVarImp
#' 
#' @details NONE
#' 
#' @references  The function uses functions from:
#'  Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, 
#'  Chris Keefer, Allan Engelhardt, Tony Cooper, Zachary Mayer, Brenton Kenkel, 
#'  the R Core Team, Michael Benesty, Reynald Lescarbeau, Andrew Ziem, 
#'  Luca Scrucca, Yuan Tang and Can Candan. (2016). caret: Classification and 
#'  Regression Training. https://CRAN.R-project.org/package=caret
#'
#' @seealso NONE
#'
#' @examples
#' \dontrun{
#' #Not run
#' }
#'
plotVarImp <- function(var_imp){
  lapply(var_imp, function(x){
    plot_var_imp <- data.frame(OVERALL = x$mean)
    rownames(plot_var_imp) <- x$VARIABLE
    
    v_imp_varsel <- list(importance = plot_var_imp,
                         model = "loess r-squared",
                         calledFrom = "varImp")
    class(v_imp_varsel) <- "varImp.train"
    var_imp_plot <- plot(v_imp_varsel, main = as.character(x$RESPONSE[1]))
  })
}