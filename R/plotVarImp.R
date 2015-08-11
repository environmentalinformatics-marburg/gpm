#' Plot mean variable importance over all model responses
#'
#' @description
#' Plot mean variable importance over all model responses
#' 
#' @param NONE
#'
#' @name plotVarImp
#' 
#' @export plotVarImp
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