#' Plot mean variable importance over all model responses as heatmap
#'
#' @description
#' Plot mean variable importance over all model responses as heatmap
#' 
#' @param NONE
#'
#' @name plotVarImpHeatmap
#' 
#' @export plotVarImpHeatmap
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
plotVarImpHeatmap <- function(var_imp, xlab = "Variable", ylab = "Method"){
  clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
  temp <- do.call("rbind", var_imp)
  temp$VARIABLE <- factor(temp$VARIABLE, 
                          levels = sort(as.character(unique(temp$VARIABLE))))
  levelplot(mean ~ RESPONSE * VARIABLE, data = temp,
            col.regions = clr(101), at = seq(0, 100, 1),
            asp = 1, as.table = TRUE,
            ylab = ylab, xlab = xlab,
            scales = list(x = list(rot = 45)),
            main = "Variable importance",
            cex.title = 1,
            colorkey = list(space = "top",
                            width = 1, height = 0.75),
            panel=function(...) {
              grid.rect(gp=gpar(col=NA, fill="grey60"))
              panel.levelplot(...)
            })
  
}