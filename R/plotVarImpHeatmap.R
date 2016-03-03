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
plotVarImpHeatmap <- function(var_imp, xlab = "Variable", ylab = "Method",
                              vis_range = "minmax"){
  temp <- do.call("rbind", var_imp)
  temp$VARIABLE <- factor(temp$VARIABLE, 
                          levels = sort(as.character(unique(temp$VARIABLE))))
  if(vis_range == "minmax"){
    vis_range <- c(min(temp$mean), max(temp$mean))
  }
  clr <- colorRampPalette(brewer.pal(9, "YlOrRd"))
  lattice::levelplot(mean ~ RESPONSE * VARIABLE, data = temp,
            col.regions = clr(101), at = seq(vis_range[1], vis_range[2], length.out = 101),
            asp = 1, as.table = TRUE,
            ylab = ylab, xlab = xlab,
            scales = list(x = list(rot = 45)),
            main = "Variable importance",
            cex.title = 1,
            colorkey = list(space = "top",
                            width = 1, height = 0.75),
            panel=function(...) {
              grid::grid.rect(gp=grid::gpar(col=NA, fill="grey60"))
              panel.levelplot(...)
            })
  
}