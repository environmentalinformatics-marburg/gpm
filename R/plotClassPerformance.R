#' Plot performance of a classification model
#'
#' @description
#' Plot performance of a classification model
#' 
#' @param NONE
#'
#' @name plotClassPerformance
#' 
#' @export plotClassPerformance
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
plotClassPerformance <- function(modstat, mean = FALSE, 
                                 scores = c("Kappa", "ETS")){
  
  scores_mean <- paste0(scores, "_mean")
  
  df_mean <- modstat[[1]]
  
  df_mean$Response <- factor(
    df_mean$Response,
    levels = df_mean$Response[order(
      df_mean$Kappa_mean, decreasing = FALSE)])
  
  df_mean_melt <- melt(df_mean, id.vars = "Response")
  
  if(mean == TRUE){
    cp <- ggplot(data = df_mean_melt[df_mean_melt$variable %in% scores_mean,], 
                 aes(x = Response, y = value, color = variable, group = variable)) + 
      geom_point() + geom_line() + coord_flip()
  } else {
    
    df <- do.call("rbind", modstat[[2]])
    
    df$Response <- factor(
      df$Response,
      levels(df$Response)[order(
        df_mean$Kappa_mean, decreasing = FALSE)])
    df_melt <- melt(df, id.vars = "Response")
    
    indv <- nrow(df[df_melt$Response == unique(df_melt$Response)[[1]] & 
                      df_melt$variable == unique(df_melt$variable)[[1]],])
    
    df_melt$variable_id <- paste0(df_melt$variable, "_", seq(indv))
    
    
    cp <- ggplot(data = df_melt[df_melt$variable %in% scores,], 
                 aes(x = Response, y = value, colour = variable, group = variable_id)) + 
      geom_line(linetype="twodash", alpha = 0.25) + 
      geom_point(data = df_mean_melt[df_mean_melt$variable %in% scores_mean,], 
                 aes(x = Response, y = value, colour = variable, group = variable)) + 
      geom_line(data = df_mean_melt[df_mean_melt$variable %in% scores_mean,], 
                aes(x = Response, y = value, colour = variable, group = variable),
                size = 1.5) + coord_flip()
  }
  return(cp)
}