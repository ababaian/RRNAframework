# plotScore
#' Plot box-plot for Score/Ratio of each nt in mod.df
#'
#' @param mod.df modification data.frame object
#' @param add.jitter Plot log(score, base = 10) instead of linear. Default = T
#' @param log.scale Plot log(score, base = 10) instead of linear. Default = T
#' @return ggplot2 object
#' @keywords RNAframework rf-modcall
#' @examples
#' plot <- plotScoreRatio_bar(RTstop.wt1, add.jitter = F)
#' plot <- plot + xlab('log(Score / RT stop score)')
#' plot
#'
#' @seealso \code{\link{geom_boxplot}}
#' @export
plotScoreRatio_Bar <- function( mod.df, add.jitter = T, log.scale = T){
  # Calculate plottable Score/Ratio
  score <- as.numeric(unlist(mod.df$score))
  ratio <- as.numeric(unlist(mod.df$ratio))
  scoreratio <- score/ratio
    # Remove entries where ratio = 0 (INF / NaN)
    scoreratio[is.infinite(scoreratio)] <- NA
    scoreratio[is.nan(scoreratio)]      <- NA
    scoreratio <- scoreratio[!is.na(scoreratio)]
    # Remove entries where score = 0
    scoreratio <- scoreratio[scoreratio != 0]

  plot.df <- data.frame(scoreratio)

  # Plot Jitter+Boxplot OR Boxplot of score/ratio
  plot <- ggplot( data = plot.df, aes(x = '', y = scoreratio) )

  if (add.jitter){
    plot <- plot + geom_jitter(color = 'gray20', alpha = 0.2, width = 0.25) +
      geom_boxplot(outlier.shape = NA)
  } else {
    plot <- plot + geom_boxplot()
  }

  plot <- plot + theme_bw() + ylab("Score / Ratio")

  if (log.scale){ plot <- plot + scale_y_log10()}

  return(plot)

}
