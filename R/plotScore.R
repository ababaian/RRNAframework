# plotScore
#
# Reads mod.df data.frame and returns a plot
# of Score vs. Ratio
#

plotScore <- function( mod.df, log = T){

  score <- as.numeric(unlist(mod.df$score))
  ratio <- as.numeric(unlist(mod.df$ratio))
  log.score <- log( score, base = 10)
  log.ratio <- log( ratio, base = 10)

  plot.df <- data.frame(score, ratio, log.score, log.ratio)

  if (log){
    # Plot Exp Data
    plot <- ggplot( data = plot.df, aes(log.score, ratio) )
    plot <- plot + geom_hex(bins = 100, aes(fill = log(..count.., base = 10)))
    plot <- plot + theme_bw()
    #plot

  } else {
    # Plot Linear Data
    plot <- ggplot( data = plot.df, aes(score, ratio) )
    plot <- plot + geom_hex(bins = 100, aes(fill = log(..count.., base = 10)))
    plot <- plot + theme_bw()
    #plot

    return(plot)
  }
}
