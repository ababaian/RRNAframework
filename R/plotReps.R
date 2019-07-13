# plotReps.R
#
# Reads mod.df data.frame and returns a plot
# of Score vs. Ratio
#

# Requires ggplot2
library(ggplot2)

plotReps <- function( mod.df1, mod.df2, log = T){

  OneinTwo <- (mod.df1$transcript.id %in% mod.df2$transcript.id)
  TwoinOne <- (mod.df2$transcript.id %in% mod.df1$transcript.id)

  # Check that transcript order in rep1 == transcript order in rep2
  trans1 <- mod.df1$transcript.id[ OneinTwo ]
  trans2 <- mod.df2$transcript.id[ TwoinOne ]

  if ( length(which(trans1 != trans2)) > 0 ){
    stop('Transcript order not equal in mod.df1 != mod.df2. TODO auto fix this')
  }

  score1     <- as.numeric(unlist(mod.df1$score[OneinTwo]))
  log.score1 <- log( score1, base = 10)
  fiveSigma1 <- fiveSigma( score1 )


  score2     <- as.numeric(unlist(mod.df2$score[TwoinOne]))
  log.score2 <- log( score2, base = 10)
  fiveSigma2 <- fiveSigma( score2 )

  plot.df <- data.frame(score1, score2, log.score1, log.score2)

  if (log){
    # Plot Exp Data

    minXY <- min(log.score1[ !is.infinite(log.score1) ], log.score2[ !is.infinite(log.score2) ])
    maxXY <- max(log.score1[ !is.infinite(log.score1) ], log.score2[ !is.infinite(log.score2) ])

    plot <- ggplot( data = plot.df, aes(log.score1, log.score2) )
    plot <- plot + geom_hex(bins = 100, aes(fill = log(..count.., base = 10)))
    plot <- plot + theme_bw()
    plot <- plot + xlim(values = c(minXY, maxXY))
    plot <- plot + ylim(values = c(minXY, maxXY))

    # X = Y line
    plot <- plot + geom_abline(slope = 1, intercept = 0, color = 'black')
    # 5 sigma from mean
    plot <- plot + geom_vline( xintercept = log(fiveSigma1, 10), color = 'red' )
    plot <- plot + geom_hline( yintercept = log(fiveSigma2, 10), color = 'red' )

    plot

  } else {
    # Plot Linear Data

    minXY <- min(score1, score2)
    maxXY <- max(score1, score2)

    plot <- ggplot( data = plot.df, aes(score1, score2) )
    plot <- plot + geom_hex(bins = 100, aes(fill = log(..count.., base = 10)))
    plot <- plot + theme_bw()
    plot <- plot + xlim(values = c(minXY, maxXY))
    plot <- plot + ylim(values = c(minXY, maxXY))

    # X = Y line
    plot <- plot + geom_abline(slope = 1, intercept = 0, color = 'black')

    # 5 Sigma from mean line
    plot <- plot + geom_vline( xintercept = fiveSigma1, color = 'red' )
    plot <- plot + geom_hline( yintercept = fiveSigma2, color = 'red')

    plot

    return(plot)
  }
}

