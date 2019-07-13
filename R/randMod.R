# randMod.R
#
# Reads a mod.df object created by readModDir()
# select N random positions from the data
# to use as a control 'background' for the expressed transcripts
#
# Returns a sig.mod.df object of random nucleotides
#

randMod <- function( mod.df, flank.seq = 5){
  # Return a sig.mod.df object
  # "Significance can be defined in two ways
  #
  # 1) Return positions which are 5 sigma greater than mean
  #
  # 2) Use manual cutoff thresholds
  #

  # Define Cut-offs
  if (method == "fiveSigma"){
    score.cutoff <- fiveSigma( as.numeric(unlist(mod.df$score)) )
    ratio.cutoff <- fiveSigma( as.numeric(unlist(mod.df$ratio)) )
  }else{
    # Requires a score.cutoff and ratio.cutoff
    if ( exists("score.cutff") & exists("ratio.cutoff")){
      # =D Have a nice day =D
    }else{
      stop("Manual (non-FiveSigma) method selected.
           Must define a score.cutoff and ratio.cutoff")
    }
  } # end cut-offs

  # For each transcript (row), find significant RT-stop nucleotides
  # return transcript.id, nt, score, ratio, flank.seq
  #
  # use sigNT function, apply to each transcript returning significant positions
  #
  sigNT <- function( inRow, score.cutoff, ratio.cutoff, flank.seq ){

    inRow = data.frame(inRow)
    colnames(inRow) <- c("transcript.id", "len", "window", "sequence", "score", "ratio")
    #print( paste0( 'Testing: ', inRow$transcript.id ))

    # In transcript, test each nt for score/ratio cutoff
    score <- as.numeric( unlist( inRow$score ))
    ratio <- as.numeric( unlist( inRow$ratio ))

    sig.nt <- which(score >= score.cutoff &
                      ratio >= ratio.cutoff)

    if ( length(sig.nt) != 0 ){
      # A significant nucleotide was found
      for (nt in sig.nt){
        nt.id    <- inRow$transcript.id
        nt.score <- score[nt]
        nt.ratio <- ratio[nt]
        nt.seq   <- substring( inRow$sequence, nt, nt )
        nt.flank <- substring( inRow$sequence, max(nt - flank.seq, 0), min(inRow$length, nt + flank.seq) )

        nt.df = data.frame(nt.id, nt.score, nt.ratio, nt.seq, nt.flank)
        #return(nt.df)

        if ( exists( 'out.df' )){
          # output matrix exists
          out.df <- rbind( out.df, nt.df )

        } else {
          # initialize output matrix
          out.df <- nt.df
        }

        # Verbose for testing
        # print( paste('Hit: ',
        #              inRow$transcript.id, score[nt], ratio[nt], nt,
        #              substring(inRow$sequence, nt, nt),
        #              substring(inRow$sequence, nt - flank.seq, nt + flank.seq)) )

      }
      return(out.df)
    } else{
      # No significant nucleotides found, return nothing
    }
  } # End sigNT function

  # Wrangle the data frame into a list :'(
  # to use lapply (for multiple output return)
  mod.list <- split(mod.df, seq(nrow(mod.df)))

  # Lapply the sigNT function (per transcript analysis) across the mod.df
  # Remove empty list elements
  sig.list <- lapply(mod.list, sigNT, score.cutoff = score.cutoff, ratio.cutoff = ratio.cutoff, flank.seq = 10)
  sig.list <- sig.list[ -which(sapply(sig.list, length) == 0) ]

  # Convert back to data.frame
  sig.df <- as.data.frame( do.call("rbind", sig.list))

  print( "Significant reactive nucleotides found." )
  print( paste0(' ', length(sig.df$nt.id), " reactive nucleotides"))
  print( table(sig.df$nt.seq))
  print( paste0(' in ', length(unique(sig.df$nt.id)), " out of ", length(unique(mod.df$transcript.id)), " total transcripts"))

  # Return the significant nucleotides in data.set
  return(sig.df)

}

# End of script


# #  Test data
# # load('mod.df.RData')
#
# sig.modNT.wt1 <- sigMod( mod.df.wt1, method = "fiveSigma", flank.seq = 5)
# sig.modNT.wt1 <- sig.modNT.wt1[order( sig.modNT.wt1$nt.score, decreasing = T), ]
#
# sig.modNT.wt2 <- sigMod( mod.df.wt2, method = "fiveSigma", flank.seq = 5)
# sig.modNT.wt2 <- sig.modNT[order( sig.modNT.wt2$nt.score, decreasing = T), ]
