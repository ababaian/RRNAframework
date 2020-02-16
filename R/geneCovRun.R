# geneCovRun
#' Reads a mod.df object created by \code{\link{readModDir}} with ucov/tcov for coverage run
#'
#' @param mod.df A modification data.frame object
#' @param threshold The number of reads in coverage required for a run count to start
#'
#' @return The maximum length (nt) of consecutive bases with coverage greater then threshold
#' @keywords RNAframework rf-modcall
#' @examples
#' geneCov.wt <- geneCovRun( mod.df.wt1, theshold = 5 )
#' @export
#'
# Embedded function
covrunSummary <- function( mod.row, threshold = 0, col.select = 8){

  #print( as.character(mod.row[1]) )
  # Encode coverage as binary RLE
  covRLE <- rle( unlist(mod.row[col.select]) >= threshold )

  if (any(covRLE$values)){
    # A run of coverage above threshold was found
    maxCovRun <- max(covRLE$lengths[ which(covRLE$values) ])
  } else {
    maxCovRun <- 0
  }
  return(maxCovRun)
}

# Main  function
geneCovRun <- function( mod.df, threshold = 1 , col.select = "tcov"){
  # Return a numeric of consecutive bases with coverage > threshold

  # Which column (tcov or ucov) to use
  if (col.select != "tcov" & col.select != "ucov"){
    stop("col.select not found; use 'tcov' or 'ucov'")
  }

  col.select <- which( colnames(mod.df) == col.select )


  geneCov <- apply(mod.df, 1, covrunSummary, threshold = threshold, col.select = col.select)

  return(geneCov)
}
