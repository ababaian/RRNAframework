# geneCoverage
#' Reads a mod.df object created by \code{\link{readModDir}} with ucov/tcov.
#'
#' @param mod.df A modification data.frame object
#' @param method How is 'coverage' summarized. "mean", "median", "max", "q95". Default: `mean'
#' @param set.na   In log2FC field, coverts all NA and NaN values to <VALUE> [FALSE]. Set to FALSE to leave unchanged.
#' @param set.inf  In log2FC field, coverts all Inf or -Inf values to <VALUE> [FALSE]. Set to FALSE to leave unchanged
#' @return A data.frame of <Gene>, <uCov_summary>, <tCov_summary>
#' @keywords RNAframework rf-modcall
#' @examples
#' geneCov.wt <- geneCoverage( mod.df.wt1, method = "mean")
#' geneCov.ko <- geneCoverage( mod.df.ko1, method = "mean")
#' @export
#'
# Embedded function
coverageSummary <- function( mod.row, method ){

  transcript.id <- mod.row[1]

  if (method == "mean"){
    ucov.sum <- mean(as.numeric( unlist(mod.row[7]) ))
    tcov.sum <- mean(as.numeric( unlist(mod.row[8]) ))
  } else if (method == "median"){
    ucov.sum <- median(as.numeric( unlist(mod.row[7]) ))
    tcov.sum <- median(as.numeric( unlist(mod.row[8]) ))
  } else if (method == "max"){
    ucov.sum <- max(as.numeric( unlist(mod.row[7]) ))
    tcov.sum <- max(as.numeric( unlist(mod.row[8]) ))
  } else if (method == "q95"){
    ucov.sum <- quantile(as.numeric( unlist(mod.row[7]) ), 0.95)
    tcov.sum <- quantile(as.numeric( unlist(mod.row[8]) ), 0.95)
  }

  covSum <- data.frame(gene = as.character(transcript.id),
                       ucov = as.numeric(ucov.sum),
                       tcov = as.numeric(tcov.sum))
  covSum$gene <- as.character(covSum$gene)

  return(covSum)

}

# Main  function
geneCoverage <- function( mod.df,  method = "mean", set.na = NA, set.inf = 10){
  # Returns a data.frame of
  # <Gene Name> <Untreated Coverage Summary> <Treated Coverage Summary>
  #
  geneCov <- apply(mod.df, 1, coverageSummary, method = method )


  geneCov <- data.frame(matrix( unlist(geneCov), ncol = 3, byrow = T))
    colnames(geneCov) <- c('gene','ucov','tcov')
    geneCov$ucov <- as.numeric(as.character(geneCov$ucov))
    geneCov$tcov <- as.numeric(as.character(geneCov$tcov))

  # Log2 ratio of Treated / Untreated
    geneCov$log2Cov <- log2(geneCov$tcov / geneCov$ucov)
    # Note: NA/INF values can be converted to user-defined values with
    # set.na and set.inf respectively
    if ( !isFALSE(set.na) &
         (length(is.nan(geneCov$log2Cov)) > 0)
         ){
      # If set.na == FALSE, do nothing else...
      geneCov$log2Cov[ is.nan(geneCov$log2Cov) ] <- set.na
    }

    if ( !isFALSE(set.inf) &
         (length(is.infinite(geneCov$log2Cov)) > 0)
    ){
      # If set.inf == FALSE, do nothing else...
      signInf <- sign(  geneCov$log2Cov[ is.infinite(geneCov$log2Cov) ] )
      geneCov$log2Cov[ is.infinite(geneCov$log2Cov) ] <- signInf * set.inf
    }

  # Return geneCov data.frame
  return(geneCov)
}
