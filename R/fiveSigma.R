# fiveSigma
#
#' Mean +/- 5*sd of a numerical vector
#'
#'     Used for score and ratio cut-off thesholding.
#' @param numVec numerical vector
#' @param greater return(mean + 5sd). Else return(mean - 5sd). Default = T
#' @param dropZero exclude values of zero from the calulation of mean/sd
#' @param verbose prints values
#' @return Numeric
#' @keywords RNAframework rf-modcall
#' @examples
#' fiveSigma( c(10,0,0,15,10) )
#'
#' # Verbose output
#' fiveSigma(c(10,0,0,0,10,15), verbose = T)
#' #"The mean of the vector is: 11.6666666666667"
#' #"The standard deviation of the vector is: 2.88675134594813"
#' #"Five standard deviations from mean is: 26.1004233964073"
#' #26.10042
#' @export
fiveSigma = function(numVec, greater = T, dropZero = T,
                     verbose = F){

  if (greater){

    # Drop zero values from calculation
    # (use only non-zero values)
    if (dropZero){
      numVec = numVec[ !(numVec == 0) ]
    }

    meanVec <- mean(numVec)
    sdVec   <- sd(numVec)

    if (greater){
      # Mean + 5Sigma
      fiveSigma0 <- meanVec + (5 * sdVec)
    }else{
      # Mean - 5Sigma
      fiveSigma0 <- meanVec - (5 * sdVec)
    }

    if (verbose){
      print(paste0('The mean of the vector is: ', meanVec))
      print(paste0('The standard deviation of the vector is: ', sdVec))
      print(paste0('Five standard deviations from mean is: ', fiveSigma0))
    }
    return( fiveSigma0 )
  }
} # end of 5sig
