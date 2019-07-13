# fiveSigma.R
#
# Input vector of numbers
# Prints the mean and standard deviation
# Returns five sigma greater/lesser than the mean
#

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
}
