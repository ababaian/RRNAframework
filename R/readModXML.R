# readModXML
#' Reads a rf-modcall XML files
#'
#'
#' @param file XML file to read
#' @param na.asZero Convert NA in rf-modcall (score and ratio) to zreo. Default: T
#' @return List object for each transcript containing scores/sequence for Transcript
#' @keywords RNAframework rf-modcall
#' @examples
#' readModXML(file = 'rf-mod.experiment/PTEN.xml')
#' The length of a string (in characters).
#' @seealso \code{\link{readModDir} Wraps thus function across an entire directory

readModXML <- function(file,
                       na.asZero = T ){

  # Ensure input XML file exists
  if ( !file.exists(file) ){
    stop( paste0('Input file: ',file,' not found'))
  }

  #file <- 'data/prima_pA_wt1_vs_input_pA_wt1_sites/HNRNPA1.xml'
  #na.asZero = T

  # Read the rf-modcall XML file as a raw file
  rawXML <- readChar(file, file.info(file)$size)

  # read 'tool' parameter, require that it be rf-modcall for this script
  tool <- transcript.id <- sub( "\" .*","", sub('.* tool=\"', "", rawXML))

  # Ensure that XML file is of the type "rf-modcall"
  if (tool == 'rf-modcall'){
    # Read transcript id, length and windowsize for this row (entry)
    transcript.id <- sub( "\" .*","", sub('.*<transcript id=\"', "", rawXML))
    length.seq    <- sub( '\".*', '', sub('.* length=\"', "", rawXML))
    win.size      <- sub( '\".*', '', sub('.* win=\"', "", rawXML))

    # Read/parse the nucleotide sequence as a string
    sequence <- sub( "</sequence>.*", "", sub('.*<sequence>', "", rawXML))
    sequence <- gsub( "\\n", "", sequence)
    sequence <- gsub( "\\t", "", sequence)

    # Read the score for each nucleotide (no way to detect what type of
    # calculation was done here at the moment. TODO Autodetect algorithm)
    score <- sub( "</score>.*", "", sub('.*<score>', "", rawXML))
    score <- gsub( "\\n", "", score)
    score <- gsub( "\\t", "", score)
    score <- as.numeric( unlist( strsplit( score, split = ',' ) ) )

    # Read the ratio score for each nucleotide ( fraction RT stop )
    ratio <- sub( "</ratio>.*", "", sub('.*<ratio>', "", rawXML))
    ratio <- gsub( "\\n", "", ratio)
    ratio <- gsub( "\\t", "", ratio)
    ratio <- as.numeric( unlist( strsplit( ratio, split = ',' ) ) )

    # The score/ratio contains NaN, convert them to 0 if na.asZero set
    if (na.asZero){
      score[ is.na(score) ] <- 0
      ratio[ is.na(ratio) ] <- 0
    } # end of na.asZero

    # End of data import

  } else {
    # Stop function if non rf-modcall XML file is input
    stop( 'readModXML reads "rf-modcall" output XML files only.
        Check that "tool=rf-modcall" is set in the XML input.')
  }

  # Parse the imported XML data into a row/data.frame entry
  modcall <- data.frame(transcript.id)
  modcall$len     <- as.numeric(length.seq)
  modcall$window   <- as.numeric(win.size)
  modcall$sequence <- sequence
  modcall$score    <- list(score)
  modcall$ratio    <- list(ratio)

  return(modcall)

} # end of readModXML function
