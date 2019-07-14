# readModDir
#' Reads a directory of rf-modcall XML files
#' wrapper for readModXML
#'
#' @param dir directory containing rf-modcall XML files
#' @param na.asZero Convert NA in rf-modcall (score and ratio) to zreo. Default: T
#' @return mod.df object. A modification data.frame where each row is a transcript in dir
#' @keywords RNAframework rf-modcall
#' @examples
#' readModDir(dir = './data/rf-mod.experiment/')
#' @seealso \code{\link{readModXML}} Wraps this function
#' @export
readModDir <- function(dir,
                      na.asZero = T){
  # Set home diretory where you started
  home = getwd()

  # Check that the mod. directory exists
  if ( !file.exists(dir) ){
    stop( "dir directory does not exist.")
  }

  # Move to modification directory
  setwd(dir)

  # List of XML files in dir
  xmlFiles <- list.files()
  xmlFiles <- xmlFiles[ grep('.xml', xmlFiles) ]

  # Check that the input directory contains XML files to read
  if ( length(xmlFiles) == 0 ){
    # No xml files detected in dir
    setwd(home) # go home
    stop( 'dir exists but does not contain any XML files')
  }

  mod.df <- sapply( xmlFiles, FUN = readModXML, simplify = T)
  mod.df <- data.frame(t(mod.df))

  # Parse the mod.df output into a simplier data.frame
  mod.df$transcript.id <- as.character( unlist(mod.df$transcript.id) )
  mod.df$len           <- as.numeric( unlist(mod.df$len) )
  mod.df$window        <- as.numeric( unlist(mod.df$window) )
  mod.df$sequence      <- as.character( unlist(mod.df$sequence) )
  # score as list
  # ratio as list

  setwd(home) # go home loser

  return(mod.df)

} # end of readModDir function
