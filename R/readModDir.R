# readModDir
#
# Reads a directory of rf-modcall XML files
# calls readModXML() to import eachfile
# returns a modcall data.frame for each transcript
#
readModDir <- function(dir,
                      na.asZero = T){
  # Set home diretory where you started
  home = getwd()

  # Check that the mod. directory exists
  #dir <- 'data/prima_pA_wt1_vs_input_pA_wt1_sites/'
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

  return(mod.df)

  setwd(home) # go home loser

} # end of readModDir function

