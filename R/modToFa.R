# modToFa
#' Write a Fasta file for each flank.seq in a nucleotide.modification data.frame
#' @param file fasta file to write to
#' @param modNT.df nucleotide.modification data.frame (either significant or control)
#' @param overwrite if file exists, delete it and overwrite. Else append to file. Default = T
#' @return NULL writes to file
#' @keywords RNAframework rf-modcall
#' @examples
#' modToFa('RTstop.motif.fa', RTstop.wt1)
#' # Create a background set x100 for motif analysis by HOMER
#'
#' for (N in 1:100){
#' rand.modNT.N <- sigMod( RTstop.wt1, method = "fiveSigma", flank.seq = 5, randomize = T)
#'   modToFa( 'BG.motif.fa', rand.modNT.N, overwrite = F)
#' }
#' @export
modToFa <- function(file, modNT.df, overwrite = T){
  # check if file exists and print warning

  if ( file.exists(file) & overwrite ) {
    print('Warning file exists, will overwrite. (Ahhh!)')
    file.remove(file)
  }

  read.nt <- function(inRow, file){
    # Create header for fasta entry
    # >[Transcript Name]_[nt][position of nt]_[score]_[ratio]

    #print(inRow)
    #inRow = data.frame(inRow) # force into df for selecting

    #print(inRow[['nt.id']])

    fa.header <- paste0('>',inRow[['nt.id']],
                        '_', inRow[['nt.seq']], inRow[['nt.pos']],
                        "_",inRow[['nt.score']],
                        "_",inRow[['nt.ratio']])
    fa.seq    <- inRow[['nt.flank']]

    fa.entry  <- paste0(fa.header,'\n',fa.seq)

    #print(fa.entry)
    #return(fa.entry)
    write.table( fa.entry, file = file, append = T, quote = F, row.names = F, col.names = F)
  }

  apply(modNT.df, 1, read.nt, file = file )

} # End of modToFa
