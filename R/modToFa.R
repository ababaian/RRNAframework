# modToFa.R
#
# Input is a sig.mod.df object or rand.mod.df object
# of nucleotides.
# Output is a fasta file of the flanking sequence
#
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
}


# # Generate Fasta files of significant modified nucleotides
# #
# modToFa( 'sigNT_wt1.fa',  sig.modNT.wt1)
#
# for (N in 1:2){
#   rand.modNT.N <- sigMod( mod.df.wt1, method = "fiveSigma", flank.seq = 5, randomize = T)
#   modToFa( 'randNT_wt1_n100.fa', rand.modNT.N, overwrite = F)
# }
#
# modToFa( 'randNT_wt1.fa', rand.modNT.wt1)
