#  readPeakBed
#' Reads an output bed files from "rf-peakcall"
#'
#' @param inbed  Bed file input generated from "rf-peakcall"
#' @param as.GRanges Returns the bed file as a "GRanges object"
#' @return Returns a bed file as as data.frame or GRanges object
#' @keywords RNAframework rf-peakcall
#' @examples
#' wt1.bed <- readPeakBed(bed.path = "primaseq/data/wt1.bed")
#' @export
readPeakBed <- function(bed.path, as.GRanges = F){

  bed <-read.table(file = bed.path)

  if (ncol(bed) == 5){
    # bed file is a peak file
    colnames(bed) <- c("transcriptID", "start", "end", "enrichment", "p-adj")

    # Return object as GenomicRanges object if specified
    if (as.GRanges){
      bed   <- GenomicRanges::GRanges(
        seqnames = bed$transcriptID,
        ranges   = IRanges(start = bed$start, end   = bed$end),
        strand   = "+",
        score    = bed$enrichment,
        pval     = bed$`p-adj`)
    }
  } else if (ncol(bed) == 3){
    # bed file is a summit file
    colnames(bed) <- c("transcriptID", "start", "end")

    # Return object as GenomicRanges object if specified
    if (as.GRanges){
      bed   <- GenomicRanges::GRanges(
        seqnames = bed$transcriptID,
        ranges   = IRanges(start = bed$start, end   = bed$end),
        strand   = "+")
    }
  } else {
    # I am uncertain what kind of bed file was input
    stop("Bed file does not contain 5 (peak) or 3 (summit) columns.
          Import Failed.")
  }

  return(bed)

}
