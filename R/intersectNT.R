# intersectNT
#' Find overlap between two modNT.df objects
#'
#' @param mod.df1 First modification data.frame object
#' @param mod.df2 Second modification data.frame object
#'
#' @return A list describing the intersection between mod.df1 and mod.df2
#' @keywords RNAframework rf-modcall
#' @examples
#' RTstop.wt1 <- sigMod( mod.df.wt1, method = "fiveSigma", flank.seq = 5)
#' RTstop.bg.wg1 <- sigMod( mod.df.wt1, method = "fiveSigma", flank.seq = 5, randomize = T)
#' @export
intersectNT <- function(nt.df1, nt.df2){

  # Create a character vector of <transcript>-<position>
    select.col <- which( colnames(nt.df1) %in% c("nt.id", "nt.pos") )
  NT1 <- do.call( paste, nt.df1[ , select.col ] )
  NT2 <- do.call( paste, nt.df2[ , select.col ] )

  oneintwo <- list(which(NT1 %in% NT2))
  twoinone <- list(which(NT2 %in% NT1))

  count <- length(oneintwo[[1]]) # number of overlap

  out.list <- list(count = count, oneintwo = oneintwo, twoinone = twoinone)
  return(out.list)

}
