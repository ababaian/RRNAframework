% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readModDir.R
\name{readModDir}
\alias{readModDir}
\title{Reads a directory of rf-modcall XML files
wrapper for readModXML}
\usage{
readModDir(dir, na.asZero = T)
}
\arguments{
\item{dir}{directory containing rf-modcall XML files}

\item{na.asZero}{Convert NA in rf-modcall (score and ratio) to zreo. Default: T}
}
\value{
mod.df object. A modification data.frame where each row is a transcript in dir
}
\description{
Reads a directory of rf-modcall XML files
wrapper for readModXML
}
\examples{
readModDir(dir = './data/rf-mod.experiment/')
}
\seealso{
\code{\link{readModXML}} Wraps this function
}
\keyword{RNAframework}
\keyword{rf-modcall}
