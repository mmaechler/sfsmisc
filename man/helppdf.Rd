\name{helppdf}
\alias{helppdf}
\title{help() Type="pdf" and View It}
\description{
  Utility to view PDF-rendered \code{\link{help}} pages; particularly
  useful in case they contain mathematical formulas or otherwise
  sophisticated formats.
}
\usage{
helppdf(topic, viewer = getOption("pdfviewer"), quiet = !interactive(), ...)
}
\arguments{
  \item{topic}{the topic, passed to \code{\link{help}()}.}
  \item{viewer}{a pdf viewer; the default is typically what you want interactively.}
  \item{quiet}{\code{\link{logical}} indicating that nothing should be
    printed to the console and the result should be returned as
    \code{\link{invisible}()}.}
  \item{\dots}{further optional arguments passed to \code{help()}.}
}
\value{
  Returns the full path of the pdf file produced.
}
\author{Martin Maechler}
\seealso{
  \code{\link{help}}, \code{\link{system}}.
}
\examples{
if(interactive()) {
  ## Both calls work :
  helppdf(Normal)
  helppdf("NegBinomial")
} else if(.Platform$OS.type != "windows") { # batch mode (Windows often too slow for this)
  od <- setwd(tempdir())
  ff <- helppdf(Normal, viewer=NULL)
  stopifnot(file.exists(ff)) ; print(ff)
  setwd(od)# revert to previous dir.
}
}
\keyword{utilities}
\keyword{print}
