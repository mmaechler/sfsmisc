\name{ps.latex}
\alias{ps.latex}
\title{Ghostview Device Driver and ``LaTeX Plots'' Utilities}
\usage{
ps.latex(file, height=5 + main.space * 1.25, width=9.5, main.space=FALSE, lab.space=main.space, iso.latin1=is.R(), lab=c(10, 10, 7), mgp.lab=c(1.6, 0.7, 0), mar=c(4, 4, 0.9, 1.1), ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{file}{ ~~Describe \code{file} here~~ }
 \item{height}{ ~~Describe \code{height} here~~ }
 \item{width}{ ~~Describe \code{width} here~~ }
 \item{main.space}{ ~~Describe \code{main.space} here~~ }
 \item{lab.space}{ ~~Describe \code{lab.space} here~~ }
 \item{iso.latin1}{ ~~Describe \code{iso.latin1} here~~ }
 \item{lab}{ ~~Describe \code{lab} here~~ }
 \item{mgp.lab}{ ~~Describe \code{mgp.lab} here~~ }
 \item{mar}{ ~~Describe \code{mar} here~~ }
 \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\description{
 ~~ A concise (1-5 lines) description of what the function does. ~~
}
\details{
 ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of `comp1'}
  \item{comp2 }{Description of `comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
ps.latex("ps.latex-ex.ps", main= TRUE)
data(sunspots)
plot(sunspots,main=paste("Sunspots Data, n=",length(sunspots)),col="red")
ps.end()
}
\keyword{ ~keyword }%-- one or more ...
