\name{formatN}
\alias{formatN}
\title{Compact Numeric Formatting}
\description{
  A simple utility getting more compact numeric representations, currently
  for \dQuote{exponential} aka \dQuote{scientific} format.
}
\usage{
formatN(x, digits = 1L)
}
\arguments{
  \item{x}{numeric or complex vector of numbers.}
  \item{digits}{positive integer number of digits to use.}
}
\value{
  a \code{\link{character}} vector of the same length as \code{x}.
}
\author{Martin Maechler}
\seealso{
  \code{\link{format}}, \code{\link{sprintf}}, \code{\link{formatC}}, are or
  can be used similarly.
}
\examples{
(x <- c(outer(c(1,pi), 10^(-10:11))))
sum(nchar(f1 <- format(x, digits=1)))        # 220
sum(nchar(f2 <- sapply(x, format, digits=1)))# 194
sum(nchar(ff <- formatN(x)))                 # 150
noquote(ff)

## the "extreme" formatting still keeps "95%" of numerical info :
stopifnot(all.equal(x, as.numeric(formatN(x)), tolerance = 0.05))
}
\keyword{utilities}
