\name{formatN}
\alias{formatN}
\title{Compact Numeric Formatting}
\description{
  A simple utility getting more compact numeric representations, currently
  for \dQuote{exponential} aka \dQuote{scientific} format.
}
\usage{
formatN(x, digits = 1L, sci = c(-2L, -1L))
}
\arguments{
  \item{x}{numeric or complex vector of numbers.}
  \item{digits}{positive integer number of digits to use.}
  \item{sci}{one or two small integers specifying
    \code{\link{format.default}}'s \code{scientific} argument (see
    \code{\link{options}("scipen")}) for \code{abs(x) < 1} and \eqn{\ge 1}{>= 1}.}
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
sum(nchar(ff1<- formatN(x, sci=-1L)))        # 152
sum(nchar(ff2<- formatN(x, sci=-2L)))        # 150
sum(nchar(ff <- formatN(x)))                 # 150 .. the best
sum(nchar(ff3<- formatN(x, sci=-3L)))        # 152
noquote(rbind(f1, f2, ff1, ff, ff2, ff3))
## the best is ff (where sci differs for neg.exp and pos.ex) - for this example

## the "extreme" formatting still keeps "95%" of numerical info :
stopifnot(all.equal(x, as.numeric(formatN(x)), tolerance = 0.05))
}
\keyword{utilities}
