\name{frmtNum}
\title{Compact Numeric Formatting}
\alias{frmtNum}
\alias{formatN}% deprecated
\description{
  A simple utility getting more compact numeric representations, currently
  for \dQuote{exponential} aka \dQuote{scientific} format.
}
\usage{% ../R/misc-goodies.R
frmtNum(x, digits = 1L, sci = c(-1L, -2L))

formatN(x, digits = 1L, sci = c(-1L, -2L)) # __ DEPRECATED __
}
\arguments{
  \item{x}{numeric or complex vector of numbers.}
  \item{digits}{positive integer number of digits to use.}
  \item{sci}{one or two small integers specifying
    \code{\link{format.default}}'s \code{scientific} argument (see
    \code{\link{options}("scipen")}) for \code{abs(x) < 1} and \eqn{\ge 1}{>= 1}.
    Note that the default \code{(-2. -1)} seems optimal; this argument
    exists mostly to explore possibilities and to prove optimality of its
    default.}
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
sum(nchar(ff1<- frmtNum(x, sci=-1L)))        # 152
sum(nchar(ff2<- frmtNum(x, sci=-2L)))        # 150
sum(nchar(ff <- frmtNum(x)))                 # 150 .. the best
sum(nchar(ff3<- frmtNum(x, sci=-3L)))        # 152
## deprecation and back compatibility:
tools::assertWarning(ff3.o <- formatN(x, sci=-3L), verbose=TRUE)
stopifnot(identical(ff3.o, ff3))
noquote(rbind(f1, f2, ff1, ff, ff2, ff3))
## the best is ff (where sci differs for neg.exp and pos.ex) - for this example

## the "extreme" formatting still keeps "95%" of numerical info :
stopifnot(all.equal(x, as.numeric(frmtNum(x)), tolerance = 0.05))
}
\keyword{utilities}
