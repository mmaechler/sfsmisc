\name{signi}
\alias{signi}
\title{Rounding to Significant Digits}
\description{
  Rounds to significant digits similarly to \code{\link{signif}}.
}
\usage{
signi(x, digits = 6)
}
\arguments{
  \item{x}{numeric vector to be rounded.}
  \item{digits}{number of significant digits required.}
}
\value{
  numeric vector ``close'' to \code{x}, i.e. by at least \code{digits}
  significant digits.
}
\author{Martin Maechler, in prehistoric times (i.e. before 1990).}
\note{
  This is really just \code{round(x, digits - trunc(log10(abs(x))))} and
  hence mainly of didactical use.   Rather use \code{signif()} otherwise.
}
\seealso{\code{\link{signif}}, \code{\link{round}}.}
\examples{
(x1 <- seq(-2, 4, by = 0.5))
identical(x1, signi(x1))# since 0.5 is exact in binary arithmetic
(x2 <- pi - 3 + c(-5,-1,0, .1, .2, 1, 10,100))
signi(x2, 3)
}
\keyword{arith}
