\name{nr.sign.chg}
\alias{nr.sign.chg}
\title{Number of Sign Changes in Sequence}
\description{
  Compute the number of sign changes in the sequence \code{y}.
}
\usage{
nr.sign.chg(y)
}
\arguments{
  \item{y}{numeric vector.}
}
\value{
  an integer giving the number of sign changes in sequence \code{y}.
  Note that going from positive to 0 to positive is \emph{not} a sign change.
}
\author{Martin Maechler, 17 Feb 1993.}
\examples{
(y <- c(1:2,1:-1,0:-2))
nr.sign.chg(y)## = 1
}
\keyword{arith}
