\name{iterate.lin.recursion}
\alias{iterate.lin.recursion}
\title{Generate Sequence Iterating a Linear Recursion}
\description{
  Generate numeric sequences applying a linear recursion \code{nr.it} times.
}
\usage{
iterate.lin.recursion(x, coeff, delta = 0, nr.it)
}
\arguments{
  \item{x}{numeric vector with \emph{initial values}, i.e., specifying
    the beginning of the resulting sequence; must be of length (larger
    or) equal to \code{length(coeff)}.}
  \item{coeff}{coefficient vector of the linear recursion.}
  \item{delta}{numeric scalar added to each term; defaults to 0.  If not
    zero, determines the linear drift component.}
  \item{nr.it}{integer, number of iterations.}
}
\value{
  numeric vector, say \code{r}, of length \code{n + nr.it}, where
  \code{n = length(x)}. Initialized as \code{r[1:n] = x}, the recursion
  is \code{r[k+1] = sum(coeff * r[(k-m+1):k])}, where \code{m = length(coeff)}.
}
\note{
  Depending on the zeroes of the characteristic polynomial of \code{coeff},
  there are three cases, of convergence, oszillation and divergence.
}
\author{Martin Maechler}
\seealso{\code{\link{seq}} can be regarded as a trivial special case.}
\examples{
## The Fibonacci sequence:
iterate.lin.recursion(0:1, c(1,1), nr = 12)
## 0 1 1 2 3 5 8 13 21 34 55 89 144 233

## seq() as a special case:
stopifnot(iterate.lin.recursion(4,1, d=2, nr=20)
          == seq(4, by=2, length=1+20))

## ''Deterministic AR(2)'' :
round(iterate.lin.recursion(1:4, c(-0.7, 0.9), d = 2, nr=15), dig=3)
## slowly decaying :
plot(ts(iterate.lin.recursion(1:4, c(-0.9, 0.95), nr=150)))
}
\keyword{arith}
