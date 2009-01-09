\name{unif}
\alias{unif}
\title{Nice Uniform Points in Interval}
\description{
  Give regularly spaced points on interval \eqn{[-c,c]} with mean 0
  (exactly) and variance about 1 (very close for \bold{even} \code{n}
  and larger \code{round.dig}).  Note that \eqn{c} depends on \code{n}.
}
\usage{
unif(n, round.dig = 1 + trunc(log10(n)))
}
\arguments{
  \item{n}{positive integer specifying the number of points desired.}
  \item{round.dig}{integer indicating to how many digits the result is
    rounded.}
}
\value{
  numeric vector of length \code{n}, symmetric around 0, hence
  with exact mean \code{0}, and variance approximately 1.
}
\note{It relies on the fact that \eqn{Var(1,2,...,n) = n(n+1)/12}.
}
\author{Martin Maechler, ca 1990}
\seealso{\code{\link{runif}} for producing uniform \emph{random} numbers.}
\examples{
(u <- unif(8))
var(u)

(u. <- unif(8, 12))# more digits in result, hence precision for Var :
var(u.)
}
\keyword{arith}
\keyword{utilities}
