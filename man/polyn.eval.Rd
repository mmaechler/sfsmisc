\name{polyn.eval}
\alias{polyn.eval}
\title{Evaluate Polynomials}
\description{
  Evaluate one or several univariate polynomials at several locations,
  i.e. compute \code{coef[1] + coef[2]*x + ... + coef[p+1]* x^p}
  (in the simplest case where \code{x} is scalar and \code{coef} a vector).
}
\usage{
polyn.eval(coef, x)
}
\arguments{
  \item{coef}{numeric vector or matrix.  If a vector, \code{x} can be an
    array and the result matches \code{x}.\cr
    If \code{coef} is a matrix it specifies several polynomials of the
    same degree as rows, \code{x} must be a vector, \code{coef[,k]} is
    for \eqn{x^{k-1}}{x^(k-1)} and the result
    is a matrix of dimension \code{length(x) * nrow(coef)}.}
  \item{x}{numeric vector or array.  Either \code{x} or \code{coef} must
    be a vector.}
}
\details{
  The stable ``Horner rule'' is used for evaluation in any case.
}
\value{
  numeric vector or array, depending on input dimensionalities, see above.
}
\author{Martin Maechler, ages ago.}
\seealso{For much more sophisticated handling of polynomials, use the
  \code{polynom} package, e.g. \code{\link[polynom]{predict.polynomial}}.
}
\examples{
polyn.eval(c(1,-2,1), x = 0:3)# (x - 1)^2
polyn.eval(c(0, 24, -50, 35, -10, 1), x = matrix(0:5, 2,3))# 5 zeros!
(cf <- rbind(diag(3), c(1,-2,1)))
polyn.eval(cf, 0:5)
}
\keyword{arith}
