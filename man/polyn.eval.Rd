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
  \item{coef}{\dQuote{numeric} vector or matrix.  If a vector, \code{x} can be an
    array and the result matches \code{x}.\cr
    If \code{coef} is a matrix it specifies several polynomials of the
    same degree as rows, \code{x} must be a vector, \code{coef[,k]} is
    for \eqn{x^{k-1}}{x^(k-1)} and the result
    is a matrix of dimension \code{length(x) * nrow(coef)}.

    Note that \code{coef} can also be \code{\link{complex}} or bigrational
    (\code{as.bigq(.)} from \CRANpkg{gmp}, or arbitrary precision
    (\code{"mpfr"}) from \CRANpkg{Rmpfr}, or similar number-like objects
    for which basic arithmetic is defined.
}
  \item{x}{\dQuote{numeric} vector or array.  Either \code{x} or \code{coef} must
    be a vector.}

}
\details{
  The stable \dQuote{Horner rule} is used for evaluation in any case.

  When \code{length(coef) == 1L}, \code{polyn.eval(coef, x)} now returns a
  vector of \code{length(x)} whereas previously, it just gave the number
  \code{coef} independent of \code{x}.
}
\value{
  numeric vector or array, depending on input dimensionalities, see above.
}
\author{Martin Maechler, ages ago.}
\seealso{For much more sophisticated handling of polynomials, use the
  \CRANpkg{polynom} package, see, e.g., \code{\link[polynom]{predict.polynomial}}.
  For multivariate polynomials (and also for nice interface to the
  \CRANpkg{orthopolynom} package), consider the \CRANpkg{mpoly} package.
}
\examples{
polyn.eval(c(1,-2,1), x = 0:3)# (x - 1)^2
polyn.eval(c(0, 24, -50, 35, -10, 1), x = matrix(0:5, 2,3))# 5 zeros!
(cf <- rbind(diag(3), c(1,-2,1)))
polyn.eval(cf, 0:5)

x  <- seq(-3,7, by=1/4)
cf <- 4:1
(px <- polyn.eval(cf, x)) # is exact
if((gmpT <-"package:gmp" \%in\% search()) || require("gmp")) withAutoprint({
 pxq <- polyn.eval(coef = as.bigq(cf, 1), x=x)
 pxq
 stopifnot(pxq == px)
 if(!gmpT) detach("package:gmp")
})

if((RmpfrT <-"package:Rmpfr" \%in\% search()) || require("Rmpfr")) withAutoprint({
 pxM <- polyn.eval(coef = mpfr(cf, 80), x=x) # 80 bits accuracy
 pxM
 stopifnot(pxM == px)
 if(!RmpfrT) detach("package:Rmpfr")
})

stopifnot(identical(polyn.eval(12, x),      rep(12, length(x))),
          identical(polyn.eval(7, diag(3)), matrix(7, 3,3)))
}
\keyword{arith}
