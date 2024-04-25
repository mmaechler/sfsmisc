\name{integrate.xy}
\alias{integrate.xy}
\title{Cheap Numerical Integration through Data Points}
\description{
  Given \eqn{(x_i, f_i)} where \eqn{f_i = f(x_i)}, compute a cheap
  approximation of \eqn{\int_a^b f(x) dx}{integral(a .. b) f(x) dx}.
}
\usage{
integrate.xy(x, fx, a, b, use.spline=TRUE, xtol=2e-08)
}
\arguments{
  \item{x}{abscissa values.}
  \item{fx}{corresponding values of \eqn{f(x)}.}
  \item{a,b}{the boundaries of integration; these default to min(x) and
    max(x) respectively.}
  \item{use.spline}{logical; if TRUE use an interpolating spline.}
  \item{xtol}{tolerance factor, typically around
    \code{sqrt(.Machine$double.eps)} ......(fixme)....}
}
\details{
  Note that this is really not good for noisy \code{fx} values;
  probably a smoothing spline should be used in that case.

  Also, we are not yet using Romberg in order to improve the trapezoid
  rule.  This would be quite an improvement in equidistant cases.
}
\value{
  the approximate integral.
}
\author{Martin Maechler, May 1994 (for S).}
\seealso{\code{\link{integrate}} for numerical integration of
  \emph{functions}.}
\examples{
 x <- 1:4
 integrate.xy(x, exp(x))
 print(exp(4) - exp(1), digits = 10) # the true integral

 for(n in c(10, 20,50,100, 200)) {
   x <- seq(1,4, len = n)
   cat(formatC(n,wid=4), formatC(integrate.xy(x, exp(x)), dig = 9),"\n")
 }
}
\keyword{math}
\keyword{utilities}
