% This is also sym.linked into
% Martin's WpDensity package /u/maechler/R/Pkgs/WpDensity/
\name{D1D2}
\alias{D1D2}
\title{Numerical Derivatives of (x,y) Data via Smoothing Splines}
\description{
  Compute numerical derivatives of \eqn{f()} given observations
  \code{(x,y)}, using cubic smoothing splines with GCV, see
  \code{\link[stats]{smooth.spline}}.  In other words, estimate \eqn{f'()}
  and/or \eqn{f''()} for the model
  \deqn{Y_i = f(x_i) + E_i, \ \ i = 1,\dots n,}
}
\usage{
D1D2(x, y, xout = x, spar.offset = 0.1384, deriv = 1:2, spl.spar = NULL)
}
\arguments{
  \item{x,y}{numeric vectors of same length, supposedly from a model
    \code{y ~ f(x)}.}
  \item{xout}{abscissa values at which to evaluate the derivatives.}
  \item{spar.offset}{numeric fudge added to the smoothing parameter,
    see \code{spl.par} below.}
  \item{deriv}{integer in \code{1:2} indicating which
    derivatives are to be computed.}
  \item{spl.spar}{direct smoothing parameter for \code{smooth.spline}.
    If it is \code{NULL} (as per default), the smoothing parameter used
    will be \code{spar.offset + sp$spar}, where \code{sp$spar} is the GCV
  estimated smoothing parameter, see \code{\link{smooth.spline}}.}
}
\details{
  It is well known that for derivative estimation, the optimal smoothing
  parameter is larger (more smoothing) than for the function itself.
  \code{spar.offset} is really just a \emph{fudge} offset added to the
  smoothing parameter. Note that in \R's implementation of
  \code{\link{smooth.spline}}, \code{spar} is really on the
  \eqn{\log\lambda} scale.

  When \code{deriv = 1:2} (as per default), both derivatives are
  estimated with the \emph{same} smoothing parameter which is suboptimal
  for the single functions individually.  Another possibility is to call
  \code{D1D2(*, deriv = k)} twice with \code{k = 1} and \code{k = 2} and
  use a \emph{larger} smoothing parameter for the second derivative.
}
\value{
  a list with several components,
  \item{x}{the abscissae values at which the derivative(s) are evaluated.}
  \item{D1}{if \code{deriv} contains 1, estimated values of
    \eqn{f'(x_i)} where \eqn{x_i} are the values from \code{xout}.}
  \item{D2}{if \code{deriv} contains 2, estimated values of \eqn{f''(x_i)}.}
  \item{spar}{the \bold{s}moothing \bold{par}ameter used in the (final)
    \code{smooth.spline} call.}
  \item{df}{the equivalent \bold{d}egrees of \bold{f}reedom in that
    \code{smooth.spline} call.}
}
\author{Martin Maechler, in 1992 (for S).}
\seealso{\code{\link{D2ss}} which calls \code{smooth.spline} twice,
  first on \code{y}, then on the \eqn{f'(x_i)} values;
  \code{\link[stats]{smooth.spline}} on which it relies completely.
}
\examples{
 set.seed(8840)
 x <- runif(100, 0,10)
 y <- sin(x) + rnorm(100)/4

 op <- par(mfrow = c(2,1))
 plot(x,y)
 lines(ss <- smooth.spline(x,y), col = 4)
 str(ss[c("df", "spar")])
 plot(cos, 0, 10, ylim = c(-1.5,1.5), lwd=2,
      main = expression("Estimating f'() : "~~ frac(d,dx) * sin(x) == cos(x)))
 offs <- c(-0.1, 0, 0.1, 0.2, 0.3)
 i <- 1
 for(off in offs) {
   d12 <- D1D2(x,y, spar.offset = off)
   lines(d12$x, d12$D1, col = i <- i+1)
 }
 legend(2,1.6, c("true cos()",paste("sp.off. = ", format(offs))), lwd=1,
        col = 1:(1+length(offs)), cex = 0.8, bg = NA)
 par(op)
}
\keyword{smooth}
