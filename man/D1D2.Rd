\name{D1D2}
\alias{D1D2}
\title{Numerical Derivatives of (x,y) Data via Smoothing Splines}
\description{
  Compute numerical derivatives of \eqn{f()} given observations
  \code{(x,y)}, using cubic smoothing splines with GCV, see
  \code{\link{smooth.spline}}.  In other words, estimate \eqn{f'()}
  and/or \eqn{f''()} for the model
  \deqn{Y_i = f(x_i) + E_i, \ \ i = 1,\dots n,}
}
\usage{
D1D2(x, y, xout = x, fudge.fact = 10, deriv = 1:2, spl.spar = NULL)
}
\arguments{
  \item{x,y}{numeric vectors of same length, supposedly from a model
    \code{y ~ f(x)}.}
  \item{xout}{abscissa values at which to evaluate the derivatives.}
  \item{fudge.fact}{numeric fudge factor for the smoothing parameter,
    see \code{spl.par} below.}
  \item{deriv}{integer in \code{1:2} indicating which derivatives are to
    be computed.}
  \item{spl.spar}{direct smoothing parameter for \code{smooth.spline}.
    If it is \code{NULL} (as per default), the smoothing parameter used
    will be \code{fudge.fact * sp$spar}, where \code{sp$spar} is the GCV
  estimated smoothing parameter, see \code{\link{smooth.spline}}.}
}
\details{
  It is well known that for derivative estimation, the optimal smoothing
  parameter is larger (more smoothing) than for the function itself.
  \code{fudge.fact} is really just a  \emph{fudge} factor with which the
  smoothing parameter is multiplied.

  When \code{deriv = 1:2} (as per default), both derivatives are
  estimated with the \emph{same} smoothing parameter which is suboptimal
  for the single functions individually.  Another possibility is to call
  \code{D1D2(*, deriv = k)} twice with \code{k = 1} and \code{k = 2} and
  use a \emph{larger} smoothing parameter for the second derivative.
}
\value{
  A list with components
  \item{D1}{if \code{deriv} contains 1, estimated values of
    \eqn{f'(x_i)} where \eqn{x_i} are the values from \code{xout}.}
  \item{D2}{if \code{deriv} contains 2, estimated values of \eqn{f''(x_i)}.}
  \item{spar}{the \bold{s}moothing \bold{par}ameter used in the (final)
    \code{smooth.spline} call.}
  \item{df}{the equivalent \bold{d}egrees of \bold{f}reedom in that
    \code{smooth.spline} call.}
}
\author{Martin Maechler, in 1992 (for S).}
\seealso{\code{\link{smooth.spline}} on which it relies completely.}
\examples{
 set.seed(8840)
 x <- runif(100, 0,10)  
 y <- sin(x) + rnorm(100)/4

 op <- par(mfrow = c(2,1))
 plot(x,y)
 lines(ss <- smooth.spline(x,y), col = 4)
 str(ss[c("df", "spar")])
 if(is.R()) plot(cos, 0, 10) else { # Splus
   xx <- seq(0,10, len=201); plot(xx, cos(xx), type = 'l')
 } 
 i <- 1
 for(fudge in c(1, 3,10,30)) {
   d12 <- D1D2(x,y, fudge = fudge)
   lines(d12$x, d12$D1, col = i <- i+1)
 }
 par(op)
}
\keyword{smooth}
