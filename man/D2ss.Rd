\name{D2ss}
\alias{D2ss}
\alias{D1ss}
\alias{D1tr}
\title{Numerical Derivatives of (x,y) Data  (via Smoothing Splines)}
\description{
  Compute the numerical first or 2nd derivatives of \eqn{f()} given
  observations \code{(x[i], y ~= f(x[i]))}.

  \code{D1tr} is the \emph{\bold{tr}ivial} discrete first derivative
  using simple difference ratios, whereas \code{D1ss} and \code{D2ss}
  use cubic smoothing splines (see \code{\link[stats]{smooth.spline}})
  to estimate first or second derivatives, respectively.

  \code{D2ss} first uses \code{smooth.spline} for the first derivative
  \eqn{f'()} and then applies the same to the predicted values
  \eqn{\hat f'(t_i)}{f'^(t[i])} (where \eqn{t_i}{t[i]} are the values of
  \code{xout}) to find   \eqn{\hat f''(t_i)}{f''^(t[i])}.
}
\usage{
D1tr(y, x = 1)

D1ss(x, y, xout = x, spar.offset = 0.1384, spl.spar=NULL)
D2ss(x, y, xout = x, spar.offset = 0.1384, spl.spar=NULL)
}
\arguments{
  \item{x,y}{numeric vectors of same length, supposedly from a model
    \code{y ~ f(x)}.  For \code{D1tr()}, \code{x} can have length one
    and then gets the meaning of \eqn{h = \Delta x}.}
  \item{xout}{abscissa values at which to evaluate the derivatives.}
  \item{spar.offset}{numeric fudge added to the smoothing parameter(s),
    see \code{spl.par} below.  Note that the current default is there
    for historical reasons only, and we often  would recommend to use
    \code{spar.offset = 0} instead.}
  \item{spl.spar}{direct smoothing parameter(s) for \code{smooth.spline}.
    If it is \code{NULL} (as per default), the smoothing parameter used
    will be \code{spar.offset + sp$spar}, where \code{sp$spar} is the GCV
  estimated smoothing parameter for \emph{both} smooths, see
  \code{\link{smooth.spline}}.}
}
\details{
  It is well known that for derivative estimation, the optimal smoothing
  parameter is larger (more smoothing needed) than for the function itself.
  \code{spar.offset} is really just a \emph{fudge} offset added to the
  smoothing parameters. Note that in \R's implementation of
  \code{\link{smooth.spline}}, \code{spar} is really on the
  \eqn{\log\lambda} scale.
%
%   When \code{deriv = 1:2} (as per default), both derivatives are
%   estimated with the \emph{same} smoothing parameter which is suboptimal
%   for the single functions individually.  Another possibility is to call
%   \code{D1D2(*, deriv = k)} twice with \code{k = 1} and \code{k = 2} and
%   use a \emph{larger} smoothing parameter for the second derivative.
}
\value{
  \code{D1tr()} and \code{D1ss()} return a numeric vector of the length
  of \code{y} or \code{xout}, respectively.

  \code{D2ss()} returns a list with components
  \item{x}{the abscissae values (= \code{xout}) at which the
    derivative(s) are evaluated.}
  \item{y}{estimated values of \eqn{f''(x_i)}.}
  \item{spl.spar}{numeric vector of length 2, contain the \code{spar}
    arguments to the two \code{smooth.spline} calls.}
  \item{spar.offset}{as specified on input (maybe rep()eated to length 2).}
}
\author{Martin Maechler, in 1992 (for S).}
\seealso{\code{\link{D1D2}} which directly uses the 2nd derivative of
  the smoothing spline;  \code{\link{smooth.spline}}.
}
\examples{

## First Derivative  --- spar.off = 0  ok "asymptotically" (?)
set.seed(330)
mult.fig(12)
for(i in 1:12) {
  x <- runif(500, 0,10); y <- sin(x) + rnorm(500)/4
  f1 <- D1ss(x=x,y=y, spar.off=0.0)
  plot(x,f1, ylim = range(c(-1,1,f1)))
  curve(cos(x), col=3, add= TRUE)
}

 set.seed(8840)
 x <- runif(100, 0,10)
 y <- sin(x) + rnorm(100)/4

 op <- par(mfrow = c(2,1))
 plot(x,y)
 lines(ss <- smooth.spline(x,y), col = 4)
 str(ss[c("df", "spar")])
 xx <- seq(0,10, len=201)
 plot(xx, -sin(xx), type = 'l', ylim = c(-1.5,1.5))
 title(expression("Estimating f''() :  " * frac(d^2,dx^2) * sin(x) == -sin(x)))
 offs <- c(0.05, 0.1, 0.1348, 0.2)
 i <- 1
 for(off in offs) {
   d12 <- D2ss(x,y, spar.offset = off)
   lines(d12, col = i <- i+1)
 }
 legend(2,1.6, c("true :  -sin(x)",paste("sp.off. = ", format(offs))), lwd=1,
        col = 1:(1+length(offs)), cex = 0.8, bg = NA)
 par(op)
}
\keyword{smooth}
