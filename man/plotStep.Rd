% Originally /u/maechler/S/GRAPHICS/plot.step.d,v 1.2 1997/05/27 10:17:27
% Copyright (c), 1991, 1997 Martin Maechler, Statistik, ETH Zurich
%		Martin Maechler <maechler@stat.math.ethz.ch>
\name{plotStep}
\alias{plotStep}
\title{Plot a Step Function}
\description{
  Plots a step function
  f(x)= \eqn{\sum_i  y_i  1_[ t_{i-1}, t_i ](x) }{sum(i; y[i] *
    Ind[t[i-1], t[i])(x))}, i.e., a piecewise constant function of one variable.
  With one argument, plots \bold{the} empirical cumulative distribution
  function.
}
\usage{
plotStep(ti, y,
          cad.lag = TRUE,
          verticals = !cad.lag,
          left.points= cad.lag, right.points= FALSE, end.points= FALSE,
	  add = FALSE,
	  pch = par('pch'),
          xlab=deparse(substitute(ti)), ylab=deparse(substitute(y)),
          main=NULL, \dots)
}
\arguments{
  \item{ti}{numeric vector = \code{X[1:N]} or  \code{t[0:n]}.}
  \item{y}{numeric vector \code{y[1:n]}; if omitted take y = k/N
    for empirical CDF.}
  \item{cad.lag}{logical: Draw 'cad.lag', i.e., ``continue à droite, limite à
    gauche''.  Default = TRUE.}
  \item{verticals}{logical: Draw vertical lines?  Default= ! cad.lag}
  \item{left.points}{logical: Draw left points?     Default= cad.lag}
  \item{right.points}{logical: Draw right points?    Default= FALSE}
  \item{end.points}{logical: Draw 2 end points?    Default= FALSE}
  \item{add}{logical: Add to existing plot?    Default= FALSE}
  \item{pch}{The plotting character for points}
  \item{xlab}{Label of x-axis}
  \item{ylab}{Label of y-axis}
  \item{main}{main title; defaults to the call' if you do not want a title,
    use \code{main = ""}.}
  \item{\dots}{Any valid argument to \code{\link{plot}(..)}.}
}
\value{
[INVISIBLY:]  List with components \code{t} and \code{y}.
}
\section{Side Effects}{
  Calls plot(..), points(..),  segments(..) appropriately
  and plots on current graphics device.
}
\author{
  Martin Maechler, Seminar for Statistics, ETH Zurich,
  \email{maechler@stat.math.ethz.ch}, 1991 ff.
}
\seealso{
  \code{\link[stepfun]{plot.ecdf}} and
  \code{\link[stepfun]{plotStepfun}} in \R which are conceptually nicer.

  \code{\link{plot}},
  \code{\link{segments}(\dots, method = "constant")}.
}
\examples{
##-- Draw an Empirical CDF  (and see the default title ..)
plotStep(rnorm(15))

plotStep(runif(25), cad.lag=FALSE)
plotStep(runif(25), cad.lag=FALSE, add=TRUE, lty = 2)

ui <- sort(runif(20))
plotStep(ui, ni <- cumsum(rpois(19, lambda=1.5) - 1.5), cad.lag = FALSE)
plotStep(ui, ni, verticals = TRUE, right.points = TRUE)

plotStep(rnorm(201), pch = '.') #- smaller points
}
\keyword{nonparametric}
\keyword{hplot}
