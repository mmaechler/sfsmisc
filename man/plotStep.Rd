% $Header: /u/maechler/S/GRAPHICS/plot.step.d,v 1.2 1997/05/27 10:17:27 maechler Exp $ ---
% Copyright (c), 1991, 1997 Martin Maechler, Statistik, ETH Zurich
%		Martin Maechler <maechler@stat.math.ethz.ch>
\name{plot.step}
\alias{plot.step}
\title{Plot a Step Function}
\description{
  Plots a step function
  f(x)= sum\{ y[i] * 1_[ t[i-1], t[i] ) (x) \},
  i.e., a piecewise constant function of one variable.
  With one argument, plots "THE" empirical cumulative distribution function.
}
\usage{
plot.step(ti, y,
          cad.lag = T,
          verticals = !cad.lag,
          left.points= cad.lag, right.points= F, end.points= F,
	  add = FALSE,
	  pch = par('pch'),
          xlab=deparse(substitute(ti)), ylab=deparse(substitute(y)),
          main=NULL, \dots)
}
\arguments{
\item{ti}{Numeric Vector = X[1:N] or  t[0:n]}
\item{y}{Numeric Vector y[1:n]; if omitted take y = k/N  for empirical CDF
}
\item{cad.lag}{
Logical: Draw 'cad.lag', i.e., "continue a droite, limite a gauche". Default = T
}
\item{verticals}{Logical: Draw vertical lines?  Default= ! cad.lag}
\item{left.points}{Logical: Draw left points?     Default= cad.lag}
\item{right.points}{Logical: Draw right points?    Default= FALSE}
\item{end.points}{Logical: Draw 2 end points?    Default= FALSE}
\item{add}{Logical: Add to existing plot?    Default= FALSE

  Any arguments to \code{plot(..)}; the following are treated specially:
}
\item{pch}{The plotting character for points}
\item{xlab}{Label of x-axis}
\item{ylab}{Label of y-axis}
\item{main}{Main Title; Default = 'call'; if you do not want a title,
  use 'main=""'.}
\item{...}{Any valid argument to plot(..), e.g.,
  main = "This is the Main Title", lty=3. 
}
}
\value{
[INVISIBLY:]  List with components \code{t} and \code{y}.
}
\section{Side Effects}{
Calls plot(..), points(..),  segments(..) appropriately
and plots on current graphics device.


NOTE:
}
\author{
Martin Maechler, Seminar for Statistics, ETH Zurich, Switzerland;
\email{maechler@stat.math.ethz.ch}
}
\seealso{
plot, segments, approx(\dots{}, method = "constant")
}
\examples{
##-- Draw an Empirical CDF  (and see the default title ..)
plot.step(rnorm(15))

plot.step(runif(25), cad.lag=F)
plot.step(runif(25), cad.lag=F, add=T, lty = 2)


ui _ sort(runif(20))
plot.step(ui, ni _cumsum(rpois(19, lambda=1.5) - 1.5), cad.lag = F)
plot.step(ui, ni, verticals = T, right.points = T)


plot.step(rnorm(201), pch = '.') #- smaller points
}
\keyword{empirical}
\keyword{nonparametric}
\keyword{hplot}
\keyword{step}
% Converted by Sd2Rd version 0.3-3.
