\name{pl.ds}
\alias{pl.ds}
\title{Plot Data and Smoother / Fitted Values}
\description{
  For one-dimensional nonparametric regression, plot the data and fitted
  values, typically a smooth function, and optionally use segments to
  visualize the residuals.
}
\usage{
pl.ds(x, yd, ys, xlab = "", ylab = "", ylim = rrange(c(yd, ys)),
      xpd = TRUE, do.seg = TRUE, seg.p = 0.95,
      segP = list(lty = 2, lwd = 1,   col = 2),
      linP = list(lty = 1, lwd = 2.5, col = 3),
      \dots)
}
\arguments{
  \item{x, yd, ys}{numeric vectors all of the same length, representing
    \eqn{(x_i, y_i)} and fitted (smooth) values \eqn{\hat{y}_i}{y^_i}.
    Note that \code{x} will be sort increasingly if necessary, and
    \code{yd} and \code{ys} accordingly.}
  \item{xlab, ylab}{x- and y- axis labels, as in \code{\link{plot.default}}.}
  \item{ylim}{limits of y-axis to be used; defaults to a \emph{robust}
    range of the values.}
  \item{xpd}{see \code{\link{par}(xpd=.)}; by default do allow to draw
    outside the plot region.}
  \item{do.seg}{logical indicating if residual segments should be drawn,
    at \code{x[i]}, from \code{yd[i]} to \code{ys[i]} (approximately,
    see \code{seg.p}).}
  \item{seg.p}{segment percentage of segments to be drawn, from
    \code{yd} to \code{seg.p*ys + (1-seg.p)*yd}.}
  \item{segP}{list with named components \code{lty, lwd, col} specifying
    line type, width and color for the residual segments,
    used only when \code{do.seg} is true.}
  \item{linP}{list with named components \code{lty, lwd, col} specifying
    line type, width and color for \dQuote{smooth curve lines}.}
  \item{\dots}{further arguments passed to \code{\link{plot}}.}
}
\author{Martin Maechler, 1990-1994}
\note{Non-existing components in the lists \code{segP} or \code{linP}
  will result in the \code{\link{par}} defaults to be used.
}
%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
 data(cars)
 x <-  cars$speed
 yd <- cars$dist
 ys <- lowess(x, yd,f = .2)$y
 pl.ds(x, yd, ys)

 ## More interesting : Version of example(Theoph)
 data(Theoph)
 Th4 <- subset(Theoph, Subject == 4)
 fm1 <- nls(conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data = Th4)
 pl.ds(Th4$Time, Th4$conc, fitted(fm1),
       sub  = "Theophylline data - Subject 4 only",
       segP = list(lty=1,col=2), las = 1)
 xvals <- seq(0, par("usr")[2], len = 55)
 lines(xvals, predict(fm1, newdata = list(Time = xvals)), col = 4)
}
\keyword{hplot}
