\name{plotDS}
\alias{plotDS}
\alias{pl.ds}% <-- old name; deprecated from 2007 to 2013; defunct since 2014-01
\title{Plot Data and Smoother / Fitted Values}
\description{
  For one-dimensional nonparametric regression, plot the data and fitted
  values, typically a smooth function, and optionally use segments to
  visualize the residuals.
}
\usage{
plotDS(x, yd, ys, xlab = "", ylab = "", ylim = rrange(c(yd, ys)),
       xpd = TRUE, do.seg = TRUE, seg.p = 0.95,
       segP = list(lty = 2, lwd = 1,   col = 2),
       linP = list(lty = 1, lwd = 2.5, col = 3),
       \dots)
}
\arguments{
  \item{x, yd, ys}{numeric vectors all of the same length, representing
    \eqn{(x_i, y_i)} and fitted (smooth) values \eqn{\hat{y}_i}{y^_i}.
    \code{x} will be sorted increasingly if necessary, and \code{yd} and
    \code{ys} accordingly.

    Alternatively, \code{ys} can be an x-y list (as resulting from
    \code{\link[grDevices]{xy.coords}}) containing fitted values on a
    finer grid than the observations \code{x}.  In that case, the
    observational values \code{x[]} \bold{must} be part of the larger
    set; \code{\link{seqXtend}()} may be applied to construct such a set
    of abscissa values.
  }
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
\author{Martin Maechler, since 1990}
\note{Non-existing components in the lists \code{segP} or \code{linP}
  will result in the \code{\link{par}} defaults to be used.

  \code{plotDS()} used to be called \code{pl.ds} up to November 2007.
}
\seealso{\code{\link{seqXtend}()} to construct more smooth \code{ys}
  \dQuote{objects}.
}
\examples{
 data(cars)
 x <-  cars$speed
 yd <- cars$dist
 ys <- lowess(x, yd, f = .3)$y
 plotDS(x, yd, ys)

 ## More interesting : Version of example(Theoph)
 data(Theoph)
 Th4 <- subset(Theoph, Subject == 4)
 ## just for "checking" purposes -- permute the observations:
 Th4 <- Th4[sample(nrow(Th4)), ]
 fm1 <- nls(conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data = Th4)

 ## Simple
 plotDS(Th4$Time, Th4$conc, fitted(fm1),
        sub  = "Theophylline data - Subject 4 only",
        segP = list(lty=1,col=2), las = 1)

 ## Nicer: Draw the smoother not only at x = x[i] (observations):
 xsm <- unique(sort(c(Th4$Time, seq(0, 25, length = 201))))
 ysm <- c(predict(fm1, newdata = list(Time = xsm)))
 plotDS(Th4$Time, Th4$conc, ys = list(x=xsm, y=ysm),
        sub  = "Theophylline data - Subject 4 only",
        segP = list(lwd=2), las = 1)
}
\keyword{hplot}
