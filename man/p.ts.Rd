\name{p.ts}
\alias{p.ts}
\title{plot.ts with multi-plots and Auto-Title -- on 1 page}
\description{
  For longer time-series, it is sometimes important to spread the
  time-series plots over several subplots.
  p.ts(.) does this both automatically, and under manual control.

  Actually, this is a generalization of \code{\link[base]{plot.ts}}
  (with different defaults).
}
\usage{
p.ts(x, nrplots = max(1, min(8, n \%/\% 400)), overlap = nk \%/\% 16,
     date.x = NULL, do.x.axis = !is.null(date.x), do.x.rug = FALSE,
     ax.format, main.tit = NULL, ylim = NULL, ylab = "", xlab = "Time",
     quiet = FALSE, mgp = c(1.25, .5, 0), \dots)
}
\arguments{
  \item{x}{timeseries (possibly multivariate) or numeric vector.}
  \item{nrplots}{number of sub-plots.  Default: in \{1..8\},
    approximately \code{n/400} if possible.}
  \item{overlap}{by how much should subsequent plots overlap. Defaults
    to about 1/16 of sub-length on each side.}
  \item{date.x}{a time \dQuote{vector} of the same length as \code{x}
    and coercable to class \code{"POSIXct"} (see \link{DateTimeClasses}).}
  \item{do.x.axis}{logical specifying if an x axis should be drawn
    (i.e., tick marks and labels).}
  \item{do.x.rug}{logical specifying if \code{\link{rug}} of
    \code{date.x} values should drawn along the x axis.}
  \item{ax.format}{when \code{do.x.axis} is true, specify the
    \code{format} to be used in the call to \code{\link{axis.POSIXct}}.}
  \item{main.tit}{\bold{Main} title (over all plots).  Defaults to name
    of \code{x}.}
  \item{ylim}{numeric(2) or NULL; if the former, specifying the y-range
    for the plots. Defaults to a common pretty range.}
  \item{ylab, xlab}{labels for y- and x-axis respectively, see
    description in \code{\link{plot.default}}.}
  \item{quiet}{logical; if \code{TRUE}, there's no reporting on each subplot.}
  \item{mgp}{numeric(3) to be passed to \code{\link{mult.fig}()}, see
    \code{\link{par}(mgp = .)}.}
  \item{\dots}{further graphic parameters for each \code{\link{plot.ts}(..)}.}
}
\section{Side Effects}{
  A page of \code{nrplots} subplots is drawn on the current
  graphics device.
}
\author{Martin Maechler, \email{maechler@stat.math.ethz.ch}; July 1994
  (for S).}
}
\seealso{\code{p.ts()} calls \code{\link{mult.fig}()} for setup.
  Further, \code{\link{plot.ts}} and \code{\link{plot}}.
}
\examples{
stopifnot(require(stats)) # package

data(sunspots)
p.ts(sunspots, nr=1) # == usual  plot.ts(..)
p.ts(sunspots)
p.ts(sunspots, nr=3, col=2)

## multivariate :
data(EuStockMarkets)
p.ts(log10(EuStockMarkets), col = 2:5)

## with Date - x-axis (dense random dates):
set.seed(12)
x <- as.Date("2000-02-29") + cumsum(1+ rpois(1000, lambda= 2.5))
z <- cumsum(.1 + 2*rt(1000, df=3))
p.ts(z, 4, date.x = x)
p.ts(z, 6, date.x = x, ax.format = "\%b \%Y", do.x.rug = TRUE)
}
\keyword{hplot}
\keyword{ts}
