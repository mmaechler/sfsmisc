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
     main.tit = NULL, ylim = NULL, ylab = "", quiet = FALSE,
     mgp = c(1.25, .5, 0),\dots)
}
\arguments{
  \item{x}{timeseries (possibly multivariate) or numeric vector.}
  \item{nrplots}{number of sub-plots.  Default: in \{1..8\},
    approximately \code{n/400} if possible.}
  \item{overlap}{by how much should subsequent plots overlap. Defaults
    to about 1/16 of sub-length on each side.}
  \item{main.tit}{\bold{Main} title (over all plots).  Defaults to name
    of \code{x}.}
  \item{ylim}{numeric(2) or NULL; if the former, specifying the y-range
    for the plots. Defaults to a common pretty range.}
  \item{ylab}{label for y-axis, see description in \code{\link{plot.default}}.}
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
\seealso{\code{\link{plot.ts}}, \code{\link{plot}}.
}
\examples{
require(ts) # package

data(sunspots)
p.ts(sunspots, nr=1) # == usual  plot.ts(..)
p.ts(sunspots)	
p.ts(sunspots, nr=3, col=2)	

## multivariate :
data(EuStockMarkets)
p.ts(log10(EuStockMarkets), col = 2:5)
}
\keyword{hplot}
\keyword{ts}

