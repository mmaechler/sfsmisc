\name{p.ts}
\alias{p.ts}
\title{plot.ts with multi-plots and Auto-Title -- on 1 page}
\description{
  For longer time-series, it is sometimes important to spread the
  time-series plots over several subplots.
  p.ts(.) does this both automatically, and under manual control.

  This is a generalization of \code{\link[base]{plot.ts}} for the case
  of only one time-series. 
}
\usage{
p.ts(x, nrplots=max(1, min(8, n \%/\% 400)), overlap=nk \%/\% 16, 
     main.tit = NULL, quiet = FALSE, \dots)
}
\arguments{
  \item{x}{timeseries or numeric vector.}
  \item{nrplots}{number of sub-plots. DEFAULT: in \{1..8\},  ~=
    \code{n}/400 if possible}
  \item{overlap}{how much should subsequent plots overlap.
    DEFAULT: ca. 1/16 of sub-length on each side.}
  \item{main.tit}{MAIN title (over all plots). DEFAULT: Name of \code{x}.}
  \item{quiet}{logical; if \code{TRUE}, there's no reporting on each subplot.}
  \item{\dots}{further graphic parameters for each  ts.plot(..).}
}
\section{Side Effects}{
  A new page of \code{nrplots} subplots are drawn on the current
  graphics device.
}
\author{Martin Maechler, \email{maechler@stat.math.ethz.ch}
}
\note{
  BUG: For "real" time-series and multiple plots (\code{nrplots > 1}),
  the simple sequence \code{1:n} is used instead of the "real time scale".
}
\seealso{\code{\link{plot.ts}}, \code{\link{plot}}.
}
\examples{
data(sunspots)
p.ts(sunspots, nr=1) # == usual  plot.ts(..)
p.ts(sunspots)	
p.ts(sunspots, nr=3, col=2)	
}
\keyword{hplot}
\keyword{ts}
% Converted by Sd2Rd version 0.3-3.
