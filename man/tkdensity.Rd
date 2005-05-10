\name{tkdensity}
\alias{tkdensity}
\title{GUI Density Estimation using Tcl/Tk}
\description{
  This is graphical user interface (GUI) to \code{\link{density}},
  allowing for dynamic bandwidth choice and a simple kind of zooming,
  relying on \code{library(tcltk)}.
}
\usage{
tkdensity(y, n = 1024, log.bw = TRUE, showvalue = TRUE,
          xlim = NULL, do.rug = size < 1000, kernels = NULL,
          from.f = if (log.bw) -2 else 1/1000,
          to.f   = if (log.bw) +2.2 else 2,
          col = 2)
}
\arguments{
  \item{y}{numeric; the data the density of which we want.}
  \item{n}{integer; the number of abscissa values for
    \code{\link{density}} evaluation (and plotting).}
  \item{log.bw}{logical; if true (default), the gui scrollbar is on a
    \emph{log} bandwidth scale, otherwise, simple interval.}
  \item{showvalue}{logical; if true, the value of the current (log)
    bandwidth is shown on top of the scrollbar.}
  \item{xlim}{initial \code{xlim} for plotting, see \code{\link{plot.default}}.}
  \item{do.rug}{logical indicating if \code{\link{rug}(y)} should be
    added to each plot.  This is too slow for really large sample sizes.}
  \item{kernels}{character vector of kernel names as allowable for the
    \code{kernels} argument of the standard \code{\link{density}} function.}
  \item{from.f, to.f}{numeric giving the left and right limit of the
    bandwidth scrollbar.}
  \item{col}{color to be used for the density curve.}
}
\details{
  \code{library(tcltk)} must be working, i.e., Tcl/Tk must have been
  installed on your platform, and must have been visible during \R's
  configuration and/or installation.

  You can not only choose the bandwidth (the most important parameter),
  but also the kernel, and you can zoom in and out (in x-range only).
}
\value{
  none.\cr
  (How could this be done?  \code{tcltk} widgets run as separate processes!)
}
\author{Martin Maechler, building on \code{demo(tkdensity)}.}

%\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
data(faithful)
if(try(require(tcltk)))# still not there on broken installations
  tkdensity(faithful $ eruptions)

%% not yet
%% if(try(require(nor1mix)))
%%  tkdensity(rnorMix(150, MW...), kernels = c("gaussian", "epanechnikov"))
}

\keyword{hplot}
\keyword{dynamic}
