\name{p.tachoPlot}
\alias{p.tachoPlot}
\title{Draw Symbol on a Plot}
\description{
Puts a symbol (pointer) on a plot at each of the specified locations.
}
\usage{
p.tachoPlot(x, y, z, angle=c(pi/4,3*pi/4), size,
	    method = c("robust", "sensitive", "rank"),
	    legend = TRUE, show.method = legend,
	    xlab = deparse(substitute(x)), ylab = deparse(substitute(y)),
	    xlim, ylim, \dots)
}
\arguments{
  \item{x,y,z}{coordinates of points. Numeric vectors of the same length.
    Missing values (\code{NA}s) are allowed.}
  \item{angle}{numeric vector whose elements give the angles between the
    horizontal baseline and the minimum and maximum direction of the
    pointer measured clockwise in radians.}
  \item{size}{length of the pointers in cm.}
  \item{method}{string specifying the method to calculate the angle of
    the pointer.  One of \code{"sensitive"}, \code{"robust"} or
    \code{"rank"}.  Only the first two characters are necessary.

    The minimum and maximum direction of the pointer corresponds to
    min(z) and max(z) if method is \code{"sensitive"} or \code{"rank"}
    and to the upper and lower extreme of z if method is \code{"robust"}
    (see \code{boxplot} or \code{rrange} for details).  The angle is
    proportional to z or rank(z) in case of \code{method="rank"}.
  }
  \item{legend}{logical flag: if \code{TRUE} (default), a legend giving
    the values of the minimum and maximum direction of the pointer is drawn.}
  \item{show.method}{logical flag, defaulting to \code{legend}; if true,
    the method name is printed.}
  \item{xlab,ylab}{labels for x and y axis; defaults to the
    \sQuote{expression} used in the function call.}
  \item{xlim,ylim}{numeric of length 2, the limits for the x and y axis,
    respectively; see \code{\link{plot.default}}.}
  \item{\dots}{further arguments to \code{\link{plot}}.  Graphical
    parameters (see \code{\link{par}}) may also be supplied as arguments
    to this function.}
}
\section{Side Effects}{
  A plot is created on the current graphics device.
}
\details{
  A scatter plot of the variables x and y is plotted. The value of the third
  variable z is given by the direction of a pointer (similar to a
  tachometer). Observations whose z-coordinate is missing are marked by a dot.
}
\author{Christian Keller, June 1995}
\seealso{\code{\link{symbols}}}
\examples{
data(state)
data(USArrests)
p.tachoPlot(state.center $x, state.center $y, USArrests[,"UrbanPop"])

data(mtcars)
par(mfrow=c(2,2))
## see the difference between the three methods (not much differ. here!)
%% hence, IMPROVE the example !
p.tachoPlot(mtcars$hp, mtcars$disp, mtcars$mpg, method="sens")
p.tachoPlot(mtcars$hp, mtcars$disp, mtcars$mpg, method="rank")
p.tachoPlot(mtcars$hp, mtcars$disp, mtcars$mpg, method="rob")
}
\keyword{hplot}
