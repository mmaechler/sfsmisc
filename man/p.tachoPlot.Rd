\name{p.tachoPlot}
\alias{p.tachoPlot}
\title{Draw Symbol on a Plot}
\description{
Puts a symbol (pointer) on a plot at each of the specified locations. 
}
\usage{
p.tachoPlot <- function(x, y, z, angle=c(pi/4,3*pi/4), size,
			method="robust",
                        legend=T, show.method=T, \dots)
}
\arguments{
\item{x,y,z}{
coordinates of points. Numeric vectors of the same length.
Missing values (\code{NA}s) are allowed.
}
\item{angle}{
numeric vector whose elements give the angles between the horizontal
baseline and the minimum and maximum direction of the pointer measured
clockwise in radians. DEFAULT is c(pi/4,3*pi/4).
}
\item{size}{
length of the pointers in cm.
}
\item{method}{
the method to calculate the angle of the pointer. Either the string
\code{"sensitive"}, \code{"robust"} or \code{"rank"}. Only the first two characters are
necessary. DEFAULT is \code{"robust"}.
The minimum and maximum direction of the pointer corresponds to min(z)
and max(z) if method is \code{"sensitive"} or \code{"rank"} and to the upper and lower
extreme of z if method is \code{"robust"} (see \code{boxplot} or \code{rrange} for
details). The angle is proportional to z or rank(z) in case of method=\code{"rank"}.
}
\item{legend}{
logical flag: if \code{TRUE} a legend giving the values of the minimum and maximum
direction of the pointer is drawn. DEFAULT is \code{TRUE}.
}
\item{show.method}{
logical flag: if \code{TRUE} the method name is printed. DEFAULT is \code{TRUE}.
}
\item{...}{
further arguments to plot.
Graphical parameters (see \code{\link{par}}) may also
be supplied as arguments to this function.
}}
\section{Side Effects}{
A plot is created on the current graphics device.
}
\details{
A scatter plot of the variables x and y is plotted. The value of the third
variable z is given by the direction of a pointer (similar to a
tachometer). Observations whose z-coordinate is missing are marked by a dot.
}
\section{AUTHOR}{
Christian Keller  \code{<keller@stat.math.ethz.ch>}
}
\seealso{
\code{\link{symbols}}
}
\examples{
p.tachoPlot(ozone.xy$x, ozone.xy$y, ozone.median)


par(mfrow=c(2,2))
## see the difference between the three methods
p.tachoPlot(car.all$Length, car.all$Width, car.all$Height, method="sens")
p.tachoPlot(car.all$Length, car.all$Width, car.all$Height, method="rank")
p.tachoPlot(car.all$Length, car.all$Width, car.all$Height, method="rob")
}
\keyword{hplot}
\keyword{plot}
% Converted by Sd2Rd version 0.3-3.
