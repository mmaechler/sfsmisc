\name{xy.grid}
\alias{xy.grid}
\title{Produce regular grid matrix.}
\description{
  Produce the grid used by \code{\link{persp}}, \link{contour}, etc, as
  an \code{N x 2} matrix.
  This is really outdated by \code{\link{expand.grid}()} nowadays.
}
\usage{
xy.grid(x, y)
}
\arguments{
  \item{x,y}{any vectors of same mode.}
}
\value{
  a 2-column matrix of ``points'' for each combination of \code{x} and
  \code{y}, i.e. with \code{length(x) * length(y)} rows.
}
\author{Martin Maechler, 26 Oct 1994.}
\seealso{\code{\link{expand.grid}} which didn't exist when
  \code{xy.grid} was first devised.}
\examples{
plot(xy.grid(1:7, 10*(0:4)))

x <- 1:3 ;  y <- 10*(0:4)
xyg <- xy.grid(x,y)

## Compare with expand.grid() :
m2 <- as.matrix(expand.grid(y,x)[, 2:1])
dimnames(m2) <- NULL
stopifnot(identical(xyg, m2))
}
\keyword{array}
