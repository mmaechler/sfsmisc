\name{quadrant}
\alias{quadrant}
\title{Give the Quadrant Number of Planar Points}
\description{
  Determine the quadrant of planar points, i.e. in which of the four
  parts cut by the x- and y- axis the points lie.  Zero values
  (i.e. points on the axes) are treated as if \emph{positive}.
}
\usage{
quadrant(x, y=NULL)
}
\arguments{
  \item{x,y}{numeric vectors of the same length, or \code{x} is an \eqn{x-y}
    structure and \code{y=NULL}, see \code{\link{xy.coords}}.}
}
\value{
  numeric vector of same length as \code{x} (if that's a vector) with
  values in \code{1:4} indicating the quadrant number of the
  corresponding point.
}
%%\seealso{ ~~objects to See Also as \code{\link{~~fun~~}}, ~~~ }
\examples{
xy <- as.matrix(expand.grid(x= -7:7, y= -7:7)); rownames(xy) <- NULL
(qu <- quadrant(xy))
plot(xy, col = qu+1, main = "quadrant() number", axes = FALSE)
abline(h=0, v=0, col="gray") # the x- and y- axis
text(xy, lab = qu, col = qu+1, adj = c(1.4,0))
}
\keyword{utilities}

