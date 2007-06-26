\name{rot2}
\encoding{latin1}% "^o" (degree) below
\alias{rot2}
\title{Rotate Planar Points by Angle}
\description{
  Rotate planar (xy) points by angle \code{phi} (in radians).
}
\usage{
rot2(xy, phi)
}
\arguments{
  \item{xy}{numeric 2-column matrix, or coercable to one.}
  \item{phi}{numeric scalar, the angle in radians (i.e., \code{phi=pi}
    corresponds to 180 degrees) by which to rotate the points.}
}
\value{
  A two column matrix as \code{xy}, containing the rotated points.
}
\author{Martin Maechler, Oct.1994}
\examples{
## Rotate three points by 60 degrees :
(xy0 <- rbind(c(1,0.5), c(1,1), c(0,1)))
(Txy <- rot2(xy0, phi = 60 * pi/180))
plot(xy0, col = 2, type = "b", asp = 1,
     xlim=c(-1,1), ylim=c(0,1.5), main = "rot2(*, pi/3) : 2d rotation by 60°")
points(Txy, col = 3, type = "b")
O <- rep(0,2); P2 <- rbind(xy0[2,], Txy[2,])
arrows(O,O,P2[,1],P2[,2], col = "dark gray")

xy0 <- .8*rbind(c(1,0), c(.5,.6), c(.7,1), c(1,1), c(.9,.8), c(1,0)) - 0.2
plot(xy0, col= 2, type="b", main= "rot2( <polygon>, pi/4 * 1:7)", asp=1,
     xlim=c(-1,1),ylim=c(-1,1), lwd= 2, axes = FALSE, xlab="", ylab="")
abline(h=0, v=0, col="thistle"); text(1.05, -.05, "x"); text(-.05,1.05, "y")
for(phi in pi/4 * 0:7)
   do.call("arrows",c(list(0,0),rot2(xy0[2,], phi), length=0.1, col="gray40"))
for(phi in pi/4 * 1:7)
   polygon(rot2(xy0, phi = phi), col = 1+phi/(pi/4), border=2, type = "b")
}
\keyword{manip}
\keyword{math}
