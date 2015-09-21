\name{p.arrows}
\alias{p.arrows}
\title{Prettified Arrows Plots}
\description{
  Draws arrows, like the \code{\link{arrows}} function, but with
  \dQuote{nice} \emph{filled} arrow heads.
}
\usage{
p.arrows(x1, y1, x2, y2, size = 1, width, fill = 2, ...)
}
\arguments{
  \item{x1, y1}{coordinates of points \bold{from} which to draw.}
  \item{x2, y2}{coordinates of points \bold{to} which to draw.}
  \item{size}{symbol size as a fraction of a character height; default 1.}
  \item{width}{width of the arrow head; defaults to ....}%fixme (code => ??)
  \item{fill}{color for filling the arrow head.}
  \item{\dots}{further arguments passed to \code{\link{segments}()}.}
}
\author{Andreas Ruckstuhl, 19 May 1994; (cosmetic by MM).}
\seealso{\code{\link{arrows}}.}
\examples{
example(arrows, echo = FALSE) #-> x, y, s
plot(x,y, main="p.arrows(.)")
p.arrows(x[s], y[s], x[s+1], y[s+1], col= 1:3, fill = "dark blue")
}
\keyword{aplot}

