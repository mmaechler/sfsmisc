\name{cum.Vert.funkt}
\alias{cum.Vert.funkt}

\title{Kumulative Verteilung Aufzeichnen}
\description{
  Kumulative Verteilung von \code{x} aufzeichnen, auf Wunsch auch Median
  und Quartile.

  This is just an old German language version of
  \code{\link[stats]{plot.ecdf}()} used for teaching at ETHZ.
}
\usage{
cum.Vert.funkt(x, Quartile = TRUE, titel = TRUE, Datum = TRUE,
               rang.axis = n <= 20, xlab = "", main = "", \dots)
}
\arguments{
  \item{x}{numeric vector whose empirical distribution should be plotted.}
  \item{Quartile}{logical indicating if all 3 non-trivial quartiles
    should be drawn.}
  \item{titel}{logical indicating if a German title should be drawn.}
  \item{Datum}{logical indicating if \code{\link{p.datum}} should be added.}
  \item{rang.axis}{logical indicating if all the ranks should be marked at
    the y-axis.  Defaults to true if there are not more than 20 observations.}
  \item{xlab, main}{x-axis label and main title; default to empty.}
  \item{\dots}{optional further arguments, passed to \code{\link{plotStep}}.}
}
\value{
  the return value of \code{\link{plotStep}()} which is called
  internally, \emph{invisibly}.
}
\author{Martin Maechler et al.}
\seealso{\code{\link{plotStep}} on which it is based; but you should
  really consider using \code{\link[stats]{plot.ecdf}()} from the
  \pkg{stats} package instead of this.}
\examples{
cum.Vert.funkt(runif(12))
cum.Vert.funkt(runif(20))

Z <- rnorm(50)
cum.Vert.funkt(Z)
}
\keyword{hplot}
