\name{mpl}
\alias{mpl}
\alias{p.m}
\title{Simple Matrix Plots}
\description{
  Do simple matrix plots, providing an easy interface to
  \code{\link{matplot}} by using a default x variable.
}
\usage{
mpl(mat, \dots)
p.m(mat, \dots)
}
\arguments{
  \item{mat}{numeric matrix.}
  \item{\dots}{further arguments passed to \code{\link{matplot}},
    e.g., \code{type}, \code{xlab}, etc.}
}
\details{
  \code{p.m(m)} use the first column of \code{m} as \eqn{x} variable,
  whereas \code{mpl(m)} uses the integers 1, 2, \ldots, \code{nrow(m)}
  as coordinates and \code{rownames(m)} as axis labels if possible.
}
\note{These were really created for playing around with curves etc, and
  probably should be \emph{deprecated} since in concrete examples, using
  \code{matplot()} directly is more appropriate.
}
\author{Martin Maechler}
\seealso{\code{\link{matplot}},
  \code{\link{plot.mts}(*, plot.type = "single")}.}
\examples{
data(animals, package = "cluster")
mpl(animals, type = "l")
}
\keyword{hplot}
\keyword{array}
