\name{u.boxplot.x}
\alias{u.boxplot.x}
\title{Utility Returning x-Coordinates of Boxplot}
\description{
  Return the x-coordinates in an \sQuote{n-way} side-by-side boxplot.
  This is an auxiliary function and exists mainly for backcompatibility
  with S-plus.
}
\usage{
u.boxplot.x(n, j = 1:n, fullrange = 100)
}
\arguments{
  \item{n}{number of boxplots.}
  \item{j}{indices of boxplots.}
  \item{fullrange}{x-coords as 'uniform' in \eqn{[0,fullrange]};
    (f.=100, corresponds to Splus 3.x (x = 1,2)).}
}
\value{
  a numeric vector of length \code{n}, with values inside \eqn{(0,M)} where
  \eqn{M = } \code{fullrange}.
}
\author{Martin Maechler}
\seealso{\code{\link{boxplot}}.}
\examples{
 u.boxplot.x(7) # == 8.93 22.62 36.3 ... 91.07
}
\keyword{dplot}
\keyword{utilities}
