\name{compact01}
\alias{compact01}
\title{Compact Representation of Binary, 0/1 Values}
\description{
  Of a \code{0-1} i.e., binary vector, using \code{\link{symnum}(z == 1)},
  produce a compact \dQuote{visual} \code{\link{character}} representation
  for human consumption.

  No rocket science, but a nice example of combining basic \R idioms.
}
\usage{
compact01(z, ncol = 50L)
}
\arguments{
  \item{z}{a vector, typically of \code{0}s and \code{1}s, but more
    generally of \code{1}s and \dQuote{everything else} which you'd want to
    show visually.  Inside \code{compact01(z, *)}, only \code{z == 1} is used.}
  \item{ncol}{a positive integer, specifying the number of \dQuote{columns} of the result.}
}
\value{
  a \code{\link{character}} matrix \code{Ch}, \eqn{K \times 1}{K x 1},
  where each row is a string of \code{"."} and \code{"|"} of string length
  typically \code{ncol}, but for short \code{z}, will have
  \code{\link{nchar}(Ch[1,1]) == min(ncol, length(z))}.
}
\author{Martin Maechler}
\seealso{
  \code{\link{symnum}}
}
\examples{
set.seed(12)
z <- rbinom(120, size = 1, prob = 1/4)
(compact01(z) -> Ch)
 compact01(z, 20)
## "visualize"  a TRUE/FALSE property
x <- rnorm(150)
compact01(abs(x) > 1.5)
stopifnot(identical(0:1, dim(compact01(integer()))))
}
\keyword{utilities}
