\name{pmax.sa}
\alias{pmax.sa}
\alias{pmin.sa}
\title{Parallel Maxima / Minima (Scalar, Array)}
\description{
  These are versions of \code{\link{pmax}} and \code{\link{pmin}}
  which return (more-dimensional) arrays.
}
\usage{
pmax.sa(scalar, arr)
pmin.sa(scalar, arr)
}
\arguments{
  \item{scalar}{numeric scalar.}
  \item{arr}{any numeric \R object, typically array.}
}
\value{
  an array with the same \code{\link{dim}} and \code{\link{dimnames}} as
  the input \code{arr}.
}
\author{Martin Maechler}
\seealso{\code{\link{pmin}}.}
\examples{
m <- cbind(a=1:5, b=3:7, c=-2:2)
pmax   (3, m) # < dropping all attributes
pmax.sa(3, m) # matrix with dimnames

## no "added value" for simple case:
x <- rpois(20, 7)
stopifnot(identical(pmin   (2.1, x),
		    pmin.sa(2.1, x)),
	  identical(pmax   (2.1, 3),
		    pmax.sa(2.1, 3)))
}
\keyword{array}
\keyword{utilities}
