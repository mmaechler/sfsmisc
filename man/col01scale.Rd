\name{col01scale}
\alias{col01scale}
\alias{colcenter}
\title{Matrix Scaling Utilities}
\description{
  \code{col01scale} and \code{colcenter} (re)scale the columns of a
  matrix.  These are simple one-line utilities, mainly with a didactical
  purpose.
}
\usage{
colcenter (mat)
col01scale(mat, scale.func = function(x) diff(range(x)), location.func = mean)
}
\arguments{
  \item{mat}{numeric matrix, to rescaled.}
  \item{scale.func, location.func}{two functions mapping a numeric
    vector to a single number.}
}
\value{
  a matrix with the same attributes as the input \code{mat}.
}
\author{Martin Maechler}
\seealso{The standard \R function \code{\link{scale}()}.}
\examples{
## See the simple function definitions:

colcenter ## simply one line

col01scale# almost as simple
}
\keyword{array}
