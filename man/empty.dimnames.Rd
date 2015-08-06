\name{empty.dimnames}
\alias{empty.dimnames}
\title{Empty Dimnames of an Array}
\description{
  Remove all dimension names from an array for compact printing.
}
\usage{
empty.dimnames(a)
}
\arguments{
  \item{a}{an \code{\link{array}}, i.e., as special case a matrix.}
}
\value{
  Returns \code{a} with its dimnames replaced by empty character strings.
}
\author{Bill Venables / Martin Maechler, Sept 1993.}
\seealso{\code{\link{unname}} \emph{removes} the dimnames.}
\examples{
empty.dimnames(diag(5)) # looks much nicer

(a <- matrix(-9:10, 4,5))
empty.dimnames(a) # nicer, right?
}
\keyword{array}
\keyword{print}
\keyword{utilities}
