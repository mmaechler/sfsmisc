\name{diagX}
\alias{diagX}
\title{The ``Other'' Diagonal Matrix}
\description{
  Compute the \emph{other} diagonal identity matrix.
  The result is basically a \emph{fast} version of \code{diag(n)[, n:1]}.
}
\usage{
diagX(n)
}
\arguments{
  \item{n}{positive integer.}
}
\value{
  a numeric \eqn{n \times n}{n * n} matrix with many zeros -- apart from
  \code{1}s in the \emph{other} diagonal.
}
\author{Martin Maechler, 1992.}
\seealso{\code{\link{diag}}.}
\examples{
diagX(4)
for(m in 1:5)
  stopifnot(identical(diagX(m), diag(m)[, m:1, drop = FALSE]))
}
\keyword{array}
\keyword{utilities}
