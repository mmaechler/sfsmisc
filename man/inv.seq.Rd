\name{inv.seq}
\alias{inv.seq}
\title{Inverse seq() -- Short Expression for Index Vector}
\description{
  Compute a short expression for a given integer vector, typically
  an index, that can be expressed shortly, using \code{\link{:}} etc.
}
\usage{
inv.seq(i)
}
\arguments{
  \item{i}{vector of (usually increasing) integers.}
}
%%\details{
%%}
\value{
  a \code{\link{call}} (\dQuote{the inside of an
    \code{\link{expression}}}) to be \code{\link{eval}()}ed to
  return the original \code{i}.
}
\author{Martin Maechler, October 1995; more elegant implementation from Tony Plate.}
\seealso{\code{\link{rle}} for another kind of integer vector coding.}
\examples{
(rr <- inv.seq(i1 <- c(3:12, 20:24, 27, 30:33)))
eval(rr)
stopifnot(eval(rr) == i1)

e2 <- expression(c(20:13, 3:12, -1:-4, 27, 30:31))
(i2 <- eval(e2))
(r2 <- inv.seq(i2))
stopifnot(all.equal(r2, e2[[1]]))

## Had {mapply()} bug in this example:
ii <- c(1:3, 6:9, 11:16)
stopifnot(identical(ii, eval(inv.seq(ii))))
}
\keyword{arith}
\keyword{utilities}
