\name{primes}
\alias{primes}
\title{Find all Primes Less Than n}
\description{
    Find all prime numbers aka \sQuote{primes} less than \eqn{n}.

    Uses an obvious sieve method (and some care, working with
    \code{\link{logical}} and and \code{\link{integer}}s to be quite fast.
}
\usage{
primes(n)
}
\arguments{
  \item{n}{a (typically positive integer) number.}
}
\details{
  As the function only uses \code{\link{max}(n)}, \code{n} can also be a
  \emph{vector} of numbers.
}
\value{
  numeric vector of all prime numbers \eqn{\le n}{<= n}.
}
\author{Bill Venables (<= 2001); Martin Maechler gained another 40\% speed,
  working with logicals and integers.
}
\seealso{
  \code{\link{factorize}}.  For large \eqn{n}, use the \pkg{gmp} package
  and its \code{\link[gmp]{isprime}} and \code{\link[gmp]{nextprime}}
  functions.
}
\examples{
 (p1 <- primes(100))
 system.time(p1k <- primes(1000)) # still lightning ..
 stopifnot(length(p1k) == 168)
}
\keyword{math}
\keyword{arithmetic}
