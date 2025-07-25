\name{gcd}
\alias{GCD}
\alias{LCM}
\title{Greatest Common Divisor  and  Least Common Multiple}
\description{
  \code{gcd(a,b)} computes the greatest common divisor of two positive
  integer numbers by Euclid's algorithm.

  \code{lcm(...)} computes the least common multiple of an arbitrary number
  of integers, iteratively applying \code{lcm(a,b) = (a * b) / gcd(a,b)}.
}
\usage{
GCD(a, b)
LCM(n, ...)
}
\arguments{
  \item{a, b}{two integer numbers.}
  \item{n, \dots}{an integer (vector or number) and possibly more; the
    \code{...} argument is for convenience, allowing e.g., \code{LCM(2,3,4)}.}
}
\value{
  a positive integer.
}
\author{Martin Maechler}
\note{
  Very simple, but too useful to spend time on, if you need it.
}
\seealso{
  \code{\link{primes}}, and \code{\link{factorize}}.
}
\examples{
GCD(12, 18)
GCD(15, 105)
GCD(84, 64)

LCM(1,2,3,4,5,6) # 60
LCM(2,3,5,7) == print(2*3*5*7) # true, of course
LCM(1:8) # 840

## the LCMs needed to get integer coefficients / N  in Taylor polynomial for log(1+x):
vapply(1:24, function(n) LCM(1:n), 1)
}
\keyword{math}
\keyword{arithmetic}
