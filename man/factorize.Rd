\name{factorize}
\alias{factorize}
\title{Prime Factorization of Integers}
\description{
  Compute the prime factorization(s) of integer(s) \code{n}.

  % ## Purpose:  Prime factorization of integer(s) 'n'
  % ## -------------------------------------------------------------------------
  % ## Arguments: n vector of integers to factorize (into prime numbers)
  % ##	--> needs 'prime.sieve'
  % ## >> Better would be: Define class 'primefactors' and "multiply" method
  % ##			 then use this function recursively only "small" factors
  % ## -------------------------------------------------------------------------
}
\usage{
factorize(n, verbose = FALSE)
}
\arguments{
  \item{n}{vector of integers to factorize.}
  \item{verbose}{logical indicating if some progress information should
    be printed.}
}
\details{
  works via \code{\link{primes}}, currently in a cheap way, sub-optimal
  for large composite \eqn{n}.
%% FIXME: ALSO,
%% % ## >> Better would be: Define class 'primefactors' and "multiply" method
}
\value{
  A named \code{\link{list}} of the same length as \code{n},
  each element a 2-column matrix with column \code{"p"} the prime
  factors and column~\code{"m"} their respective exponents (or
  multiplities), i.e., for a prime number \code{n}, the resulting matrix
  is \code{cbind(p = n, m = 1)}.
}
\author{Martin Maechler, Jan. 1996.}
\seealso{
  \code{\link{primes}}.

  For factorization of moderately or really large numbers, see the \CRANpkg{gmp}
  package, and its \code{\link[gmp]{factorize}()}.
}
\examples{
 factorize(47)
 factorize(seq(101, 120, by=2))
}
\keyword{math}

