\name{primes}
\alias{primes}
\title{Find all Primes Less Than n}
\description{
    Find all prime numbers aka \sQuote{primes} less than \eqn{n}.

    Uses an obvious sieve method (and some care), working with
    \code{\link{logical}} and and \code{\link{integer}}s to be quite fast.
}
\usage{
primes(n, pSeq = NULL)
}
\arguments{
  \item{n}{a (typically positive integer) number.}
  \item{pSeq}{optionally a vector of primes (2,3,5,...) as if from a
    \code{primes()} call; \bold{must} be correct.
    The goal is a speedup, but currently we have not found one single
    case, where using a non-NULL \code{pSeq} is faster.}
}
\details{
  As the function only uses \code{\link{max}(n)}, \code{n} can also be a
  \emph{vector} of numbers.

  The famous prime number theorem states that \eqn{\pi(n)}, the
  \emph{number} of primes below \eqn{n} is asymptotically \eqn{n /
  \log(n)} in the sense that \eqn{\lim_{n \to \infty}{\pi(n) \cdot \log(n) /
  n \sim 1}}{lim[n -> Inf] \pi(n) * log(n) / n  ~ 1}.

  Equivalently, the inverse of \eqn{pi()}, the \eqn{n}-th prime number
  \eqn{p_n} is around \eqn{n \log n}; recent results (Pierre Dusart, 1999),
  prove that
  \deqn{\log n + \log\log n - 1 < \frac{p_n}{n} < \log n + \log \log n
    \quad\mathrm{for } n \ge 6.}{%
  log n + log log n - 1  <  p_n / n  < log n + log log n   for n >= 6.}
}
\value{
  numeric vector of all prime numbers \eqn{\le n}{<= n}.
}
\author{Bill Venables (<= 2001); Martin Maechler gained another 40\% speed,
  carefully working with logicals and integers.
}
\seealso{
  \code{\link{factorize}}.  For large \eqn{n}, use the \pkg{gmp} package
  and its \code{\link[gmp]{isprime}} and \code{\link[gmp]{nextprime}}
  functions.
}
\examples{
 (p1 <- primes(100))
 system.time(p1k <- primes(1000)) # still lightning fast
 stopifnot(length(p1k) == 168)
\donttest{
 system.time(p.e7 <- primes(1e7)) # still only 0.3 sec (2015 (i7))
 stopifnot(length(p.e7) == 664579)
 ## The famous  pi(n) :=  number of primes <= n:
 pi.n <- approxfun(p.e7, seq_along(p.e7), method = "constant")
 pi.n(c(10, 100, 1000)) # 4 25 168
 plot(pi.n, 2, 1e7, n = 1024, log="xy", axes = FALSE,
      xlab = "n", ylab = quote(pi(n)),
      main = quote("The prime number function " ~ pi(n)))
 eaxis(1); eaxis(2)
}
## Exploring  p(n) := the n-th prime number :
## pnn(n) := log n + log log n
pnn <- function(n) { L <- log(n); L + log(L) }
n <- 6:(N <- length(PR <- primes(1e5)))
m.pn <- cbind(l.pn = ceiling(n*(pnn(n)-1)), pn = PR[n], u.pn = floor(n*pnn(n)))
matplot(n, m.pn, type="l", ylab = quote(p[n]), main = quote(p[n] ~~
        "with lower/upper bounds" ~ n*(log(n) + log(log(n)) -(1~"or"~0))))
plot(n, PR[n]/n - (pnn(n)-1), type = 'l', cex = 1/8, log="x", xaxt="n")
eaxis(1); abline(h=0, col=adjustcolor(1, 0.5))
}
\keyword{math}
\keyword{arithmetic}
