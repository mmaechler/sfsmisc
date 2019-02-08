\name{roundfixS}
\alias{roundfixS}
\concept{apportionment}
\title{Round to Integer Keeping the Sum Fixed}
\description{
  Given a real numbers \eqn{y_i} with the particular property that
  \eqn{\sum_i y_i} is integer, find \emph{integer} numbers \eqn{x_i}
  which are close to \eqn{y_i} (\eqn{\left|x_i - y_i\right| < 1 \forall i}{%
    |x[i] - y[i]| < 1 for all i}), and have identical \dQuote{marginal}
  sum, \code{sum(x) == sum(y)}.

  As I found later, the problem is known as \dQuote{Apportionment Problem}
  and it is quite an old problem with several solution methods proposed
  historically, but only in 1982, Balinski and Young proved that there
  is no method that fulfills three natural desiderata.

  Note that the (first) three methods currently available here were all
  (re?)-invented by M.Maechler, without any knowledge of the
  litterature.  At the time of writing, I have not even checked to which
  (if any) of the historical methods they match.
}
\usage{
roundfixS(x, method = c("offset-round", "round+fix", "1greedy"))
}
\arguments{
  \item{x}{a numeric vector which \bold{must} sum to an integer}
  \item{method}{character string specifying the algorithm to be used.}
%   \item{trace}{logical or integer, enabling algorithm tracing.}
}
\details{
  Without hindsight, it may be surprising that all three methods give
  identical results (in all situations and simulations considered),
  notably that the idea of \sQuote{mass shifting} employed in the
  iterative \code{"1greedy"} algorithm seems equivalent to the much simpler
  idea used in \code{"offset-round"}.

  I am pretty sure that these algorithms solve the \eqn{L_p}
  optimization problem, \eqn{\min_x \left\|y - x\right\|_p}{min_x ||y - x||_p},
  typically for all \eqn{p \in [1,\infty]}{p in [1,Inf]}
  \emph{simultaneously}, but have not bothered to find a formal proof.
}
\value{
  a numeric vector, say \code{r}, of the same length as \code{x}, but
  with integer values and fulfulling \code{sum(r) == sum(x)}.
}
\author{Martin Maechler, November 2007}
\references{
  Michel Balinski and H. Peyton Young (1982)
  \bold{Fair Representation: Meeting the Ideal of One Man, One Vote};

  \url{https://en.wikipedia.org/wiki/Apportionment_paradox}

  \url{https://www.ams.org/samplings/feature-column/fcarc-apportionii3}
}
\seealso{\code{\link{round}} etc
}
\examples{
## trivial example
kk <- c(0,1,7)
stopifnot(identical(kk, roundfixS(kk))) # failed at some point

x <- c(-1.4, -1, 0.244, 0.493, 1.222, 1.222, 2, 2, 2.2, 2.444, 3.625, 3.95)
sum(x) # an integer
r <- roundfixS(x)
stopifnot(all.equal(sum(r), sum(x)))
m <- cbind(x=x, `r2i(x)` = r, resid = x - r, `|res|` = abs(x-r))
rbind(m, c(colSums(m[,1:2]), 0, sum(abs(m[,"|res|"]))))

chk <- function(y) {
  cat("sum(y) =", format(S <- sum(y)),"\n")
  r2  <- roundfixS(y, method="offset")
  r2. <- roundfixS(y, method="round")
  r2_ <- roundfixS(y, method="1g")
  stopifnot(all.equal(sum(r2 ), S),
            all.equal(sum(r2.), S),
            all.equal(sum(r2_), S))
  all(r2 == r2. & r2. == r2_) # TRUE if all give the same result
}

makeIntSum <- function(y) {
   n <- length(y)
   y[n] <- ceiling(y[n]) - (sum(y[-n]) \%\% 1)
   y
}
set.seed(11)
y <- makeIntSum(rnorm(100))
chk(y)

## nastier example:
set.seed(7)
y <- makeIntSum(rpois(100, 10) + c(runif(75, min= 0, max=.2),
                                   runif(25, min=.5, max=.9)))
chk(y)

\dontrun{
for(i in 1:1000)
    stopifnot(chk(makeIntSum(rpois(100, 10) +
                             c(runif(75, min= 0, max=.2),
                               runif(25, min=.5, max=.9)))))
}
}
\keyword{arith}
\keyword{manip}
