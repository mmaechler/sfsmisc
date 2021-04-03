\name{posdefify}
\alias{posdefify}
\title{Find a Close Positive Definite Matrix}
\description{
  From a matrix \code{m}, construct a \emph{"close"} positive definite
  one.
}
\usage{
posdefify(m, method = c("someEVadd", "allEVadd"),
          symmetric = TRUE, eigen.m = eigen(m, symmetric= symmetric),
          eps.ev = 1e-07)
}
\arguments{
  \item{m}{a numeric (square) matrix.}
  \item{method}{a string specifying the method to apply; can be abbreviated.}
  \item{symmetric}{logical, simply passed to \code{\link{eigen}} (unless
    \code{eigen.m} is specified); currently, we do not see any reason
    for \emph{not} using \code{TRUE}.}
  \item{eigen.m}{the \code{\link{eigen}} value decomposition of
    \code{m}, can be specified in case it is already available.}
  \item{eps.ev}{number specifying the tolerance to use, see Details
    below.}
}
\details{
  We form the eigen decomposition
  \deqn{m = V \Lambda V'}{m = V L V'} where \eqn{\Lambda}{L} is the
  diagonal matrix of eigenvalues, \eqn{\Lambda_{j,j} = \lambda_j}{L[j,j]
    = l[j]}, with \emph{decreasing} eigenvalues \eqn{\lambda_1 \ge
    \lambda_2 \ge \ldots \ge \lambda_n}{l[1] >= l[2] >= ... >= l[n]}.

  When the smallest eigenvalue \eqn{\lambda_n}{l[n]} are less than
  \code{Eps <- eps.ev * abs(lambda[1])}, i.e., negative or \dQuote{almost
    zero}, some or all eigenvalues are replaced by \emph{positive}
  (\code{>= Eps}) values,
  \eqn{\tilde\Lambda_{j,j} = \tilde\lambda_j}{L~[j,j] = l~[j]}.
  Then, \eqn{\tilde m = V \tilde\Lambda V'}{m~ = V L~ V'} is computed
  and rescaled in order to keep the original diagonal (where that is
  \code{>= Eps}).
}
\value{
  a matrix of the same dimensions and the \dQuote{same} diagonal
  (i.e. \code{\link{diag}}) as \code{m} but with the property to
  be positive definite.
}
\author{Martin Maechler, July 2004}
\note{As we found out, there are more sophisticated algorithms to solve
  this and related problems.  See the references and the
  \code{\link[Matrix]{nearPD}()} function in the \CRANpkg{Matrix} package.
  We consider \code{nearPD()} to also be the successor of this package's \code{\link{nearcor}()}.
}
\references{
  Section 4.4.2 of
  Gill, P.~E., Murray, W. and Wright, M.~H. (1981)
  \emph{Practical Optimization}, Academic Press.

  Cheng, Sheung Hun and Higham, Nick (1998)
  A Modified Cholesky Algorithm Based on a Symmetric Indefinite Factorization;
  \emph{SIAM J. Matrix Anal.\ Appl.}, \bold{19}, 1097--1110.

  Knol DL, ten Berge JMF (1989)
  Least-squares approximation of an improper correlation matrix by a
  proper one.
  \emph{Psychometrika} \bold{54}, 53--61.

  Highham (2002)
  Computing the nearest correlation matrix - a problem from finance;
  \emph{IMA Journal of Numerical Analysis} \bold{22}, 329--343.

  Lucas (2001)
  Computing nearest covariance and correlation matrices.  A thesis
  submitted to the University of Manchester for the degree of Master of
  Science in the Faculty of Science and Engeneering.
}
\seealso{\code{\link{eigen}} on which the current methods rely.
  \code{\link[Matrix]{nearPD}()} in the \CRANpkg{Matrix} package.
  (Further, the deprecated \code{\link{nearcor}()} from this package.)
}
\examples{
 set.seed(12)
 m <- matrix(round(rnorm(25),2), 5, 5); m <- 1+ m + t(m); diag(m) <- diag(m) + 4
 m
 posdefify(m)
 1000 * zapsmall(m - posdefify(m))
}
\keyword{algebra}
\keyword{array}
