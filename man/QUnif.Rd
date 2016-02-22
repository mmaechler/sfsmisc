\name{QUnif}
\title{Quasi Randum Numbers via Halton Sequences}
\alias{QUnif}
\alias{sHalton}
%
\concept{Quasi Monte Carlo}
\concept{low discrepancy sequence}
\concept{space filling}
%
\description{
  These functions provide quasi random numbers or \emph{space filling} or
  \emph{low discrepancy} sequences in the \eqn{p}-dimensional unit cube.
}
\usage{
sHalton(n.max, n.min = 1, base = 2, leap = 1)
 QUnif (n, min = 0, max = 1, n.min = 1, p, leap = 1, silent = FALSE)
}
\arguments{
  \item{n.max}{maximal (sequence) number.}
  \item{n.min}{minimal sequence number.}
  \item{n}{number of \eqn{p}-dimensional points generated in
    \code{QUnif}.  By default, \code{n.min = 1, leap = 1} and
    the maximal sequence number is \code{n.max = n.min + (n-1)*leap}.}
  \item{base}{integer \eqn{\ge 2}{>= 2}: The base with respect to which
    the Halton sequence is built.}
  \item{min, max}{lower and upper limits of the univariate intervals.
    Must be of length 1 or \code{p}.}
  \item{p}{dimensionality of space (the unit cube) in which points are
    generated.}
  \item{leap}{integer indicating (if \eqn{> 1}) if the series should be
    leaped, i.e., only every \code{leap}th entry should be taken.}
  \item{silent}{logical asking to suppress the message about enlarging
    the prime table for large \code{p}.}
}
\value{
  \code{sHalton(n,m)} returns a numeric vector of length \code{n-m+1} of
  values in \eqn{[0,1]}.

  \code{QUnif(n, min, max, n.min, p=p)} generates \code{n-n.min+1}
  p-dimensional points in \eqn{[min,max]^p} returning a numeric matrix
  with p columns.
}
\note{
  For \code{leap} Kocis and Whiten recommend values of
  \eqn{L=31,61,149,409}, and particularly the \eqn{L=409} for dimensions
  up to 400.
}
\references{
  James Gentle (1998)
  \emph{Random Number Generation and Monte Carlo Simulation}; sec.\ 6.3.
  Springer.

  Kocis, L. and Whiten, W.J. (1997)
  Computational Investigations of Low-Discrepancy Sequences.
  \emph{ACM Transactions of Mathematical Software} \bold{23}, 2, 266--294.
}
\author{Martin Maechler}
\examples{
32*sHalton(20, base=2)

stopifnot(sHalton(20, base=3, leap=2) ==
          sHalton(20, base=3)[1+2*(0:9)])
## ------- a 2D Visualization -------

Uplot <- function(xy, axes=FALSE, xlab="", ylab="", ...) {
  plot(xy, xaxs="i", yaxs="i", xlim=0:1, ylim=0:1, xpd = FALSE,
       axes=axes, xlab=xlab, ylab=ylab, ...)
  box(lty=2, col="gray40")
}

do4 <- function(n, ...) {
  op <- mult.fig(4, main=paste("n =", n,": Quasi vs. (Pseudo) Random"),
                 marP=c(-2,-2,-1,0))$old.par
  on.exit(par(op))
  for(i in 1:2) {
     Uplot(QUnif(n, p=2), main="QUnif", ...)
     Uplot(cbind(runif(n), runif(n)), main="runif", ...)
  }
}
do4(100)
do4(500)
do4(1000, cex = 0.8, col="slateblue")
do4(10000, pch= ".", col="slateblue")
do4(40000, pch= ".", col="slateblue")
}
\keyword{math}
\keyword{multivariate}
\keyword{datagen}
