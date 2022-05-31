\name{hatMat}
\alias{hatMat}
\title{Hat Matrix of a Smoother}
\description{
  Compute the hat matrix or smoother matrix, of \sQuote{any} (linear) smoother,
  smoothing splines, by default.
}
\usage{
hatMat(x, trace= FALSE,
       pred.sm = function(x, y, ...)
                 predict(smooth.spline(x, y, ...), x = x)$y,
       \dots)
}
\arguments{
  \item{x}{numeric vector or matrix.}
  \item{trace}{logical indicating if the whole hat matrix, or only its
    trace, i.e. the sum of the diagonal values should be computed.}
  \item{pred.sm}{a function of at least two arguments \code{(x,y)}
    which returns fitted values, i.e. \eqn{\hat{y}}{y.hat}, of length
    compatible to \code{x} (and \code{y}).}
  \item{\dots}{optionally further arguments to the smoother function
    \code{pred.sm}.}
}
\value{
  The hat matrix \eqn{H} (if \code{trace = FALSE} as per default) or
  a number, \eqn{tr(H)}, the \emph{trace} of \eqn{H}, i.e.,
  \eqn{\sum_i H_{ii}}{sum(i) H[i,i]}.

  Note that \code{dim(H) == c(n, n)} where \code{n <- length(x)} also in
  the case where some x values are duplicated (aka \emph{ties}).
}
\references{
  Hastie and Tibshirani (1990).
  \emph{Generalized Additive Models}.
  Chapman & Hall.
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch}}
\seealso{\code{\link{smooth.spline}}, etc.
  Note the demo, \code{demo("hatmat-ex")}.
}
\examples{
require(stats) # for smooth.spline() or loess()

x1 <- c(1:4, 7:12)
H1 <- hatMat(x1, spar = 0.5) # default : smooth.spline()

matplot(x1, H1, type = "l", main = "columns of smoother hat matrix")

## Example 'pred.sm' arguments for hatMat() :
pspl <-  function(x,y,...) predict(smooth.spline(x,y, ...), x = x)$y
pksm <-  function(x,y,...) ksmooth(sort(x),y, "normal", x.points=x, ...)$y
## Rather than ksmooth():
if(require("lokern"))
  pksm2 <- function(x,y,...) glkerns(x,y, x.out=x, ...)$est

% pRmean <- function(x,y,...) run.mean(y, ...)
% pRline <- function(x,y,...) run.line(x,y, ...)$y

## Explaining 'trace = TRUE'
all.equal(sum(diag((hatMat(c(1:4, 7:12), df = 4)))),
                    hatMat(c(1:4, 7:12), df = 4, trace = TRUE), tol = 1e-12)

## ksmooth() :
Hk <- hatMat(x1, pr = pksm, bandwidth = 2)
cat(sprintf("df = \%.2f\\n", sum(diag(Hk))))
image(Hk)
Matrix::printSpMatrix(as(round(Hk, 2), "sparseMatrix"))

##--->  see demo("hatmat-ex")  for more (and larger) examples
%% should test these also for x with ties
}
\keyword{smooth}
\keyword{regression}
