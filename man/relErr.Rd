\name{relErr}
\title{Relative Error When Appropriate, Absolute Otherwise}
\alias{relErrV}
\alias{relErr}
\description{
  \describe{
    \item{\code{relErrV()}:}{
      Compute the signed relative error componentwise (\dQuote{vectorized})
      between the \code{target} and \code{current} vectors,
      using the \emph{absolute} error, i.e., the difference
      in case the relative error is not well defined, i.e., when \code{target}
      is zero or infinite.}
    \item{\code{relErr()}:}{simply the \emph{mean} absolute value of the relative
      errors between \code{target} and \code{current} vectors; typically
      the \dQuote{same} as \code{\link{all.equal.numeric}(target, vector, tolerance=0, countEQ=TRUE)}.

      Currently useful only when both vectors are finite.
    }
  }
}
\usage{
relErrV(target, current, eps0 = .Machine$double.xmin)
relErr (target, current)
}
\arguments{
  \item{target}{numeric, possibly scalar.}
  \item{current}{numeric vector of \code{\link{length}()} a multiple of
    \code{length(target)}.}
  \item{eps0}{non-negative number; values \code{abs(target) < eps0} should
    be treated as zero (and hence \emph{absolute} instead of relative error
    be computed).  This may be crucial when \code{target} is an
    \code{"mpfr"}-number vector.}
}
\value{
  \describe{
    \item{\code{relErrV()}:}{a numeric vector of the same length as \code{current}.}
    \item{\code{relErr()}:}{a single number.}
  }
}
\author{Martin Maechler,
  originally as part of \CRANpkg{Matrix} package's \file{test-tools.R}.
}
\seealso{
  \code{\link{all.equal.numeric}()} is similar in spirit but returns \code{TRUE} or
  string containing the \emph{mean} relative or absolute error.
}
\examples{
## relErrV() test example: showing how it works fine with {NA, Inf, 0} :
eps <- 1e-4*c(-9, -8, -6, -4, 0.5, 1, 5)
target  <- c(-1:1, 0,   0, NA, NaN, Inf, -Inf, Inf, 0 , Inf, 1 , -3:3)
current <- c(-1:1,1e-7,NaN,NA,  0 , Inf,  Inf,  0, Inf,  1, Inf, -3:3+ eps)
cbind(target, current, absE = current-target,
                       relE = relErrV(target,current)) -> M ; M
stopifnot(exprs = {
         is.logical(isFr <- is.finite(rF <- M[,"relE"]))
  target==current | isFr == is.finite(aF <- M[,"absE"])
  identical(aF[!isFr] , rF[!isFr])
  identical(numeric(), relErrV(numeric(), integer())) # length 0 {used to fail}
})
tools::assertError(relErrV(1, numeric()), verbose=TRUE) # no longer allowed
## relErr() is pretty simple --- (possibly too simple, currently)
relErr
relErr(target, current) # NA (of course)
all.equal.numeric(target, current) ## "'is.NA' value mismatch ..."

## comparison after dropping NA's :
hasN <- is.na(target) | is.na(current)
all.equal(target[!hasN], current[!hasN], tolerance=0) # "Mean abs. diff.: Inf"
   relErr(target[!hasN], current[!hasN]) # NaN  (to improve?)

## comparison after only keeping cases where both are finite:
finN <- is.finite(target) & is.finite(current)
all.equal(target[finN], current[finN], tol=0)          # "Mean abs.d.: 0.000279.."
all.equal(target[finN], current[finN], tol=0, countEQ=TRUE) #   "  " : 0.000239..
   relErr(target[finN], current[finN]) # 0.0002392929
}
\keyword{arith}
\keyword{utilities}
