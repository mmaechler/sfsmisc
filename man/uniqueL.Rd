\name{uniqueL}
\alias{uniqueL}
\title{A Reversable Version of unique()}
\description{
  A version of \code{\link{unique}} keeping enough information to
  reverse (or \emph{invert}) to the original data.
}
\usage{
uniqueL(x, isuniq = !duplicated(x))
}
\arguments{
  \item{x}{numeric vector, of length \code{n}, say.}
  \item{isuniq}{logical vector of the same length as \code{x}.  For the
    reversion to work this should select at least all unique values of
    \code{x}.}
}
\value{
  list of two components,
  \item{ix}{integer vector of indices}
  \item{xU}{vector of values from \code{x}}
  such that both \code{x[isuniq] === xU} and \code{xU[ix] === x}.
}
\author{Martin Maechler}
\seealso{\code{\link{unique}}, \code{\link{duplicated}}.}
\examples{
 x0 <- c(1:3,2:7,8:4)
 str(r0 <- uniqueL(x0))
 with(r0, xU[ix]) ## == x0 !
\testonly{
 for(n in 1:100) {
   x0 <- round((1+rpois(1,lam=1))*rnorm(40))
   r0 <- uniqueL(x0)
   stopifnot(sort(unique(x0)) == r0$xU,
	     with(r0, xU[ix]) == x0)
 }
}
}
\keyword{utilities}
