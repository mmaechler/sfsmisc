\name{tapplySimpl}
\alias{tapplySimpl}
\title{More simplification in tapply() result}
\description{
  For the case of more than two categories or indices (in \code{INDEX}),
  traditional \code{\link{tapply}(*, simplify = TRUE)} still returns a
  list when an array may seem more useful and natural.  This is provided
  by \code{tapplySimpl()} if the function \code{FUN()} is defined such
  as to return a vector of the same length in all cases.
}
\usage{
tapplySimpl(X, INDEX, FUN, \dots)
}
\arguments{
  \item{X}{an atomic object, typically a vector.  All these arguments
    are as in \code{\link{tapply}()} and are passed to \code{tapply(..)}.}
  \item{INDEX}{list of (typically more than one) factors, each of same
    length as \code{X}.}
  \item{FUN}{the function to be applied.  For the result to be
    simplifiable, \code{FUN()} must return a vector of always the same
    length.}
  \item{\dots}{optional arguments to \code{FUN}.}
}
\value{
  If the above conditions are satisfied, the list returned from
  \code{r <- tapply(X, INDEX, FUN, \dots)} is simplified into an
  \code{\link{array}} of rank \eqn{1 + \#\{indices\}}, i.e.,
  \code{1+length(INDEX)}; otherwise, \code{tapplySimpl()} returns the list
  \code{r}, i.e., the same as \code{tapply()}.
}
\author{Martin Maechler, 14 Jun 1993 (for S-plus).}
\seealso{\code{\link{tapply}(*, simplify=TRUE)}.}
\examples{
  ## Using tapply() would give a list (with dim() of a matrix);
  ## here we get 3-array:

  data(esoph)
  with(esoph, {
       mima <<- tapplySimpl(ncases/ncontrols, list(agegp, alcgp),  range)
       stopifnot(dim(mima) == c(2, nlevels(agegp), nlevels(alcgp)))
       })
  aperm(mima)
}
\keyword{iteration}
\keyword{category}
