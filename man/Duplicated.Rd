\name{Duplicated}
\alias{Duplicated}
\title{Counting-Generalization of duplicated()}
\description{
  Duplicated() generalizes the \code{\link{duplicated}} method for
  vectors, by returning indices of \dQuote{equivalence classes} for
  duplicated entries and returning \code{nomatch} (\code{NA} by default)
  for unique entries.

  Note that \code{duplicated()} is not \code{TRUE} for the first time a
  duplicate appears, whereas \code{Duplicated()} only marks unique
  entries with \code{nomatch} (\code{NA}).
}
\usage{
Duplicated(v, incomparables = FALSE, fromLast = FALSE, nomatch = NA_integer_)
}
\arguments{
  \item{v}{a vector, often character, factor, or numeric.}
  \item{incomparables}{a vector of values that cannot be compared,
    passed to both \code{\link{duplicated}()} and \code{\link{match}()}.
    \code{FALSE} is a special value, meaning that all values can be
    compared, and may be the only value accepted for methods other than
    the default.  It will be coerced internally to the same type as \code{x}.}
  \item{fromLast}{logical indicating if duplication should be considered
    from the reverse side, i.e., the last (or rightmost) of identical
    elements would correspond to \code{duplicated=FALSE}.}
  \item{nomatch}{passed to \code{\link{match}()}: the value to be
    returned in the case when no match is found.  Note that it is
    coerced to \code{integer}.}
}
\value{
  an integer vector of the same length as \code{v}.  Can be used as a
  \code{\link{factor}}, e.g., in \code{\link{split}},
  \code{\link{tapply}}, etc.
}
\author{Christoph Buser and Martin Maechler, Seminar fuer Statistik, ETH
  Zurich, Sep.2007}
\seealso{\code{\link{uniqueL}} (also in this \pkg{sfsmisc} package);
  \code{\link{duplicated}}, \code{\link{match}}.
}
\examples{
x <- c(9:12, 1:4, 3:6, 0:7)
data.frame(x, dup = duplicated(x),
              dupL= duplicated(x, fromLast=TRUE),
              Dup = Duplicated(x),
              DupL= Duplicated(x, fromLast=TRUE))
}
\keyword{manip}
