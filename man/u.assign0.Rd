\name{u.assign0}
\alias{u.assign0}
\alias{u.get0}
\title{`Portable' assign / get functions (R / S-plus) for `frame 0'}
\description{
  \R doesn't have S' concept of \code{frame = 0}, aka \sQuote{session
    frame}.  These two function were an attempt to provide a portable
  way for working with frame 0, particularly when porting code
  \emph{from} S.  Its author, MM, no longer recommends them.
}
\usage{
u.assign0(x, value, immediate = FALSE)
u.get0(x)
}
\arguments{
  \item{x}{character string giving the \emph{name} of the object.}
  \item{value}{any \R object which is to be assigned.}
  \item{immediate}{logical, for S compatibility.  No use in \R.}
}
\author{Martin Maechler}
\note{Really don't use these anymore...}
\seealso{\code{\link{get}}, \code{\link{assign}}.}
%
\keyword{utilities}
\keyword{environment}% System, not R
