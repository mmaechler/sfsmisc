\name{bl.string}
\alias{bl.string}
\title{Blank String}
\description{
  Simple constructor of a ``blank'' string of given string length.
}
\usage{
bl.string(no)
}
\arguments{
  \item{no}{non-negative integer.}
}
\value{
  A ``blank'' string (i.e. \code{\link{character}(1)}) fulfilling
  \code{n == nchar(bl.string(n))}.
}
\author{Martin Maechler, early 1990's.}
\seealso{\code{\link{paste}}, \code{\link{character}}, \code{\link{nchar}}.}
\examples{
r <- sapply(0:8, function(n) ccat(bl.string(n),n))
cbind(r)

bl.string # shows the very simple function definition
}
\keyword{character}
