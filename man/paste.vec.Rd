\name{paste.vec}
\alias{paste.vec}
\title{Utility for 'Showing' S vectors}
\description{
  A simple utility for displaying simple S vectors;
  can be used as debugging utility.
}
\usage{
paste.vec(name, digits = options()$digits)
}
\arguments{
  \item{name}{string with an variable name which must exist in the
    current environment (\R session).}
  \item{digits}{how many decimal digits to be used; passed to
    \code{\link{format}}.}
}
\value{
  a string of the form "NAME =  x1 x2 ..."
}
\author{Martin Maechler, about 1992.}
\examples{
 x <- 1:4
 paste.vec(x)   ##->  "x = 1 2 3 4"
}
\keyword{utilities}
