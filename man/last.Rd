\name{last}
\alias{last}
\title{Get Last Elements of a Vector}
\description{
  Extract the last elements of a vector.
}
\usage{
last(x, length.out = 1, na.rm = FALSE)
}
\arguments{
  \item{x}{any vector.}
  \item{length.out}{integer indicating how many element are desired.  If
    positive, return the \code{length.out} last elements of \code{x}; if
    negative, the last \code{length.out} elements are \emph{dropped}.
  }
  \item{na.rm}{logical indicating if the last non-missing value (if any)
    shall be returned.  By default (it is \code{FALSE} and) the last
    elements (whatever its values) are returned.}
}
\value{
  a vector of length \code{abs(length.out)} of \emph{last} values from \code{x}.
}
\author{Werner Stahel (\email{stahel@stat.math.ethz.ch}), and independently,
        Philippe Grosjean (\email{phgrosjean@sciviews.org}),
        Frédéric Ibanez (\email{ibanez@obs-vlfr.fr}).}
\note{
  This function may eventually be deprecated for the standard \R
  function \code{\link{tail}()}.

  Useful for the \code{\link[pastecs]{turnogram}()} function in package
  \CRANpkg{pastecs}.
}

\seealso{\code{\link[pastecs]{first}}, \code{\link[pastecs]{turnogram}}
}
\examples{
a <- c(NA, 1, 2, NA, 3, 4, NA)
last(a)
last(a, na.rm=TRUE)

last(a, length = 2)
last(a, length = -3)
}
\keyword{ manip }
