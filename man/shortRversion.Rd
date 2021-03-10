\name{shortRversion}
\alias{shortRversion}
\title{Short R Version String}
\description{
  From \pkg{base} \R's \code{\link{R.version.string}}, produce a somewhat
  shorter version, with or without date, notably also for \emph{patched} or
  \emph{devel}opment versions of \R.

  Main use is for plotting or construction of file of variable names.
}
\usage{
shortRversion(Rv = R.version, Rst = Rv$status,
              Rvstring = if (!is.null(s <- Rv$version.string)) s else R.version.string,
              date = Rst != "", spaces = TRUE)
}
\arguments{
  \item{Rv}{a \code{\link{list}} with some of the same components as \code{\link{R.version}}.}
  \item{Rst}{a string specifying the \emph{status} of \R's version.  For
    released versions of \R, this is \code{""}; almost always use the default \code{Rv$status}.}
  \item{Rvstring}{a string with a default that should work even for \R
    versions previous to 1.0.0.}
  \item{date}{logical specifying if the date of the \R version should be
    included in the result; by default, this will be true only for
    non-released versions of \R.}
  \item{spaces}{logical indicating if the result may contain spaces (aka
    \sQuote{blanks}); setting it to false, replaces the blanks by \code{"-"} and \code{"_"}.}
}
\value{
  a \code{\link{character}} string, typically a shortened version of \code{Rvstring}.
}
\author{Martin Maechler}
\seealso{
  \code{\link{R.version}},   \code{\link{R.version.string}}
}
\examples{
shortRversion() ## (including the date, typically for an R Core developer)
## but this is shorter:
(Rver <- shortRversion(date=FALSE))
shortRversion(spaces=FALSE)# e.g. for a file of even directory name
shortRversion(spaces=FALSE, date=FALSE)# even shorter, ditto

## If you want even shorter { abbreviate() will remove spaces, too }:
abbreviate(shortRversion(), 11)
abbreviate(shortRversion(date=FALSE), 13)
}
\keyword{utilities}
