\name{cairoSwd}
\alias{cairoSwd}
\title{Cairo PDF Graphics Device useful for Sweave}
\description{
  Provides a graphics device for Sweave, based on
  \code{\link{cairo_pdf}}. The advantage of \code{cairoSwd()} compared
  to \code{\link{pdf}()} is its support of Unicode characters.
}
\usage{
cairoSwd(name, width, height, ...)
}
\arguments{
  \item{name}{file name prefix to which \file{.pdf} will be appended.}
  \item{width, height}{in inches, see \code{\link{cairo_pdf}}.}

  \item{\dots}{further arguments, passed to \code{\link{cairo_pdf}()}}
}
\note{
  Sweave devices need to have an argument list as above.

  Usage in a Sweave chunk:
  \preformatted{
<<some-plot, fig=TRUE, grdevice=cairoSwd>>=
}
}
\author{Alain Hauser}
%% \details{
%% }
%% \references{
%% }
\seealso{
  \code{\link{pdf}}, \code{\link{cairo_pdf}}, \code{\link{Sweave}}.
}
%% \examples{
%% }
\keyword{device}

