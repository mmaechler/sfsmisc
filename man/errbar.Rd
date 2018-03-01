\name{errbar}
\encoding{latin1}
\title{Scatter Plot with Error Bars}
\alias{errbar}
\description{
  Draws a scatter plot, adding vertical \dQuote{error bars} to all the points.
}
\usage{
errbar(x, y, yplus, yminus, cap = 0.015,
       ylim = range(y,yplus,yminus),
       xlab= deparse(substitute(x)),
       ylab= deparse(substitute(y)), \dots)
}
\arguments{
  \item{x}{vector of x values.}
  \item{y}{vector of y values.}
  \item{yplus}{vector of y values: the tops of the error bars.}
  \item{yminus}{vector of y values: the bottoms of the error bars.}
  \item{cap}{the width of the little lines at the tops and bottoms of
    the error bars in units of the width of the plot.  Default is 0.015.}
  \item{ylim}{(numeric of length 2): the y-axis extents with a sensible default.}
  \item{xlab, ylab}{axis labels for the plot, as in
    \code{\link{plot.default}}.}
  \item{\dots}{Graphical parameters (see \code{\link{par}}) may also
    be supplied as arguments to this function.}
}
\author{Originally Charles Geyer, U.Chicago, early 1991; then Martin Mächler.}
\seealso{\code{\link[Hmisc]{errbar}} in package \CRANpkg{Hmisc} is similar.
%% maybe deprecate ours?  FIXME
}
\examples{
y <- rnorm(10); d <- 1 + .1*rnorm(10)
errbar(1:10, y, y + d, y - d, main="Error Bars example")
}
\keyword{hplot}

