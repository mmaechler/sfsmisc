\name{rrange}
\alias{rrange}
\title{Robust Range using Boxplot `Quartiles'}
\description{
  Compute a robust range, i.e. the usual \code{\link{range}()} as long
  as there are no outliers,  using the ``whisker boundaries'' of
  \code{\link{boxplot}}.
}
\usage{
rrange(x, range=1, coef = 1.5, na.rm = TRUE)
}
\arguments{
  \item{x}{numeric vector the robust range of which shall be computed.}
  \item{range}{number for S compatibility; \code{1.5 * range} is
    equivalent to \code{coef}.}
  \item{coef}{numeric multiplication factor definying the outlier
    boundary, see ``Details'' below.}
  \item{na.rm}{logical indicating how \code{\link{NA}} values should be
    handled;  they are simply dropped when \code{na.rm = TRUE} as by default.}
}
\details{
  The robust range is really just what \code{\link{boxplot.stats}(x,
    coef=coef)} returns as the whisker boundaries.
  This is the most extreme values \code{x[j]} still inside median
  plus/minus \code{coef * IQR}.
}
\value{
  numeric vector \code{c(m,M)} with \eqn{m \le M}{m <= M} which is (not
  strictly) inside \code{range(x) = c(min(x),max(x))}.
}
\author{Martin Maechler, 1990.}
\seealso{\code{\link{range}}, \code{\link{fivenum}}, \code{\link{boxplot}}.}
\examples{
stopifnot(rrange(c(1:10,1000)) == c(1,10))
}
\keyword{univar}
\keyword{robust}
