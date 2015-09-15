\name{xy.unique.x}
\alias{xy.unique.x}
\title{Uniqify (X,Y) Values using Weights}
\description{
  Given \emph{smoother} data \eqn{(x_i, y_i)} and maybe weights \eqn{w_i},
  with multiple \eqn{x_i}, use the unique x values, replacing the
  \eqn{y}'s by their (weighted) mean and updating the weights
  accordingly.
}
\usage{
xy.unique.x(x, y, w, fun.mean = mean)
}
\arguments{
  \item{x,y}{numeric vectors of same length.  Alternatively, \code{x}
    can be a \sQuote{xy} like structure, see \code{\link{xy.coords}}.}
  \item{w}{numeric vector of non-negative weights -- or missing which
    corresponds to all weights equal.}
  \item{fun.mean}{the mean \code{\link{function}} to use.}
}
\value{
  Numeric matrix with three columns, named
  \code{x}, \code{y} and \code{w} with unique \code{x} values and
  corresponding \code{y} and weights \code{w}.
}
\author{Martin Maechler, 8 Mar 1993.}
\seealso{e.g., \code{\link{smooth.spline}} uses something like
  this internally.}
\examples{
## simple example:
xy.unique.x(c(1,1,1,2,4), 1:5)
#   x y w
# 1 1 2 3
# 2 2 4 1
# 3 4 5 1
}
\keyword{utilities}
