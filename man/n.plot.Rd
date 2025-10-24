\name{n.plot}
\alias{n.plot}
\title{Name Plot: Names or Numbers instead of Points in Plot}
\description{
  A utility function which basically calls \code{\link{plot}(*, type="n")}
  and \code{\link{text}}.   To have names or numbers instead of points
  in a plot is useful for identifaction, e.g., in a residual plot, see
  also \code{\link{TA.plot}}.
}
\usage{
n.plot(x, y = NULL, nam = NULL, abbr = n >= 20 || max(nchar(nam))>=8,
       type = "n", xlab = NULL, ylab = NULL, log = "",
       cex = par("cex"), col = par("col"), pt.col = par("col"), \dots)
}
\arguments{
  \item{x, y}{coordinates at which to plot. If \code{y} is missing,
    \code{x} is used for both, if it's a \code{\link{data.frame},
      \link{list}}, 2-column matrix etc -- via \code{\link{xy.coords}};
    formula do \bold{not} work.}
  \item{nam}{the labels to plot at each (x,y).  Per default, these
    taken from the data \code{x} and \code{y};  case numbers \code{1:n}
    are taken if no names are available.}
  \item{abbr}{logical indicating if the \code{nam} labels should be
    abbreviated -- with a sensible default.}
  \item{type}{passed to \code{plot.default},a string denoting the type of
    the initial \code{\link{plot()}} call; the default draws nothing but
    the axes, etc; \code{type = "l"} maybe useful occasionally.}
  \item{xlab, ylab}{labels for the x- and y- axis, the latter being empty
    by default.}
  \item{log}{character specifying if log scaled axes should be used, see
    \code{\link{plot.default}}.}
  \item{cex}{plotting character expansion, see \code{\link{par}}.}
  \item{col}{color to use for \code{\link{text}()}.}
  \item{pt.col}{color to use for \code{\link{plot}()} \emph{if} \code{type} is
    set different to \code{"n"}.}
  \item{\dots}{further arguments to be passed to the \code{\link{plot}} call.}
}
\value{
  invisibly, a character vector with the labels used.
}
\author{Martin Maechler, since 1992}
\seealso{\code{\link{plot.default}}, \code{\link{text}}.}
\examples{
 n.plot(1:20, cumsum(rnorm(20)))
 data(cars)
 with(cars, n.plot(speed, dist, cex = 0.8, col = "forest green"))

## visualize periodicity of  exp(<complex>)   {pass args to plot.default() via `...`}
cpxN <- function(r,i) setNames(complex(real=r, imaginary=i), i)
n.plot(exp(cpxN(2, 0:32)), type = "l", pt.col = "gray", asp = 1,
       col = adjustcolor(rainbow(33), offset = rep(-1/4, 4)),
       panel.first = abline(h=0, v=0, lty=3, col="gray30"))
symbols(0,0, circles = exp(2), inches=FALSE, fg = adjustcolor(2, 1/3), lwd=3, add=TRUE)
}
\keyword{hplot}
