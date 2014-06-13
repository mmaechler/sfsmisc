\name{p.res.2x}
\title{Stahel's Residual Plot against 2 X's}
\alias{p.res.2x}
\alias{p.res.2x.default}
\alias{p.res.2x.formula}
\description{
  Plot Residuals (e.g., of a multiple linear regression) against two
  (predictor) variables.  This is now a (S3) \emph{generic} function
  with a \code{default} and a \code{\link{formula}} method.
}
\usage{
p.res.2x(x, \dots)

\S3method{p.res.2x}{default}(x, y, z, restricted, size = 1, slwd = 1, scol = 2:3,
         xlab = NULL, ylab = NULL, main = NULL,
         xlim = range(x), ylim = range(y), \dots)

\S3method{p.res.2x}{formula}(x = ~., data, main = deparse(substitute(data)),
         xlab = NULL, ylab = NULL, \dots)
}
\arguments{
  \item{x, y}{numeric vectors of the same length specifying 2
   covariates.  For the \code{formula} method, \code{x} is a \code{\link{formula}}.}
  \item{z}{numeric vector of same length as \code{x} and \code{y},
    typically residuals.}
  \item{restricted}{positive value which truncates the size.  The
    corresponding symbols are marked by stars.}
  \item{size}{the symbols are scaled so that \code{size} is the size of
    the largest symbol in cm.}
  \item{slwd, scol}{line width and color(s) for the residual
    \code{\link{segments}}.  If \code{scol} has length 2 as per default,
    the two colors are used for positive and negative \code{z} values,
    respectively.}
  \item{xlab, ylab, main}{axis labels, and title see \code{\link{title}},
    each with a sensible default.  To suppress, use, e.g., \code{main = ""}.}
  \item{xlim, ylim}{the basic x- and y- axis extents, see
    \code{\link{plot.default}}.  Note that these will be slightly
    extended such that segments are not cut off.}
  \item{\dots}{further arguments passed to \code{plot}, or
    \code{p.res.2x.default()}, respectively.}

  \item{data}{(for the \code{\link{formula}} method:) a data frame or a fitted
    \code{"\link{lm}"} object.}
}
\details{
  The formula interface will call \code{\link{p.res.2fact}()} when
    \emph{both} \code{x} and \code{y} are \code{\link{factor}}s.

  ...........
  ..........
}
\references{Stahel, W. (1996) ........}
\author{Andreas Ruckstuhl in June 1991 and
  Martin Maechler, in 1992, '94, 2003-4.}
\seealso{\code{\link{p.res.2fact}},
  \code{\link{plot.lm}},
  \code{\link{TA.plot}}.
}
\examples{
xx <- rep(1:10,7)
yy <- rep(1:7, rep(10,7))
zz <- rnorm(70)
p.res.2x(xx,yy,zz, restr = 2, main = "i.i.d.  N(0,1) random residuals")
\dontshow{
 p.res.2x(xx,yy,zz, restr = 2, main = "p.res.2x(*, xlim, ylim)",
          xlim = c(2,8), ylim = c(1,4))
}
example(lm.influence, echo = FALSE)

op <- mult.fig(2, marP=c(-1,-1,-1,0), main="p.res.2x(*,*, residuals(lm.SR))")$old.par
with(LifeCycleSavings,
     { p.res.2x(pop15, ddpi, residuals(lm.SR), scol=c("red", "blue"))
       p.res.2x(pop75, dpi,  residuals(lm.SR), scol=2:1)
     })

## with formula interface:
p.res.2x(~ pop15 + ddpi, lm.SR, scol=c("red", "blue"))
p.res.2x(~ pop75 +  dpi, lm.SR, scol=2:1)

par(op) # revert par() settings above
}
\keyword{hplot}
\keyword{regression}
