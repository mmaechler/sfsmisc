\name{p.res.2fact}
\alias{p.res.2fact}
\title{Plot Numeric (e.g. Residuals) vs 2 Factors Using Boxplots}
\description{
  Plots a numeric (``residual like'') variable against two factor
  covariates, using boxplots.
}
\usage{
p.res.2fact(x, y, z, restricted, notch = FALSE,
            xlab = NULL, ylab = NULL, main = NULL)
}
\arguments{
  \item{x,y}{two factors or numeric vectors giving the levels of factors.}
  \item{z}{numeric vector of same length as \code{x} and \code{y},
    typically residuals.}
  \item{restricted}{positive value which truncates the size.  The
    corresponding symbols are marked by stars.}
  \item{notch}{logical indicating if the boxplots should be notched, see
    \code{\link{boxplot}(*,notch)}.}
  \item{xlab,ylab}{axis labels, see \code{\link{plot.default}}, per
    default the actual argument expressions.}
  \item{main}{main title passed to \code{plot}, defaulting to the
    deparsed \code{z} argument.}
}
\details{
  if values \emph{are} restricted, this make use of the auxiliar
  function \code{\link{u.boxplot.x}}.
}
\author{Lorenz Gygax \email{logyg@wild.unizh.ch} and Martin Maechler, Jan.95;
  starting from \code{\link{p.res.2x}()}.
}
\seealso{\code{\link{p.res.2x}}, \code{\link{boxplot}},
  \code{\link{plot.lm}}, \code{\link{TA.plot}}.
}
\examples{
I <- 8; J <- 3; K <- 20
xx <- factor(rep(rep(1:I, rep(K,I)),J))
yy <- factor(rep(1:J, rep(I*K,J)))
zz <- rt(I*J*K, df=5) #-- Student t with 5 d.f.
p.res.2fact(xx,yy,zz, restr= 4, main= "i.i.d. t <- 5 random  |.| <= 4")
mtext("p.res.2fact(xx,yy,zz, restr= 4, ..)",
      line=2, adj=1, outer=TRUE, cex=1)

## Real data
data(warpbreaks)
(fm1 <- lm(breaks ~ wool*tension, data = warpbreaks))
with(warpbreaks,
     p.res.2fact(wool, tension, residuals(fm1)))
}
\keyword{hplot}
\keyword{regression}
