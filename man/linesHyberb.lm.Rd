\name{linesHyperb.lm}
\alias{linesHyperb.lm}
\title{Plot Confidence or Prediction Hyperbolas around a Regression Line}
\description{
  Add confidence/prediction hyperbolas for \eqn{y(x_0)}
  to a plot with data or regression line.
}
\usage{
linesHyperb.lm(object, c.prob=0.95, confidence=FALSE,
            k=if (confidence) Inf else 1,
            col=2, lty=2, do.abline=TRUE)
}
\arguments{
  \item{object}{result of \code{\link{lm}(.)}.}
  \item{c.prob}{coverage probability in \eqn{(0,1)}.}
  \item{confidence}{logical; if true, do (small) confidence band, else,
    realistic prediction band for the mean of \code{k} observations.}
  \item{k}{integer or \code{Inf}; assume \code{k} future observations;
    \code{k = Inf}  corresponds to confidence intervals (for y).}
  \item{col, lty}{attributes for the \code{\link{lines}} to be drawn.}
  \item{do.abline}{logical; if true, the regression line is drawn as well.}
}
\note{
  With \code{\link{predict.lm}(*, interval=)} is available,
  this function \code{linesHyperb.lm} is only slightly more general for
  its \code{k} argument.
}
\author{Martin Maechler, Oct 1995}
\seealso{\code{\link{predict.lm}(*, interval=)} optionally computes
  prediction or confidence intervals.}
\examples{
data(swiss)
      plot(Fertility ~ Education, data = swiss) # the data
(lmS <- lm(Fertility ~ Education, data = swiss))
linesHyperb.lm(lmS)
linesHyperb.lm(lmS, conf=TRUE, col="blue")
}
\keyword{regression}
\keyword{aplot}
