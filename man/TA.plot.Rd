\name{TA.plot}
\alias{TA.plot}
\title{Tukey-Anscombe plot (residual ~ fitted) of a linear model fit.
}
\description{
From a linear (or glm) model fitted, produce the so-called Tukey-Anscombe
plot.  Useful (optional) additions include:   0-line, lowess smooth, 
2sigma lines,  and automatic labeling of observations.
}
\usage{
TA.plot(lm.res, 
          fit= fitted(lm.res), res= residuals(lm.res, "pearson"),
          labels= NULL, main= mk.main(), draw.smooth= n >= 10,
          show.call= T, show.2sigma= T, lo.iter = NULL, lo.cex=0.8, \dots)
}
\arguments{
  \item{lm.res}{Result of lm(..), aov(..), glm(..) or a similar object.}
  \item{fit}{fitted values; you probably want the default here.}
  \item{res}{residuals to use. Default: WEIGHTED ("Pearson") residuals if
    weights have been used for the model fit.}
  \item{labels}{strings to use as plotting symbols for each point.
    Default(NULL): extract observations' names or use its sequence number.
    Use, e.g., "*" to get simple \code{*} symbols.
  }
  \item{main}{main title to plot. Default: sophisticated, resulting in
    something like "Tukey-Anscombe Plot of :   y \~ x"  constructed from
    \code{call(.)} in \code{lm.res}. 
  }
  \item{draw.smooth}{logical; if \code{T}, draw a \code{lowess} smoother
    (with automatic smoothing fraction).}
  \item{show.call}{logical; if \code{T}, write the "call"ing syntax with
    which the fit was done.}
  \item{show.2sigma}{logical; if \code{T}, draw horizontal lines at +- 2
    sigma (sigma := mad(resid)).}
  \item{lo.iter}{positive integer, giving the number of lowess
    robustness iterations.}
  \item{lo.cex}{character expansion ("cex") for  lowess and other
    marginal texts.}
  \item{...}{further graphical parameters  are passed to  \code{n.plot(.)}.}
}
\section{Side Effects}{
  The above mentioned plot is produced on the current graphic device.
}
\author{
Martin Maechler, Seminar fuer Statistik, ETH Zurich, Switzerland;
\email{maechler@stat.math.ethz.ch}
}
\seealso{\code{\link{plot.lm}}
}
\examples{
data(stackloss)
TA.plot(lm(stack.loss ~ stack.x))

if(FALSE) {
 TA.plot(lm(Fuel ~ . , fuel.frame))
 TA.plot(lm(Fuel ~ . , fuel.frame), label='o') #-- plot symbol instead of label
 TA.plot(lm(cost ~ age+ type+ car.age, claims, weights=number, na.action=na.omit))
 ##---------- for  aov(.) : ----------------------------------
 data(gun)
 TA.plot( aov(Rounds ~ Method + Physique/Team, data = gun))
 ###---------- Not so clear what it means for GLM, but: ------
 data(solder)
 TA.plot(glm(skips ~ ., data = solder, family = poisson), cex= .6)
 data(kyphosis)
 TA.plot(glm(Kyphosis ~ poly(Age,2) + Start, data=kyphosis, family = binomial),
	 cex=.75) # smaller title and plotting characters
}
}
\keyword{diagnostics}
\keyword{lm}
% Converted by Sd2Rd version 0.3-3.
