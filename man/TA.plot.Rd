\name{TA.plot}
\alias{TA.plot}
\title{Tukey-Anscombe Plot (residual vs. fitted) of a Linear Model. }
\description{
  From a linear (or glm) model fitted, produce the so-called Tukey-Anscombe
  plot.  Useful (optional) additions include:   0-line, lowess smooth,
  2sigma lines,  and automatic labeling of observations.
}
\usage{
TA.plot(lm.res,
        fit= fitted(lm.res), res= residuals(lm.res, "pearson"),
        labels= NULL, main= mk.main(), xlab = "Fitted values",
        draw.smooth= n >= 10, show.call = TRUE, show.2sigma= TRUE,
        lo.iter = NULL, lo.cex= NULL, 
        par0line  = list(lty = 2, col = 2),
        parSmooth = list(lwd = 1.5, lty = 4, col = 3),
        parSigma  = list(lwd = 1.2, lty = 3, col = 4),
        \dots)
}
\arguments{
  \item{lm.res}{Result of \code{\link{lm}(..)}, \code{\link{aov}(..)},
    \code{\link{glm}(..)} or a similar object.}
  \item{fit}{fitted values; you probably want the default here.}
  \item{res}{residuals to use. Default: \bold{Weighted} ("Pearson") residuals if
    weights have been used for the model fit.}
  \item{labels}{strings to use as plotting symbols for each point.
    Default(NULL): extract observations' names or use its sequence number.
    Use, e.g., "*" to get simple \code{*} symbols.
  }
  \item{main}{main title to plot. Default: sophisticated, resulting in
    something like "Tukey-Anscombe Plot of :   y \~ x"  constructed from
    \code{call(.)} in \code{lm.res}.
  }
  \item{xlab}{x-axis lable for plot.}
  \item{draw.smooth}{logical; if \code{TRUE}, draw a \code{lowess} smoother
    (with automatic smoothing fraction).}
  \item{show.call}{logical; if \code{TRUE}, write the "call"ing syntax with
    which the fit was done.}
  \item{show.2sigma}{logical; if \code{TRUE}, draw horizontal lines at
    \eqn{\pm 2\sigma}{+- 2 sigma} where \eqn{\sigma} is \code{mad(resid)}.}
  \item{lo.iter}{positive integer, giving the number of lowess
    robustness iterations. The default depends on the model and
    is \code{0} for non Gaussian \code{\link{glm}}'s.}
  \item{lo.cex}{character expansion ("cex") for lowess and other
    marginal texts.}
  \item{par0line}{a list of arguments (with reasonable defaults) to be passed to
    \code{\link{abline}(.)} when drawing the x-axis, i.e.,
    the \eqn{y = 0} line.}
  \item{parSmooth, parSigma}{each a list of arguments (with reasonable
    default) for drawing the smooth curve (if \code{draw.smooth} is
    true), or the horizontal sigma boundaries (if \code{show.2sigma} is
    true) respectively.}
  \item{\dots}{further graphical parameters are passed to
    \code{\link{n.plot}(.)}.}
}
\section{Side Effects}{
  The above mentioned plot is produced on the current graphic device.
}
\author{
  Martin Maechler, Seminar fuer Statistik, ETH Zurich, Switzerland;
  \email{maechler@stat.math.ethz.ch}
}
\seealso{\code{\link{plot.lm}} which also does a QQ normal plot and more.
}
\examples{
data(stackloss)
TA.plot(lm(stack.loss ~ stack.x))

example(airquality)
summary(lmO <- lm(Ozone ~ ., data= airquality))
TA.plot(lmO)
TA.plot(lmO, label = "O") # instead of case numbers

if(FALSE) { %%  from S-plus
 TA.plot(lm(cost ~ age+type+car.age, claims, weights=number, na.action=na.omit))
}

##--- for  aov(.) : -------------
data(Gun, package = "nlme")
TA.plot( aov(rounds ~ Method + Physique/Team, data = Gun))

##--- Not so clear what it means for GLM, but: ------
if(require(rpart)) { # for the two datasets only
 data(solder, package = "rpart")
 TA.plot(glm(skips ~ ., data = solder, family = poisson), cex= .6)

 data(kyphosis, package = "rpart")
 TA.plot(glm(Kyphosis ~ poly(Age,2) + Start, data=kyphosis, family = binomial),
	 cex=.75) # smaller title and plotting characters
}
}
\keyword{models}
\keyword{regression}

