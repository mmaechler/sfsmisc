\name{p.dnorm}
\alias{p.dnorm}
\alias{p.dchisq}
\alias{p.dgamma}
\title{Plot Parametric Density Functions}
\description{
  These are utilities for pretty plotting of often used parametric
  densities.
}
\usage{
p.dnorm (mu = 0, s = 1, h0.col = "light gray",
         ms.lines = TRUE, ms.col = "gray", ...)
p.dchisq(nu,    h0.col = "light gray", ...)
p.dgamma(shape, h0.col = "light gray", ...)
}
\arguments{
  \item{mu,s}{numbers, the mean and standard deviation of the normal
    distribution.}
  \item{nu}{positive number, the degrees of freedom \code{df} argument
    for the \eqn{\chi^2}{chi^2}-density function \code{\link{dchisq}}.}
  \item{shape}{number, the \code{shape} parameter for the Gamma
    distribution.}
  \item{h0.col}{color specification for the line \eqn{y = 0}.}
  \item{ms.lines}{logical, used for the normal only: should lines be
    drawn at the mean and \eqn{\pm}{+-} 1 standard deviation.}
  \item{ms.col}{color for the \code{ms} lines if \code{ms.lines} is TRUE.}
  \item{\dots}{further parameter passed to \code{\link{curve}()}, e.g.,
    \code{add = TRUE} for adding to current plot.}
}
\author{Werner Stahel et al.}
\seealso{the underlying density functions,
  \code{\link{dnorm}},  \code{\link{dchisq}},  \code{\link{dgamma}}.}
\examples{
p.dnorm()
p.dnorm(mu=1.5, add = TRUE, ms.lines = FALSE) # add to the plot above

p.dchisq(2, main= "Chi^2 Densities -- nu = 2,3,4")
p.dchisq(3, add = TRUE, col = "red")
p.dchisq(4, add = TRUE, col = "blue")

op <- par(mfrow = c(2,2), mgp = c(1.6, 0.6,0), mar = c(3,3,1,1))
for(sh in 1:4)
   p.dgamma(sh)
par(op)
}
\keyword{hplot}
\keyword{utilities}
