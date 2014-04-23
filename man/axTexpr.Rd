\name{axTexpr}
\alias{axTexpr}
\title{Axis Ticks Expressions in Nice 10 ** k Form}
\description{
  Produce nice \eqn{a \times 10^k}{a * 10^k} expressions for
  \code{\link{axis}} labeling instead of the scientific notation
  \code{"a E<k>"}.

}
\usage{
axTexpr(side, at = axTicks(side, axp = axp, usr = usr, log = log),
        axp = NULL, usr = NULL, log = NULL,
        drop.1 = FALSE)
}
\arguments{
  \item{side}{integer in 1:4 specifying the axis side, as for
    \code{\link{axis}}.}
  \item{at}{numeric vector; with identical default as in
    \code{\link{axTicks}()}.}
  \item{axp, usr, log}{as for \code{\link{axTicks}()}.}
  \item{drop.1}{logical indicating if \eqn{1 \times}{1 *} should be
    dropped from the resulting expressions.}
}
\details{
  This is just a utility with the same arguments as
  \code{\link{axTicks}}, a wrapper \code{\link{pretty10exp}(at, *)}.
}
\value{
  an expression of the same length as \code{x}, with elements of the
  form \code{a \%*\% 10 ^ k}.
}
\author{Martin Maechler}
\seealso{\code{\link{pretty10exp}};
  \code{\link{eaxis}},   \code{\link{axis}}, \code{\link{axTicks}}.
}
\examples{
x <- 1e7*(-10:50)
y <- dnorm(x, m=10e7, s=20e7)
plot(x,y)## not really nice,  the following is better:

## For horizontal y-axis labels, need more space:
op <- par(mar= .1+ c(5,5,4,1))
plot(x,y, axes= FALSE, frame=TRUE)
aX <- axTicks(1); axis(1, at=aX, label= axTexpr(1, aX))
## horizontal labels on y-axis:
aY <- axTicks(2); axis(2, at=aY, label= axTexpr(2, aY), las=2)
par(op)

### -- only 'x' and using log-scale there:
plot(x,y, xaxt= "n", log = "x")
aX <- axTicks(1); axis(1, at=aX, label= axTexpr(1, aX))

## Now an `` engineer's version '' ( more ticks; only label "10 ^ k" ) :

axp <- par("xaxp") #-> powers of 10 *inside* 'usr'
axp[3] <- 1 # such that only 10^. are labeled
aX <- axTicks(1, axp = axp)
xu <- 10 ^ par("usr")[1:2]
e10 <- c(-1,1) + round(log10(axp[1:2])) ## exponents of 10 *outside* 'usr'
v <- c(outer(1:9, e10[1]:e10[2], function(x,E) x * 10 ^ E))
v <- v[xu[1] <= v & v <= xu[2]]

plot(x,y, xaxt= "n", log = "x", main = "engineer's version of x - axis")
axis(1, at = aX, label = axTexpr(1, aX, drop.1=TRUE)) # `default'
axis(1, at = v,  label = FALSE, tcl = 2/3 * par("tcl"))
}
\keyword{dplot}
