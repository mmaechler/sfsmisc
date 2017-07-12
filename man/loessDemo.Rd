\name{loessDemo}
\title{Graphical Interactive Demo of loess()}
\alias{loessDemo}
\description{
  A graphical and interactive demonstration and visualization of how
  \code{\link{loess}} works.  By clicking on the graphic, the user
  determines the current estimation window which is visualized together
  with the weights.
}
\usage{
loessDemo(x, y, span = 1/2, degree = 1, family = c("gaussian", "symmetric"),
          nearest = FALSE, nout = 501,
          xlim = numeric(0), ylim = numeric(0), strictlim = TRUE, verbose = TRUE,
          inch.sym = 0.25, pch = 4, shade = TRUE, w.symbols = TRUE,
          sym.col = "blue", w.col = "light blue", line.col = "steelblue")
}
\arguments{
  \item{x,y}{numeric vectors of the same length; the demo is about
    \code{\link{loess}(y ~ x)}.}
  \item{span}{the smoothing parameter \eqn{\alpha}.}
  \item{degree}{the degree of the polynomials to be used; must be in \eqn{{0,1,2}}.}
  \item{family}{if \code{"gaussian"} fitting is by least-squares, and if
    \code{"symmetric"} a re-descending M estimator is used with Tukey's
    biweight function.  Can be abbreviated.}
  \item{nearest}{logical indicating how \eqn{x_0} should be determined,
    the value at which \eqn{\hat{f}(x_0)}{f^(x_0)} is computed.  If
    \code{nearest} is true, the closest \emph{data} value is taken.
  }
  \item{nout}{the number of points at which to evaluate, i.e,
    determining \eqn{u_i}, \eqn{i = 1,2, \dots, \mathtt{nout}}, at
    which \eqn{\hat{f}(u_i)}{f^(u_i)} is computed.}
  \item{xlim}{x-range; to extend or determine (iff \code{strictlim} is
    true) the \eqn{x}-range for plotting.}
  \item{ylim}{y-range; to extend or determine (iff \code{strictlim} is
    true) the \eqn{y}-range for plotting.}
  \item{strictlim}{logical determining if \code{xlim} and \code{ylim}
    should be strict limits (as e.g., in \code{\link{plot.default}}), or
    just a suggestion to \emph{extend} the data-dependent ranges.}
  \item{verbose}{logical ......}
  \item{inch.sym}{symbol size in inches of the maximal weight circle symbol.}
  \item{pch}{plotting character, see \code{\link{points}}.}
  \item{shade}{logical; if true, \code{\link{polygon}(.., density=..)}
    will be used to shade off the regions where the weights are zero.}
  \item{w.symbols}{logical indicating if the non-zero weights should be
    visualized by circles with radius proportional to \code{inch.sym}
    and \eqn{\sqrt{w}} where \eqn{w} are the weights.}
  \item{sym.col, w.col, line.col}{colors for the symbols, weights and
    lines, respectively.}
}
%% \value{
%% }
\author{%%for grep: Greg Snow, ... Henrik Aa. Nielsen ..
  As function \code{loess.demo()},  written and posted to S-news, on 27
  Sep 2001, by Greg Snow, Brigham Young University, % gls@byu.edu
  it was modified by Henrik Aa. Nielsen, IMM, DTU, % han@imm.dtu.dk
  and subsequently spiffed up for \R by Martin Maechler.
}
\seealso{
  \code{\link{loess}}.
}
\examples{
if(dev.interactive()) {

 if(requireNamespace("lattice")) {
    data("ethanol", package = "lattice")
    attach(ethanol)
    loessDemo(E,NOx, span=.25)
    loessDemo(E,NOx, span=.25, family = "symmetric")

    loessDemo(E,NOx, degree=0)# Tricube Kernel estimate
  }% if (. lattice .)

 ## Artificial Example with one outlier
 n2 <- 50; x <- 1:(1+2*n2)
 fx <- (x/10 - 5)^2
 y <- fx + 4*rnorm(x)
 y[n2+1] <- 1e4
 loessDemo(x,y, span=1/3, ylim= c(0,1000))# not robust !!
 loessDemo(x,y, span=1/3, family = "symm")
 loessDemo(x,y, span=1/3, family = "symm", w.symb = FALSE, ylim = c(0,40))
 loessDemo(x,y, span=1/3, family = "symm", ylim = c(0,40))
 ## but see warnings() --- there's a "fixup"

}% only if  interactive
}
\keyword{loess}
\keyword{dynamic}
\keyword{hplot}
