\name{errbar}
\alias{errbar}
\title{Plot with Error Bars}
\usage{
errbar(x, y, yplus, yminus, cap )
}
\arguments{
  \item{x}{vector of x values.}
  \item{y}{vector of y values.}
  \item{yplus}{vector of y values: the tops of the error bars.}
  \item{yminus}{vector of y values: the bottoms of the error bars.}
  \item{cap}{The width of the little lines at the tops and bottoms of
    the error bars in units of the width of the plot.  Default is .015. 
    Graphical parameters (see \code{\link{par}}) may also
    be supplied as arguments to this function.}
}
\section{EFFECT}{
Makes a plot with error bars.
}
\examples{
y <- rnorm(10); d <- 1 + .1*rnorm(10)
errbar(1:10, y, y + d, y - d, main="Error Bars example")
}
% Converted by Sd2Rd version 0.3-3.
