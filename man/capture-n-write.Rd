\name{capture.and.write}
\alias{capture.and.write}
\title{Capture output and Write / Print First and Last Parts}
\description{
  Capture output and print first and last parts, eliding
  middle parts.  Particularly useful for teaching purposes, and, e.g.,
  in Sweave (\code{\link{RweaveLatex}}).

  By default, when \code{middle = NA}, \code{capture.output(EXPR, first, last)}
  basically does
  \preformatted{
    co <- capture.output(EXPR)
    writeLines(head(co, first))
    cat( ... dotdots ...)
    writeLines(tail(co, last))
  }
}
\usage{
capture.and.write(EXPR, first, last = 2, middle = NA,
                  i.middle, dotdots = " ....... ", n.dots = 2)
}
\arguments{
  \item{EXPR}{the (literal) expression the output of which is to be
    captured.}
  \item{first}{integer: how many lines should be printed at beginning.}
  \item{last}{integer: how many lines should be printed at the end.}
  \item{middle}{numeric (or NA logical):}
  \item{i.middle}{index start of middle part}
  \item{dotdots}{string to be used for elided lines}
  \item{n.dots}{number of \code{dotdots} lines added between parts.}
}
\value{
  return value of \code{\link{capture.output}(EXPR)}.
}
\seealso{
  \code{\link{head}}, \code{\link{tail}}
}
\author{Martin Maechler, ETH Zurich}
\examples{
x <- seq(0, 10, by = .1)

## for matrix, dataframe, .. first lines include a header line:
capture.and.write( cbind(x, log1p(exp(x))),  first = 5)

## first, *middle* and last :
capture.and.write( cbind(x, x^2, x^3), first = 4, middle = 3, n.dots= 1)
}
%% some of MM's  own "real use" examples:
%% ~/R/Meetings-Kurse-etc/2012-Rmpfr-ZurichR/BinCoef.Rnw
%% ~/R/MM/NUMERICS/log1exp/log1exp-note.Rnw
