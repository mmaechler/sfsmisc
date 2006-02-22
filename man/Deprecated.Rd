\name{Deprecated}% <<< would like "special name": don't check \arguments{}
\title{Deprecated 'sfsmisc' Functions}
% NOTE: ../R/Deprecated.R   must be synchronized with this!
%       ~~~~~~~~~~~~~~~~~
\alias{nna}
\alias{digits}
\alias{digits.v}
\alias{subtit}
\alias{p.triangle}
\alias{p.panelL}
\alias{p.panelS}
\alias{p.pllines}
\alias{tapply.num}
\alias{list2mat}
\alias{huberM}
%------ PLEASE: put \alias{.} here for EACH !
%
\description{
  These functions are provided for compatibility with older versions of
  the \pkg{sfsmisc} package only, and may be defunct as soon as of the
  next release.
}
\usage{
nna(data)

digits.v(nvec, base = 2, num.bits = 1 + floor(log(max(nvec),base)))
digits(n, base = 10)

subtit(t)

p.panelL(x,y)
p.panelS(x,y,df = 4)
p.pllines(x,y,group,lty=c(1,3,2,4),\dots)

p.triangle(mat, label= "*", text.ecken = rep("",3), dreieck = TRUE)

list2mat(x, check = TRUE)

huberM(x, k = 1.5, tol = 1e-06, mu = median(x), s = mad(x, center=mu),
       warn0scale = getOption("verbose"))
}
\arguments{
% nna
  \item{data}{vector, matrix or data frame.}
% digits, digits.v
  \item{nvec, n}{non-negative integer (vector).}
  \item{base}{positive integer}
  \item{num.bits}{positive integer}
% subtit():
  \item{t}{character string.}
% p.triangle():
  \item{mat}{matrix mit \% A in der 1. Kolonne, \% B in der 2.. (range 0:1)}
  \item{label}{text of length \code{mat[,1]} for labeling points.}
  \item{text.ecken}{\code{character(3)} text for the three corners.}
  \item{dreieck}{logical; if true, draw the triangle border.}
% p.panelL() & p.panelS():
  \item{x,y}{panel function (or plot or general) arguments:
    numeric vectors of same length.}
  \item{df}{specify (equivalent) degrees of freedom for smoothing spline
    in \code{p.panelS}.}
% list2mat() - `x' already documented
  \item{check}{logical specifiying if it should be checked that list
    components have same length.}
% p.pllines(x,y,group,lty=c(1,3,2,4),\dots)
  \item{group}{grouping vector of same length as x or y.}
  \item{lty}{line types to use for the groups (recycled).}
  \item{\dots}{further arguments passed to (plot) methods.}
% huberM()
  \item{k}{positive factor; the algorithm winsorizes at \code{k}
    standard deviations.}
  \item{tol}{convergence tolerance.}
  \item{mu}{initial location estimator.}
  \item{s}{scale estimator held constant through the iterations.}
  \item{warn0scale}{logical; if true, and \code{s} is 0 and
    \code{length(x) > 1}, this will be warned about.}
}
\details{
  \code{nna} was a simpler version of the generic \code{\link{na.omit}}
  function; the latter should be used instead.

  \code{digits.v} and \code{digits} are old vector and \dQuote{scalar}
  versions of \code{\link{digitsBase}}.

  \code{subtit(t)} is really just a convenience abbreviation for
  \code{mtext(t, side = 3, line = 0)} which you should rather use.

  \code{p.panelL} and \code{p.panelS} are simple two-line functions to
  be used as \code{panel} arguments in \code{\link{pairs}} or
  \code{\link{coplot}}.  However, for such simple panels, it's rather
  recommended to use \emph{anonymous} functions, i.e.,
  \code{panel = function(x,y) { ....}} in the call to
  \code{\link{coplot}()} (e.g.).

  \code{p.pllines} is deprecated because basic \R graphics (but not
  S-PLUS) provide its   functionality directly: Use \code{plot(x,y, lty
    = group, type = 'l', ...)}.

  \code{p.triangle} is deprecated because package \pkg{vcd} has a much
  more versatile \code{\link[vcd]{ternaryplot}} function.

  \code{tapply.num} has simply be renamed to \code{\link{tapplySimpl}}
  which you should use instead.

  \code{list2mat(x)} is usually the same as \code{sapply(x, c)} (where
  the latter does not construct column names where \code{x} has no names).

  \code{huberM(x, ..)} is a safe (generalized) Huber M-Estimator of location
  with MAD scale, being sensible also when the scale is zero where
  \code{\link[MASS]{huber}()} returns an error. This function has been
  moved to package \pkg{robustbase}.
}

\keyword{documentation}

