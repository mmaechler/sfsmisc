\name{Deprecated}% <<< would like "special name": don't check \arguments{}
\title{Deprecated 'sfsmisc' Functions}
% NOTE: ../R/Deprecated.R   must be synchronized with this!
%       ~~~~~~~~~~~~~~~~~
\alias{p.pllines}
\alias{list2mat}
\alias{huberM}
% the whole set
\alias{rnls}
\alias{print.rnls}
\alias{fitted.rnls}
\alias{predict.rnls}
\alias{residuals.rnls}
\alias{summary.rnls}
%
%------ PLEASE: put \alias{.} here for EACH !
%
\description{
  These functions are provided for compatibility with older versions of
  the \pkg{sfsmisc} package only, and may be defunct as soon as of the
  next release.
}
\usage{
p.pllines(x,y,group,lty=c(1,3,2,4),\dots)

list2mat(x, check = TRUE)

huberM(x, k = 1.5, tol = 1e-06, mu = median(x), s = mad(x, center=mu),
       warn0scale = getOption("verbose"))
}
\arguments{
% p.pllines(x,y,group,lty=c(1,3,2,4),\dots)
  \item{x,y}{panel function (or plot or general) arguments:
    numeric vectors of same length.}
  \item{group}{grouping vector of same length as x or y.}
  \item{lty}{line types to use for the groups (recycled).}
  \item{\dots}{further arguments passed to (plot) methods.}
% list2mat() - `x' already documented
  \item{check}{logical specifiying if it should be checked that list
    components have same length.}
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
  \code{p.pllines} is deprecated because basic \R graphics (but not
  S-PLUS) provide its   functionality directly: Use \code{plot(x,y, lty
    = group, type = 'l', ...)}.

  \code{list2mat(x)} is usually the same as \code{sapply(x, c)} (where
  the latter does not construct column names where \code{x} has no names).

  \code{huberM(x, ..)} is a safe (generalized) Huber M-Estimator of location
  with MAD scale, being sensible also when the scale is zero where
  \code{\link[MASS]{huber}()} returns an error. This function has been
  moved to package \pkg{robustbase}.

  \code{rnls(formula, data, ......)} has been available, improved
  in package \pkg{robustbase} for almost a year and been replaced by a
  stub here.  All \code{*.rnls} methods have been completely removed.
}

\keyword{documentation}
