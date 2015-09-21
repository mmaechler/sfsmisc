\name{Deprecated}% <<< would like "special name": don't check \arguments{}
\title{Deprecated 'sfsmisc' Functions}
% NOTE: ../R/Deprecated.R   must be synchronized with this!
%       ~~~~~~~~~~~~~~~~~
\alias{hist.bxp}
\alias{p.pllines}
\alias{list2mat}
% the whole set
% removed, see ../Old_Defunct/ \alias{rnls}
%
%------ PLEASE: put \alias{.} here for EACH !
%
\description{
  These functions are provided for compatibility with older versions of
  the \pkg{sfsmisc} package only, and may be defunct as soon as of the
  next release.
}
\usage{
%% hist.bxp(x, ...)
p.pllines(x,y,group,lty=c(1,3,2,4),\dots)

list2mat(x, check = TRUE)
}
\arguments{
% p.pllines(x,y,group,lty=c(1,3,2,4),\dots)
  \item{x,y}{panel function (or plot or general) arguments:
    numeric vectors of same length.}
  \item{group}{grouping vector of same length as x or y.}
  \item{lty}{line types to use for the groups (recycled).}
  \item{\dots}{further arguments passed to (plot) methods.}
% list2mat() - 'x' already documented
  \item{check}{logical specifiying if it should be checked that list
    components have same length.}
}
\details{
  \code{hist.bxp} is deprecated in favor of \code{\link{histBxp}}
  because it looked too much like an S3 method to the CRAN checks.

  \code{p.pllines} is deprecated because basic \R graphics (but not
  S-PLUS) provide its functionality directly: Use \code{plot(x,y, lty
    = group, type = 'l', ...)}.

  \code{list2mat(x)} is usually the same as \code{sapply(x, c)} (where
  the latter does not construct column names where \code{x} has no names).
}

\keyword{documentation}
