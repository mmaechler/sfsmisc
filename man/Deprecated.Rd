\name{Deprecated}% <<< would like "special name": don't check \arguments{}
\title{Deprecated 'sfsmisc' Functions}
% NOTE: ../R/Deprecated.R   must be synchronized with this!
%       ~~~~~~~~~~~~~~~~~
\alias{pmax.sa}
\alias{pmin.sa}
% Move things from here to ./sfsmisc-defunct.Rd
%                            ~~~~~~~~~~~~~~~~~~
% (and even older to ../Old_Defunct/ )
%
%------ PLEASE: put \alias{.} here for EACH !
%
\description{
  These functions are provided for compatibility with older versions of
  the \pkg{sfsmisc} package only, and may be defunct as soon as of the
  next release.
}
\usage{
pmax.sa(scalar, arr)
pmin.sa(scalar, arr)
}
\arguments{
  \item{scalar}{numeric scalar.}
  \item{arr}{any numeric \R object, typically array.}
}
\details{
  \code{pmax.sa(s, a)} and \code{pmin.sa(s, a)} return (more-dimensional) arrays.
  These have been deprecated, because \code{\link{pmax}} and
  \code{\link{pmin}} do so too, \bold{if} the array is used as
  \emph{first} argument.
}

\keyword{documentation}
