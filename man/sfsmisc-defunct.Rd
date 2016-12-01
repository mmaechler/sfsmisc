\name{sfsmisc-defunct}
\title{Defunct Functions in Package \pkg{sfsmisc}}
%------ NOTE:  ../R/Defunct.R   must be synchronized with this!
%                 ~~~~~~~~~~~
\alias{sfsmisc-defunct}
%------ PLEASE: put \alias{.} here for  EACH !
% Move things from here to ../Old_Defunct/
%                          ~~~~~~~~~~~~~~~
\alias{list2mat}
\alias{pl.ds}
\alias{p.pllines}
\description{
  The functions or variables listed here are no longer part of package
  \pkg{sfsmisc} as they are not needed (any more).
}
\usage{
## Defunct in 2016-12 --> to be sfsmisc 1.1-1
list2mat(x, check = TRUE)

p.pllines(x,y,group,lty=c(1,3,2,4),\dots)

## deprecated from 2007 to 2013; defunct since 2014-01:
pl.ds() ##-- is replaced by  plotDS()
}
\details{
  \code{list2mat(x)} was usually the same as \code{sapply(x, c)} (where
  the latter does not construct column names where \code{x} has no names).

  \code{p.pllines} is now defunct because basic \R graphics (but not
  S-PLUS) provide its functionality directly: Use \code{plot(x,y, lty
    = group, type = 'l', ...)}.

  \code{pl.ds} has been renamed to \code{\link{plotDS}()} in 2007.
}
\seealso{
  \code{\link{Defunct}}
}
\keyword{internal}
