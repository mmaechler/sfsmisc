\name{pkgDesc}
\title{Version of packageDescription() as Simple Vector}
\alias{pkgDesc}
\alias{pkgBuilt}
\description{
  a simple \dQuote{version}, or wrapper for
  \code{\link{packageDescription}()}, returning a named character vector,
  including \code{"file"}, and still has a useful \code{\link{print}()}
  method.
}
\usage{
pkgDesc (pkg, lib.loc = NULL, fields = NULL, ...)
pkgBuilt(pkg, lib.loc = NULL, ...)
}
\arguments{
  \item{pkg}{a \code{\link{character}} string, name of an installed \R package.}
  \item{lib.loc}{library location to find the package in; the default
    \code{NULL} uses the full \code{\link{.libPaths}()}.}
  \item{fields}{a character vector (or \code{NULL}) specifying fields to be returned.}
  \item{...}{further optional arguments passed to \code{\link{packageDescription}()}.}
}
\value{
  a named \code{\link{character}} vector, with \code{\link{names}}, the
  \emph{fields}, identical to the names of the \code{\link{list}} returned
  by \code{\link{packageDescription}}, plus its \code{"file"} attribute.
  Additionally the resulting vector is of class \code{"Dlist"} which
  activates a useful \code{\link{print}()} method.
}
\author{Martin Maechler, Jan. 2021}
\note{The \code{file} is always returned; not the least that the author
  wants to see it quite often as his \code{\link{.libPaths}()} is
  non-trivial and typically longer than 4 entries.
}
\seealso{
  \code{\link{packageDescription}},
  \code{\link{.libPaths}}.
}
\examples{
str(pd <- pkgDesc("sfsmisc"))
pd[c("Date","Packaged", "Built","file")]

pkgBuilt("sfsmisc")
pkgs <- c("grid", "lattice", "MASS", "Matrix", "nlme", "lme4", "sfsmisc")
pkgs <- c("foobar", "barbar", pkgs, "kitty")
pkgsOk <- basename(find.package(pkgs, quiet=TRUE))
mpkg <- sapply(pkgsOk, pkgBuilt)
stopifnot(is.matrix(mpkg), nrow(mpkg) == 2)
mpkg["Built",]
}
