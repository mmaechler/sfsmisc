\name{sessionInfoX}% sessionInfo help is ~/R/D/r-devel/R/src/library/utils/man/sessionInfo.Rd
\title{Extended Information About the Current R Session}
\alias{sessionInfoX}
\alias{print.sessionInfoX}
\description{
  Collect (and print) information about the current \R session and
  environment, using \code{\link{sessionInfo}()} and more mostly
  low-level and platform dependent information.
}
\usage{
sessionInfoX(pkgs = NULL, list.libP = FALSE, extraR.env = TRUE)

\method{print}{sessionInfoX}(x, locale = TRUE, RLIBS = TRUE, Renv = TRUE, \dots)
}
\arguments{
  \item{pkgs}{an optional \code{\link{character}} vector of \R packages,
    whose \code{\link{packageDescription}()}s are wanted.}
  \item{list.libP}{a logical indicating if for all
    \code{\link{.libPaths}} entries, the files should be listed via
    \code{\link{list.files}}.}
  \item{extraR.env}{logical indicating if \emph{all} environment
    variables should be recorded which start with \code{"R_"} or
    \code{"_R_"}.}
%% print():
  \item{x}{typically the result of \code{sessionInfoX()}.}
  \item{locale}{logical, passed to \code{\link{print.sessionInfo}()}
    indicating if the locale information should be printed.}
  \item{RLIBS}{logical indicating if the information about R_LIBS should
    be printed.}
  \item{Renv}{logical indicating if the information about R environment
    variables should be printed.}
  \item{\dots}{passed to \code{\link{print}} methods.}
}
%% \details{
%% }
\value{an object of S3 class \code{"sessionInfoX"}, a \code{\link{list}}
  with components (there may be more, experimental and not yet listed here):
 \item{sInfo}{simply the value of \code{\link{sessionInfo}()}.}
 \item{sysInf}{the value of \code{\link{Sys.info}()}.}
 \item{capabilities}{the value of \code{\link{capabilities}()}.}
 \item{extSoft}{for \R 3.2.0 and newer, the value of \code{\link{extSoftVersion}()}.}
 \item{LAPACK}{for \R 3.0.3 and newer, the value of \code{\link{La_version}()}.}
 \item{pcre}{for \R 3.1.3 and newer, the value of \code{\link{pcre_config}()}.}
 \item{pkgDescr}{If \code{pkgs} was non-empty, a named
   \code{\link{list}} of \code{\link{packageDescription}()}s for each
   entry in \code{pkgs}.}
 \item{libPath}{the value of \code{\link{.libPaths}()}.}
 \item{RLIBS}{a \code{\link{character}} vector of entries from
   \code{\link{Sys.getenv}("R_LIBS")}, typically very similar to the
   \code{libPaths} component.}
 \item{n.RLIBS}{simply a \code{\link{normalizePath}()}ed version of \code{RLIBS}.}
 \item{R.env}{a named character vector with the \dQuote{important} \R
   environment variables \code{R_ENVIRON}, \code{R_PROFILE},
   \code{R_CHECK_ENVIRON}.}
 \item{xR.env}{if \code{extraR.env} was true, a named character vector
   of \dQuote{all R related} environment variables, as specified in
   \code{extraR.env}'s description above.}

%% FIXME: unfinished

}
\author{Martin Maechler, December 2015}
\seealso{
  \code{\link{sessionInfo}},
  \code{\link{.libPaths}}, \code{\link{R.version}}, \code{\link{Sys.getenv}}.
}
\examples{
six0 <- sessionInfoX()
sixN <- sessionInfoX("nlme", list.libP = TRUE)
sixN # -> print() method for "sessionInfoX"
names(sixN)
str(sixN, max = 1)# outline of lower-level structure
str(sixN$pkgDescr) # list with one component "nlme"
}
\keyword{misc}
