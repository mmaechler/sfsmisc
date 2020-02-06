\name{pkgLibs}
\alias{pkgLibs}
\title{List Compiled Code Library Dependencies (on Unix-alikes)}
\description{
  List some system level information about compiled code library
  (dependencies) for \R packages with compiled code; only on Unix-alikes,
  or when \code{cmd} is installed locally.
}
\usage{
pkgLibs(pkg, cmd = "ldd")
}
\arguments{
  \item{pkg}{\code{\link{character}} vector of package names of
    \emph{installed} \R packages.}
  \item{cmd}{a \code{character} string with the name of an OS / system
    level program that gives information about a shared library (of
    compiled code), also known as \dQuote{DLL} (dynamic-link library) or
    \dQuote{so} ((dynamic) shared object) library.  The default,
    \command{"ldd"} is a standard binary utility on Unix-alike platforms
    such as Linux or macOS.}
}
%% \details{
%% }
\value{
  a named \code{\link{list}} with one entry per package in \code{pkg}, the
  \code{\link{names}} being the directory / folder names of the
  corresponding pkgs from \code{pkg}.

  The exact structure of such entries is currently subject to change and
  you should not rely on its exact format for now.
}
\references{
  Dynamic-link Library on Wikipedia,
  \url{https://en.wikipedia.org/wiki/Dynamic-link_library}

  \command{man ldd} from a terminal on a valid OS.
}
\author{Martin Maechler}
\seealso{
  \code{\link{dyn.load}()},
  \code{\link{library.dynam}()}, and \code{\link{getLoadedDLLs}()}.

  Also, \code{\link{.C}}, \code{\link{.Call}} which use such DLLs.
}
\examples{
pl <- pkgLibs(c("stats", "MASS", "rpart", "Matrix"))
pl
stopifnot(is.list(pl),  length(pl) == 4,
          is.character(pkgDirs <- names(pl)),  dir.exists(pkgDirs))
}
\keyword{interface}
