\name{pkgLibs}
\alias{pkgLibs}
\title{R Package Compiled Code Library Dependencies (on Unix-alikes)}
\description{
  List some system level information about the compiled code library,
  typically its dependencies, for \R packages with compiled code; for
  Unix-alikes or more generally when \code{cmd} is installed locally.
}
\usage{
pkgLibs(pkg, cmd = "ldd")
}
\arguments{
  \item{pkg}{\code{\link{character}} vector of package names of
    \emph{installed} \R packages.}
  \item{cmd}{a \code{character} string with the name of an OS / system
    level program (to be called via \code{\link{system}(cmd, ..)}) which
    gives information about the shared  library (of compiled code), also
    known as \dQuote{DLL} (dynamically loadable library) or
    \dQuote{so} ((dynamic) shared object) library.  The default,
    \command{"ldd"} is a standard binary utility on Unix-alike platforms
    such as Linux or macOS.}
}
\details{
  Note that there seems some language confusion as \dQuote{DLL} on Windows
  is \emph{also} used for \dQuote{Dynamic-link Library} and Wikipedia warns about
  confusing the two concepts (\dQuote{dynamically loaded ..} vs
  \dQuote{dynamic-link ..}).
}
\value{
  a named \code{\link{list}} with one entry per package in \code{pkg}, the
  \code{\link{names}} being the directory / folder names of the
  corresponding pkgs from \code{pkg}.

  The exact structure of such entries is currently subject to change and
  you should not rely on its exact format for now.
}
\references{
  Dynamic Loading Library on Wikipedia,
  \url{https://en.wikipedia.org/wiki/Dynamic_Loading}

  On Windows, DLL is also used for Dynamic-link library,
  \url{https://en.wikipedia.org/wiki/Dynamic-link_library}.

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
