\name{u.sys}
\title{'Portable' System function (R / S-plus)}
\alias{u.sys}
\alias{Sys.ps.cmd}% was in unix/Sys.ps.Rd
\description{
  \code{u.sys()} is a convenient wrapper (of \code{system()}) to call to
  the underlying operating system.  The main purpose has been to provide
  a function with identical UI both in S-PLUS and \R.
  MM thinks you shouldn't use this anymore, usually.

  \code{Sys.ps.cmd()} returns the \samp{ps} (\sQuote{\bold{p}rocess \bold{s}tatus})
  OS command name (as \code{\link{character}} string), and is typically
  usable on unix alikes only.% Windows with 'Rtools' installed ??
}
\usage{
u.sys(\dots, intern = TRUE)

Sys.ps.cmd()
}
\arguments{
  \item{\dots}{any number of strings -- which will be
    \code{\link{paste}()}d together and passed to \code{system}.}
  \item{intern}{logical -- note that the default is \emph{reversed} from
    the one in \code{\link{system}()}.}
}
\author{Martin Maechler}
\seealso{\code{\link{system}}, really!;
  on non-Windows, \code{Sys.ps()} which makes use of \code{Sys.ps.cmd()}.
}
\examples{
u.sys # shows how simply the function is defined :
\dontrun{
  function (..., intern = TRUE)
  system(paste(..., sep = ""), intern = intern)
}

# All *running* processes of user [sometimes only R]:
try ( u.sys(Sys.ps.cmd(), "ur") )
}
\keyword{utilities}
\keyword{environment}% System, not R
