\name{ps.end}
\alias{ps.end}
\title{Close PostScript device opened by `ps.do'}
\usage{
ps.end(call.gv= NULL, do.color = u.get0("ps.do.color"),
       command = paste("gview", if (!do.color) " -monochrome", sep = ""),
       debug = getOption("verbose"))
}
\arguments{
 \item{call.gv}{logical, indicating if ghostview (or the command given
  by \code{command}) should be called.  By default, find out if
  ghostview is already runing on this file and only call it if needed.}
 \item{do.color}{logical indicating if the previewer should use color.}
 \item{command}{character, giving a unix command for Postscript previewing.
   At SfS, \code{gview} is a simple shell script, with the line\cr
  \code{gv -watch -geometry -0+0 -magstep -2 -media BBox}.}
 \item{debug}{logical; if  \code{TRUE} print information during execution.}
}
\description{
  Closes the Postscript file (\code{\link{postscript}}),
  openend by a previous \code{\link{ps.do}} (or \code{\link{ps.latex}})
  call, using \code{\link{dev.off}}, and additionally opens a previewer
  for that file, \emph{unless} the previewer is already up.  This
  effectively provides an \sQuote{interactive} device like \code{\link{x11}}
  for \code{\link{postscript}}.
}
\details{
  Depends heavily on Unix tools, such as \code{ps}.
}
\author{Martin Maechler}
\seealso{\code{\link{postscript}},..}
\examples{
  { ps.do("ex.ps")
    data(sunspots)
    plot(sunspots)
    ps.end()
  }
}
\keyword{device}%-- one or more ...
