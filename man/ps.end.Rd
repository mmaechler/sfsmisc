\name{ps.end}
\alias{ps.end}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Close PostScript device opened by `ps.do'}
\usage{
ps.end(call.gv=NULL, do.color=u.get0("ps.do.color"), 
       command=paste("gview", if (!do.color) " -monochrome", sep = ""), 
       debug=FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{call.gv}{ ~~Describe \code{call.gv} here~~ }
 \item{do.color}{ ~~Describe \code{do.color} here~~ }
 \item{command}{character, giving a unix command for Postscript previewing.
   At SfS, \code{gview} is a simple shell script, with the line\cr
  \code{gv -watch -geometry -0+0 -magstep -2 -media BBox $@}}
 \item{debug}{logical; if  \code{TRUE} print information during execution.}
}
\description{
  Closes the Postscript file (\code{\link{postscript}}), openend by a
  previous \code{\link{ps.do}} (or \code{\link{ps.do}}) call, using
  \code{\link{dev.off}},  

  and
  opens a previewer for that file, \emph{unless} the previewer is
  already up.  This effectively provides an `interactive' device like
  \code{\link{x11}} for \code{\link{postscript}}.
}
\details{
  Depends heavily on Unix tools, such as \code{ps}.
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of `comp1'}
  \item{comp2 }{Description of `comp2'}
  ...
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
\keyword{devices}%-- one or more ...
