\name{ps.end}
\alias{ps.end}
\alias{pdf.end}
\title{Close PostScript or Acrobat Graphics Device opened by 'ps.do' / 'pdf.do'}
\usage{
ps.end(call.gv= NULL, command = getOption("eps_view"),
       debug = getOption("verbose"))
pdf.end(call.viewer= NULL, command = getOption("pdfviewer"),
       debug = getOption("verbose"))
}
\arguments{
 \item{call.gv,call.viewer}{logical, indicating if the postscript or
   acrobat reader (e.g., ghostview or \code{acroread} or the command
   given by \code{command}) should be called.  By default, find out if
   the viewer is already runing on this file and only call it if needed.}
 \item{command}{character, giving a system command for PostScript previewing.
   By default, \code{getOption("eps_view")} is set to\cr
   \code{gv -watch -geometry -0+0 -magstep -2 -media BBox -noantialias}
   which assumes \code{gv} (aka \emph{ghostview}) to be in your OS path.}
 \item{debug}{logical; if \code{TRUE} print information during execution.}
}
\description{
  Closes the PostScript or PDF file
  (\code{\link{postscript}},\code{\link{pdf}}), openend by a previous
  \code{\link{ps.do}} (or \code{\link{pdf.latex}}, or \dots) call, using
  \code{\link{dev.off}}, and additionally opens a previewer for that
  file, \emph{unless} the previewer is already up.  This almost provides
  an \sQuote{interactive} device (like \code{\link{x11}}) for
  \code{\link{postscript}} or \code{\link{pdf}}.
}
\details{
  Depends heavily on Unix tools, such as \code{ps}.
}
\author{Martin Maechler}
\seealso{\code{\link{postscript}}, \code{\link{postscript}}
  \code{\link{ps.do}},..
}
\examples{
if(interactive() \dontshow{ || Sys.getenv("USER") == "maechler" }
  ) {
    ps.do("ex.ps")
    data(sunspots)
    plot(sunspots)
    ps.end()

    pdf.latex("ex-sun.pdf")
    plot(sunspots)
    pdf.end(call. = FALSE) # basically the same as dev.off()
  }
  ps.latex("ex2.eps")
  plot(sunspots)
  ps.end(call.gv = FALSE) # basically the same as dev.off()
}
\keyword{device}
