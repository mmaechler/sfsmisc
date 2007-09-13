\name{ps.latex}
\alias{pdf.do}
\alias{pdf.latex}
\alias{ps.do}
\alias{ps.latex}
\title{PostScript/PDF Preview Device with Optional `LaTeX' Touch}
\usage{
pdf.do(file, paper = "default", onefile = FALSE,
       title = NULL, version = "1.4", \dots)

pdf.latex(file, height = 5 + main.space * 1.25,  width = 9.5,
          main.space=FALSE, lab.space = main.space,
          paper = "special", title = NULL,
          lab=c(10, 10, 7), mgp.lab=c(1.6, 0.7, 0), mar=c(4, 4, 0.9, 1.1), \dots)

ps.do(file, width=-1, height=-1, onefile=FALSE, horizontal=FALSE,
      title = NULL, \dots)

ps.latex(file, height = 5 + main.space * 1.25,  width = 9.5,
         main.space=FALSE, lab.space = main.space,
         paper = "special", title = NULL,
         lab=c(10, 10, 7), mgp.lab=c(1.6, 0.7, 0), mar=c(4, 4, 0.9, 1.1), \dots)
}
\arguments{
  \item{file}{character giving the PostScript/PDF file name to be written.}
  \item{height}{device height in \emph{inches}, \code{height * 2.54} are
    \emph{cm}.  The default is 5 plus 1.25 iff \code{main.space}.}
  \item{width}{device width in \emph{inches}; for this and
    \code{height}, see \code{\link{postscript}}.}
  \item{onefile, horizontal}{logicals passed to
    \code{\link{postscript}(..)} or \code{\link{pdf}(..)}, most probably
    to be left alone.}
  \item{title}{PostScript/PDF (not plot!) title passed to
    \code{\link{postscript}()} or \code{\link{pdf}()}; by default use a
    title with \R version and \code{file} in it.}
  \item{version}{a string describing the PDF version that will be
    required to view the output, see \code{\link{pdf}}; our (high)
    default ensures alpha-transparency.}
  \item{main.space}{logical; if true, leave space for a main title
    (unusual for LaTeX figures!).}
  \item{lab.space}{logical; if true, leave space for x- and y- labels
    (by \emph{not} subtracting from \code{mar}).}
  \item{paper}{character, see \code{\link{postscript}}.  Only if this is
    \code{"special"} are your choices of \code{width} and \code{height}
    completely honored (and this may lead to files that cannot print on
    A4) with resizing.}
  \item{lab}{integer of length 3, \code{lab[1:2]} are desired number of
    tick marks on x- and y- axis, see \code{\link{par}(lab=)}.}
  \item{mgp.lab}{three decreasing numbers determining space for axis
    labeling, see \code{\link{par}(mgp=)}, the default is here smaller
    than usual.}
  \item{mar}{four numbers, indicating marginal space, see
    \code{\link{par}(mar=)}, the default is here smaller than usual.}
  \item{\dots}{arguments passed to \code{\link{ps.do}()} or
    \code{\link{pdf.do}()} from
    \code{ps.latex} / \code{pdf.latex} and to \code{\link{ps.options}}
    from \code{ps.do}/\code{pdf.do}.}
}
\description{
  All functions start a pseudo PostScript or Acrobat preview device, using
  \code{\link{postscript}} or \code{\link{pdf}}, and further registering
  the file name for subsequent calls to \code{\link{ps.end}}.
}
\details{
  \code{ps.latex} and \code{pdf.latex} have an additional \LaTeX flavor,
  and just differ by some extra \code{\link{par}} settings from the
  \code{*.do} siblings: E.g., after \code{\link{ps.do}(..)}
  is called, the graphical parameters \code{c("mar", "mgp", "lab")} are
  reset (to values that typically are better than the defaults for LaTeX
  figures).
}
\value{
  A list with components
  \item{old.par}{containing the old \code{par} values}
  \item{new.par}{containing the newly set \code{par} values}
}
\author{Martin Maechler}
\seealso{\code{\link{ps.end}}, \code{\link{pdf}}, \code{\link{postscript}},
  \code{\link{dev.print}}.
}
\examples{
if(interactive()) {

 ps.latex("ps.latex-ex.ps", main= TRUE)
  data(sunspots)
  plot(sunspots,main=paste("Sunspots Data, n=",length(sunspots)),col="red")
 ps.end()

 pdf.latex("pdf.latex-ex.pdf", main= TRUE)
  data(sunspots)
  plot(sunspots,main=paste("Sunspots Data, n=",length(sunspots)),col="red")
 pdf.end()

 ps.do("ps_do_ex.ps")
   example(plot.function)
 ps.end()
}
}
\keyword{device}
