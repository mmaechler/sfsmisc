\name{ps.latex}
\alias{ps.do}
\alias{ps.latex}
\title{PostScript Preview Device with Optional `LaTeX' Touch}
\usage{
ps.do(file, width=-1, height=-1, onefile=FALSE, horizontal=FALSE,
      title = NULL, \dots)

ps.latex(file, height = 5 + main.space * 1.25,  width = 9.5,
         main.space=FALSE, lab.space = main.space,
         paper = "special", title = NULL,
         lab=c(10, 10, 7), mgp.lab=c(1.6, 0.7, 0), mar=c(4, 4, 0.9, 1.1), \dots)
}
\arguments{
  \item{file}{character giving the PostScript file name to be written.}
  \item{height}{device height in \emph{inches}, \code{height * 2.54} are
    \emph{cm}.  The default is 5 plus 1.25 iff \code{main.space}.}
  \item{width}{device width in \emph{inches}; for this and
    \code{height}, see \code{\link{postscript}}.}
  \item{onefile, horizontal}{logicals passed to
    \code{\link{postscript}(..)}, most probably to be left alone.}
  \item{title}{PostScript (not plot!) title passed to
    \code{\link{postscript}()}; by default use a title with \R version
    and \code{file} in it.}
  \item{main.space}{logical; if true, leave space for a main title
    (unusual for LaTeX figures!).}
  \item{lab.space}{logical; if true, leave space for x- and y- labels
    (by \emph{not} subtracting from \code{mar}).}
  \item{paper}{character, see \code{\link{postscript}}.  Only if this is
    \code{"special"} are your choices of \code{width} and \code{height}
    completely honored (and this may lead to files that can't print on
    A4) with resizing.}
  \item{lab}{integer of length 3, \code{lab[1:2]} are desired number of
    tick marks on x- and y- axis, see \code{\link{par}(lab=)}.}
  \item{mgp.lab}{three decreasing numbers determining space for axis
    labeling, see \code{\link{par}(mgp=)}, the default is here smaller
    than usual.}
  \item{mar}{four numbers, indicating marginal space, see
    \code{\link{par}(mar=)}, the default is here smaller than usual.}
  \item{\dots}{arguments passed to \code{\link{ps.do}()} from
    \code{ps.latex}} and to \code{\link{ps.options}} from \code{ps.do}.}
}
\description{
  Both functions start a ``Pseudo PostScript Preview'' device, using
  \code{\link{postscript}}, and further registering the file name for
  subsequent calls to \code{\link{ps.end}}.
}
\details{
  \code{ps.latex} has an additional \LaTeX flavor, and just differs by
  some extra \code{\link{par}} settings: After \code{\link{ps.do}(..)}
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
\seealso{\code{\link{ps.end}}, \code{\link{postscript}},
  \code{\link{dev.print}}.
}
\examples{
if(interactive()) {
 ps.latex("ps.latex-ex.ps", main= TRUE)
  data(sunspots)
  plot(sunspots,main=paste("Sunspots Data, n=",length(sunspots)),col="red")
 ps.end()

 ps.do("ps_do_ex.ps")
   example(plot.function)
 ps.end()
}
}
\keyword{device}
