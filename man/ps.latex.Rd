\name{ps.latex}
\alias{ps.latex}
\title{Ghostview Device Driver and ``LaTeX Plots'' Utilities}
\usage{
ps.latex(file, height = 5 + main.space * 1.25,  width = 9.5,
         iso.latin1=is.R(),
         main.space=FALSE, lab.space = main.space,
         lab=c(10, 10, 7), mgp.lab=c(1.6, 0.7, 0), mar=c(4, 4, 0.9, 1.1),
         \dots)
}
%%>>>> put ps.do()  here as well !!
%%
\arguments{
 \item{file,...}{see \code{\link{ps.do}}.}
%  \item{height}{ ~~Describe \code{height} here~~ }
%  \item{width}{ ~~Describe \code{width} here~~ }
% \item{iso.latin1}{ ~~Describe \code{iso.latin1} here~~ }
 \item{main.space}{logical; if true, leave space for a main title
   (unusual for LaTeX figures!).}
 \item{lab.space}{logical; if true, leave space for .....}

 \item{lab}{integer of length 3, \code{lab[1:2]} are desired number of
   tick marks on x- and y- axis, see \code{\link{par}(lab=)}.}
 \item{mgp.lab}{three decreasing numbers determining space for axis
   labeling, see \code{\link{par}(mgp=)}, the default is here smaller
   than usual.}
 \item{mar}{four numbers, indicating marginal space, see
   \code{\link{par}(mar=)}, the default is here smaller than usual.}
 \item{\dots}{further arguments to \code{\link{ps.do}()}.}
}
\description{
  The \LaTeX flavor of \code{\link{ps.do}()} which is an interactive
  PostScript device driver, using \file{gv} (improved \file{ghostview}).
}
\details{
  After \code{\link{ps.do}(..)} is called, the graphical parameters
  \code{c("mar", "mgp", "lab")} are reset (to values that typically are
  better than the defaults for LaTeX figures.
}
\value{
  A list with components
  \item{old.par}{containing the old \code{par} values}
  \item{new.par}{containing the newly set \code{par} values}
}
\seealso{Underlying function \code{\link{ps.do}} and \code{\link{ps.end}}.
}
\examples{
ps.latex("ps.latex-ex.ps", main= TRUE)
 data(sunspots)
 plot(sunspots,main=paste("Sunspots Data, n=",length(sunspots)),col="red")
ps.end()
}
\keyword{devices}
