\name{mult.fig}
\alias{mult.fig}
\title{Plot Setup for MULTiple FIGures, incl. Main Title}
\description{
  Easy Setup for plotting multiple figures (in a rectangular layout) on
  one page.  It allows to specify a main title and uses \emph{smart}
  defaults for several \code{\link{par}} calls.
}
\usage{
mult.fig(nr.plots, mfrow, mfcol,
         marP = rep(0, 4), mgp = c(1.5, 0.6, 0),
         mar = marP + 0.1 + c(4,4,2,1), oma = c(0,0, tit.wid, 0),
         main = NULL,
         tit.wid = if (is.null(main)) 0 else 1 + 1.5*cex.main,
         quiet = .Device == "postscript",
         cex.main = par("cex.main"), line.main = cex.main - 1/2,
         col.main = par("col.main"), font.main = par("font.main"), \dots)
}
\arguments{
 \item{nr.plots}{integer; the number of plot figures you'll want to draw.}
 \item{mfrow}{\emph{instead} of \code{nr.plots}: integer(2) vector
   giving the rectangular figure layout for \code{\link{par}(mfrow= .)}.}
 \item{mfcol}{\emph{instead} of \code{nr.plots}: integer(2) vector
   giving the rectangular figure layout for \code{\link{par}(mfcol= .)}.}
 \item{marP}{numeric(4) vector of figure margins to \emph{add}
   (``\bold{P}lus'') to default \code{mar}, see below.}
 \item{mgp}{argument for \code{\link{par}(mpg= .)} with a smaller
   default than usual.}
 \item{mar}{argument for \code{\link{par}(mar= .)} with a smaller
   default than usual, using the \code{marP} argument, see above.}
 \item{oma}{argument for \code{\link{par}(oma= .)}, by default for
   adding space for the \code{main} title if necessary.}
 \item{main}{character.  The main title to be used for the whole graphic.}
 \item{tit.wid}{numeric specifying the vertical width to be used for the
   main title; note that this is only used for the default value of
   \code{oma} (s. above).}
 \item{cex.main}{numeric; the character size to be used for the main title.}
 \item{line.main}{numeric; the line at which the title.}
 \item{col.main, font.main}{color and font for main title; see
   \code{\link{par}(*)} for details.}
 \item{quiet}{logical; if \code{TRUE}, do \emph{not} write the reminder
   about resetting \code{\link{par}}.}
 \item{\dots}{Further arguments to \code{\link{mtext}} for the main title.}
}
\value{
  A \code{\link{list}} with two components that are lists themselves, a
  subset of \code{\link{par}()},
  \item{new.par}{the current \code{par} settings.}
  \item{old.par}{the \code{par} \emph{before} the call.}
}
\author{Martin Maechler, UW Seattle, 1990.}
\seealso{\code{\link{par}}, \code{\link{layout}}.}
\examples{
opl <- mult.fig(5, main= expression("Sine Functions " * sin(n * pi * x)))
x <- seq(0, 1, len = 201)
for (n in 1:5)
  plot(x, sin(n * pi * x), ylab ="", main = paste("n = ",n))
par(opl$old.par)

rr <- mult.fig(mfrow=c(5,1), main= "Cosinus Funktionen", cex = 1.5,
               marP = - c(0, 1, 2, 0))
for (n in 1:5)
  plot(x, cos(n * pi * x), type = 'l', col="red", ylab ="")
str(rr)
par(rr$old.par)
## The *restored* par settings:
str(do.call("par", as.list(names(rr$new.par))))
}
\keyword{hplot}
