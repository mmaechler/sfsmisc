\name{mult.fig}
\alias{mult.fig}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~}
\description{
 ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mult.fig(nr.plots, mfrow, mfcol, marP=rep(0, 4), mgp=c(1.5, 0.6, 0), mar=marP + 0.1 + c(4, 4, 2, 1), main=NULL, quiet=.Device == "postscript", tit.wid=if (is.null(main)) 0 else 4, cex.main=1.25, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{nr.plots}{ ~~Describe \code{nr.plots} here~~ }
 \item{mfrow}{ ~~Describe \code{mfrow} here~~ }
 \item{mfcol}{ ~~Describe \code{mfcol} here~~ }
 \item{marP}{ ~~Describe \code{marP} here~~ }
 \item{mgp}{ ~~Describe \code{mgp} here~~ }
 \item{mar}{ ~~Describe \code{mar} here~~ }
 \item{main}{ ~~Describe \code{main} here~~ }
 \item{quiet}{ ~~Describe \code{quiet} here~~ }
 \item{tit.wid}{ ~~Describe \code{tit.wid} here~~ }
 \item{cex.main}{ ~~Describe \code{cex.main} here~~ }
 \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
 ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of `comp1'}
  \item{comp2 }{Description of `comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	     or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (nr.plots, mfrow, mfcol, marP = rep(0, 4), mgp = c(1.5, 
    0.6, 0), mar = marP + 0.1 + c(4, 4, 2, 1), main = NULL, quiet = .Device == 
    "postscript", tit.wid = if (is.null(main)) 0 else 4, cex.main = 1.25, 
    ...) 
{
    use.row <- missing(mfcol)
    if (use.row) 
        if (missing(mfrow)) {
            if (missing(nr.plots)) 
                stop("must either specify 'nr.plots', 'mfrow' or 'mfcol' !")
            else mfrow <- nrpl.2.mfrow(nr.plots)
        }
    Nrow <- (if (use.row) 
        mfrow
    else mfcol)[1]
    if (Nrow > 2) {
        if (!is.null(main)) 
            cex.main <- cex.main * 1.5
        tit.wid <- tit.wid * 1.5
    }
    oma <- c(0, 0, tit.wid, 0)
    old.par <<- if (use.row) 
        par(mfrow = mfrow, oma = oma, mar = mar, mgp = mgp)
    else par(mfcol = mfcol, oma = oma, mar = mar, mgp = mgp)
    if (!quiet) 
        cat("Execute\n\t par(old.par) \n later to restore graphical par\n")
    if (!is.null(main)) {
        plot.new()
        mtext(main, side = 3, line = tit.wid - 4, cex = cex.main, 
            outer = TRUE, font = par("font.main"), ...)
        par(new = TRUE)
    }
    invisible(list(new.par = par(c("mfrow", "mfcol", "oma", "mar", 
        "mgp")), old.par = old.par))
  }
}
\keyword{ ~keyword }%-- one or more ...
