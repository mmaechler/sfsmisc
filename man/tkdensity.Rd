\name{tkdensity}
\alias{tkdensity}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tkdensity(y, n=1024, log.bw=TRUE, showvalue=TRUE, xlim=NULL, from.f=if (log.bw) -2 else 1/1000, to.f=if (log.bw) +2.2 else 2, col=2)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{n}{ ~~Describe \code{n} here~~ }
  \item{log.bw}{ ~~Describe \code{log.bw} here~~ }
  \item{showvalue}{ ~~Describe \code{showvalue} here~~ }
  \item{xlim}{ ~~Describe \code{xlim} here~~ }
  \item{from.f}{ ~~Describe \code{from.f} here~~ }
  \item{to.f}{ ~~Describe \code{to.f} here~~ }
  \item{col}{ ~~Describe \code{col} here~~ }
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
function (y, n = 1024, log.bw = TRUE, showvalue = TRUE, xlim = NULL, 
    from.f = if (log.bw) -2 else 1/1000, to.f = if (log.bw) +2.2 else 2, 
    col = 2) 
{
    ynam <- deparse(substitute(y))
    size <- length(y)
    sd.y <- sqrt(var(y))
    hi <- sd.y
    if (!(lo <- min(hi, IQR(y)/1.34))) 
        (lo <- hi) || (lo <- abs(x[1])) || (lo <- 1)
    bw <- bw0 <- 0.9 * lo * size^(-0.2)
    if (log.bw) 
        lbw <- lbw0 <- log10(bw0)
    ry <- range(y)
    xlim <- xl0 <- if (is.null(xlim)) 
        ry + c(-2, 2) * bw0
    else as.numeric(xlim)
    xlmid <- xm0 <- mean(xlim)
    xr0 <- diff(xlim)
    Tvar <- function(v) as.numeric(eval(substitute(tclvar$v)))
    replot <- function(...) {
        if (is.null(y)) 
            return()
        b <- if (log.bw) 
            10^(lbw <<- Tvar(log.bw))
        else bw <<- Tvar(bw)
        k <- tclvar$kernel
        xr.half <- (xr0/2) * 100/Tvar(xZoom)
        xlim <- Tvar(xlmid) + c(-xr.half, xr.half)
        eval(substitute(plot(density(y, bw = b, kernel = k, n = n), 
            main = paste("density(", ynam, ", bw = ", format(b, 
                dig = 3), ", kernel = \"", k, "\")", sep = ""), 
            xlim = xlim, col = col)))
        points(y, rep(0, size), col = 3)
    }
    replot.maybe <- function(...) if ((log.bw && Tvar(log.bw) != 
        lbw) || (!log.bw && Tvar(bw) != bw) || Tvar(xZoom) != 
        xZoom || Tvar(xlmid) != xlmid) 
        replot()
    base <- tktoplevel()
    tkwm.title(base, paste("Tk Density(", ynam, ")"))
    base.frame <- tkframe(base, borderwidth = 2)
    bw.frame <- tkframe(base.frame, relief = "groove", borderwidth = 3)
    kern.frame <- tkframe(base.frame, relief = "groove", borderwidth = 2)
    x.frame <- tkframe(base.frame)
    xr.frame <- tkframe(x.frame)
    xmid.frame <- tkframe(x.frame)
    tkpack(xr.frame, xmid.frame, side = "left", anchor = "s")
    q.but <- tkbutton(base, text = "Quit", command = function() tkdestroy(base))
    tkpack(base.frame, bw.frame, kern.frame, x.frame, q.but)
    tkpack(tklabel(bw.frame, text = if (log.bw) 
        "log10(Bandwidth)"
    else "Bandwidth"))
    tkpack(tkscale(bw.frame, command = replot.maybe, from = if (log.bw) 
        lbw0 + (from.f)
    else bw0 * from.f, to = if (log.bw) 
        lbw0 + (to.f)
    else bw0 * to.f, showvalue = showvalue, variable = if (log.bw) 
        "log.bw"
    else "bw", resolution = if (log.bw) 
        lbw0/20
    else bw0/4 * from.f, length = 200, orient = "horiz"))
    tkpack(tklabel(kern.frame, text = "Kernel"))
    for (k.name in eval(formals(density)$kernel)) tkpack(tkradiobutton(kern.frame, 
        command = replot, text = k.name, value = k.name, variable = "kernel"), 
        anchor = "w")
    tkpack(tklabel(xr.frame, text = "x zoom [%]"))
    tkpack(tkscale(xr.frame, command = replot.maybe, from = 5, 
        to = 500, showvalue = TRUE, variable = "xZoom", length = 80, 
        orient = "horiz"))
    tkpack(tklabel(xmid.frame, text = "x pan"))
    tkpack(tkscale(xmid.frame, command = replot.maybe, from = xm0 - 
        xr0, to = xm0 + xr0, showvalue = FALSE, variable = "xlmid", 
        resolution = xr0/2000, length = 80, orient = "horiz"))
    tclvar$xZoom <- xZoom <- 100
    tclvar$xlmid <- xlmid
    tclvar$size <- size
    if (log.bw) 
        tclvar$log.bw <- log10(bw)
    else tclvar$bw <- bw
    tclvar$kernel <- "gaussian"
    replot()
  }
}
\keyword{ ~keyword }%-- one or more ...
