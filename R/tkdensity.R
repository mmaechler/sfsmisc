###  demo(tkdensity) ## is at
### /usr/local/app/R/r-devel/Linux-inst/demos/tcltk/tkdensity.R

require(tcltk) || stop("tcltk support is absent")

tkdensity <- function(y, n = 1024, log.bw = TRUE, showvalue = TRUE, 
                      xlim = NULL,
                      from.f = if(log.bw) -2   else 1/1000,
                      to.f   = if(log.bw) +2.2 else 2, col = 2)
{
    ## Purpose: as density() but with  scrollbar - bandwidth selection
    ## -----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 8 Nov 2000, 19:00

    ynam <- deparse(substitute(y))
    size <- length(y)
    sd.y <- sqrt(var(y))

    ## Use Silverman's  Rule of Thumb initially :
    hi <- sd.y
    if (!(lo <- min(hi, IQR(y)/1.34))) 
        (lo <- hi) || (lo <- abs(x[1])) || (lo <- 1)
    bw <- bw0 <- 0.9 * lo * size^(-0.2)
    if(log.bw) lbw <- lbw0 <- log10(bw0)
    
    ry <- range(y)
    xlim <- xl0 <- if(is.null(xlim)) ry + c(-2,2)* bw0 else as.numeric(xlim)
    xlmid  <- xm0 <- mean(xlim)
    xrange <- xr0 <- diff(xlim)

    replot <- function(...) {
        if (is.null(y)) return() # too early...
        b <-
            if(log.bw)
                10 ^ (lbw <<- as.numeric(tclvar$log.bw))
            else bw <<- as.numeric(tclvar$bw)
        k <- tclvar$kernel
        xr.half <- tclvar$xrange / 2
        xlim <- tclvar$xlmid + c(-xr.half, xr.half)
        eval(substitute(plot(density(y, bw = b, kernel = k, n = n),
                             main =  paste("density(",ynam,
                             ", bw = ",format(b, dig = 3),
                             ", kernel = \"", k, "\")", sep=""),
                             xlim = xlim, col = col)))
        points(y,rep(0,size), col = 3)
   }

    replot.maybe <- function(...)
        if (log.bw && as.numeric(tclvar$log.bw) != lbw
            || as.numeric(tclvar$bw)     != bw
            || as.numeric(tclvar$xrange) != xrange
            || as.numeric(tclvar$xlmid)  != xlmid
            )
            replot()

    base <- tktoplevel()
    tkwm.title(base, paste("Tk Density(",ynam,")"))

    base.frame <- tkframe(base, borderwidth = 2)
    bw.frame   <- tkframe(base.frame, relief = "groove", borderwidth = 3)
    kern.frame <- tkframe(base.frame, relief = "groove", borderwidth = 2)
    xr.frame   <- tkframe(base.frame, relief = "groove")
    xmid.frame <- tkframe(base.frame, relief = "groove")
    q.but <- tkbutton(base,text = "Quit", command = function()tkdestroy(base))

    tkpack(base.frame,
           bw.frame, kern.frame,
           xr.frame, xmid.frame,
           q.but)

    ## Bandwith Frame :
    tkpack(tklabel (bw.frame,
                    text = if(log.bw)"log10(Bandwidth)" else "Bandwidth"))
    tkpack(tkscale (bw.frame, command = replot.maybe,
                    from = if(log.bw) lbw0 + (from.f) else bw0 * from.f,
                    to   = if(log.bw) lbw0 + (to.f)   else bw0 * to.f,
                    showvalue = showvalue,
                    variable = if(log.bw)"log.bw" else "bw",
                    resolution = if(log.bw) lbw0/20 else bw0/4 * from.f,
                    length = 200,
                    orient = "horiz"))

    ## Kernel Frame :
    tkpack(tklabel(kern.frame, text = "Kernel"))
    for (k.name in eval(formals(density)$kernel))
        tkpack(tkradiobutton(kern.frame, command = replot,
                             text = k.name, value = k.name, variable="kernel"),
               anchor = "w")

    ## [x range] Frame :
    tkpack(tklabel (xr.frame, text = "x range")
    tkpack(tkscale (xr.frame, command = replot.maybe,
                    from = xr0 / 100,
                    to   = xr0 * 10,
                    showvalue = showvalue, variable = "xrange",
                    ##resolution = ...
                    length = 80, orient = "horiz"))

    ## [x mid] Frame :
    tkpack(tklabel (xmid.frame, text = "x (zoom) mid")
    tkpack(tkscale (xmid.frame, command = replot.maybe,
                    from = xmid0 - 2*xr0,
                    to   = xmid0 + 2*xr0,
                    showvalue = showvalue, variable = "xlmid",
                    ##resolution = ...
                    length = 80, orient = "horiz"))

    tclvar$xrange <- xrange
    tclvar$xlmid  <- xlmid
    tclvar$size  <- size
    tclvar$bw <- bw
    if(log.bw)
        tclvar$log.bw <- log10(bw)
    tclvar$kernel <- "gaussian"

    replot()

    ## Returning doesn't work!!
    ##return(tclvar[c("bw", "kernel")])
}
