###  demo(tkdensity) ## is at
### /usr/local/app/R/r-devel/Linux-inst/demos/tcltk/tkdensity.R

require(tcltk) || stop("tcltk support is absent")

tkdensity <- function(y, n = 1024, log.bw = TRUE, showvalue = TRUE, 
                      from.fac = 1/1000, to.fac = 2)
{
    ## Purpose: as density() but with  scrollbar - bandwidth selection
    ## -----------------------------------------------------------------------
    ## Arguments:
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
    if(log.bw) lbw0 <- log10(bw0)

    xlim <- range(y) + c(-2,2)* bw0 # make this interactive as well!
    
    replot <- function(...) {
        if (is.null(y)) return() # too early...
        bw <<- b <-
            if(log.bw) 10 ^ as.numeric(tclvar$log.bw)
            else as.numeric(tclvar$bw)
        k <- tclvar$kernel
        eval(substitute(plot(density(y, bw = b, kernel = k, n = n),
                             main =  paste("density(",ynam,
                             ", bw = ",format(b, dig = 3),
                             ", kernel = \"", k, "\")", sep=""),
                             xlim = xlim, col = 2)))
        points(y,rep(0,size), col = 3)
   }

    replot.maybe <- function(...)
        if (as.numeric(tclvar$bw) != bw) replot()

    base <- tktoplevel()
    tkwm.title(base, "Density")

    base.frame <- tkframe(base, borderwidth = 2)
    frm.bw <- tkframe(base.frame) # bandwidth
    frm.k  <- tkframe(base.frame) # kernel
    q.but <- tkbutton(base,text = "Quit", command = function()tkdestroy(base))

    tkpack(base.frame, q.but)

    ## Bandwith Frame :
    bw.frame <- tkframe(frm.bw, relief = "groove", borderwidth = 3)
    tkpack(tklabel (bw.frame,
                    text = if(log.bw)"log10(Bandwidth)" else "Bandwidth"))
    tkpack(tkscale (bw.frame, command = replot.maybe,
                    from = if(log.bw) lbw0 - log10(from.fac)
                           else bw0 * from.fac,
                    to = if(log.bw) lbw0 + log10(to.fac) else bw0 * to.fac,
                    showvalue = showvalue,
                    variable = if(log.bw)"log.bw" else "bw",
                    resolution = if(log.bw) lbw0/20 else bw0/4 * from.fac,
                    orient = "horiz"))

    ## Kernel Frame :
    kern.frame <- tkframe(frm.k, relief = "groove", borderwidth = 2)
    tkpack(tklabel(kern.frame, text = "Kernel"))
    for (k.name in eval(formals(density)$kernel))
        tkpack(tkradiobutton(kern.frame, command = replot,
                             text = k.name, value = k.name, variable="kernel"),
               anchor = "w")

    tkpack( bw.frame, kern.frame, fill = "y")
    ##tkpack(frame1, kern.frame, fill="x")
    ##tkpack(frame3, bw.frame, fill="x")

    tkpack(frm.k, frm.bw, anchor = "n")

    tclvar$size  <- size
    tclvar$bw <- bw
    if(log.bw) tclvar$log.bw <- log10(bw)

    ## tclvar$dist  <- 1
    tclvar$kernel <- "gaussian"

    replot()

    ## Returning doesn't work!!
    ##return(tclvar[c("bw", "kernel")])
}
