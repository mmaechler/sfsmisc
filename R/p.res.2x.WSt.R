#### was part of ./p.goodies.R

### Exports :

### p.res.2x            Werner Stahels Plot; z.B Residuen gegen 2 x-Var.
### p.res.2fact         Aehnliche Idee: Residuen gegen 2 Faktoren (boxplots)

## p.wstPlot <- function(...)
## {
## warning("\n\n*** p.wstPlot(.) heisst neu p.res.2x(.)\n** Diese verwenden!\n")
##    p.res.2x(...)
## }

p.res.2x <-
  function(x, y, z, restricted = NULL, size = 1, slwd = 1, scol = 2:3,
           xlab = NULL, ylab = NULL, main = NULL,
           xlim = range(x), ylim = range(y), ...)
{
  ## Purpose:  Stahels Residuen-Plot
  ## Author:   ARu , Date:  11/Jun/91
  ## Aenderungen: MMae, 30/Jan/92, Dez.94 --> help(p.res.2x)
  if(is.null(xlab)) xlab <- deparse(substitute(x))
  if(is.null(ylab)) ylab <- deparse(substitute(y))
  if(is.null(main)) main <- deparse(substitute(z))

  ok <- !(is.na(x) | is.na(y) | is.na(z))
  x <- x[ok]; y <- y[ok]; z <- z[ok]
  ##
  ##--- restrict z values: ---
  az <- abs(z)
  has.restr <-
    if(is.null(restricted)) FALSE else any(restr <- az > restricted)
  if(has.restr) {
    z[z >   restricted] <- restricted
    z[z < - restricted] <- - restricted
  }

  ##--- fix plot region: ---
  pcm <- par("pin") * 2.54              #damit in cm
  ##--- damit im Plot das Symbol wirklich die Groesse size hat:
  size <- size/(2 * sqrt(2))
  fx <- (size * diff(xlim))/(pcm[1] - 2 * size)/2
  fy <- (size * diff(ylim))/(pcm[2] - 2 * size)/2
  ##--
  plot(x, y, xlim = xlim + c(-1,1)* fx, ylim = ylim + c(-1,1)* fy, pch = ".",
       xlab = xlab, ylab = ylab, main = main, ...)

  ##--- draw symbols: ---
  z <- z/max(az, na.rm = TRUE)
  usr <- par("usr")
  sxz <-     diff(usr[1:2])/pcm[1] * size * z
  syz <- abs(diff(usr[3:4])/pcm[2] * size * z)
  if(length(scol) == 2) scol <- scol[1 + as.integer(z < 0)]
  segments(x - sxz, y - syz,  x + sxz, y + syz, lwd = slwd, col = scol)

  ##--- mark restricted observations: ---
  if(has.restr) {
    points((x - sxz)[restr], (y - syz)[restr], pch = 8, mkh = 1/40)
    points((x + sxz)[restr], (y + syz)[restr], pch = 8, mkh = 1/40)
  }
  invisible()
}


p.res.2fact <-
    function(x, y, z, restricted, notch = FALSE,
             xlab = NULL, ylab = NULL, main = NULL)
{
    if(is.null(xlab)) xlab <- deparse (substitute (x))
    if(is.null(ylab)) ylab <- deparse (substitute (y))
    if(is.null(main)) main <- deparse(substitute(z))

    ok <- !(is.na(x) | is.na(y) | is.na(z))
    x <- x[ok]; y <- y[ok]; z <- z[ok]
    x <- as.factor(x)
    y <- as.factor(y)
    lx <- levels(x);  ly <- levels(y)

    ##--- restrict z values: ---
    if(missing(restricted))  restr <- FALSE
    else {
        if(!is.numeric(restricted) || restricted <= 0)
            stop("'restricted' must be POSITIVE !")
        if(any(restr <- abs(z) > restricted)) {
            zorig <- z
            z[z >  restricted] <-   restricted
            z[z < -restricted] <- - restricted
        }
    }
    rz <- range(z)
    par (mfrow = c(length(ly), 1), oma = c(5,6,6,0), mar = .1 + c(2,4,0,1))
    for (yv in rev(ly)) {
        Ind <- y == yv
        plot (x[Ind], z[Ind], ylim = rz, xlab = "", ylab = yv, notch = notch)
        abline(h = 0, lty = 3, lwd = 0)
        if(any(II <- restr & Ind)) {
            ## boxplot creates a coord.system with x = [-4, 104]
            jx <- as.numeric(x[II])     #-- in 1:length(lx)..
            cat("..Cut z=",format(zorig[II])," at ",
                xlab,"=",x[II],",  ", ylab, "=",yv,"\n")
            points( u.boxplot.x(length(lx),jx) , z[II]*1.02, pch = 8, mkh = 1/25)
        }
    }
    mtext (xlab, side = 1, line = 1, outer = TRUE, cex = 1.3)
    mtext (ylab, side = 2, line = 3, outer = TRUE, cex = 1.3)
    mtext (main, side = 3, line = 2, cex = 1.5, outer = TRUE)
    if(any(restr)) cat(sum(restr), "restricted observation(s)\n")
    invisible()
}


## Not sure if I want this (as global function).
## I had eliminated it long ago (from "SfS") but it's used above:

u.boxplot.x  <- function(n, j = 1:n, fullrange = 100)
{
  ## Purpose: Return the j-th x-coordinates in an 'n' side-by-side boxplot
  ## -------------------------------------------------------------------------
  ## Arguments: n : number of boxplots;  j: indices of boxplots
  ##  fullrange: x-coords as 'uniform' in [0,fullrange]  (f.=100, Splus 3.1,3.2)
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 19 Jan 95, 17:57
  cn <- fullrange/(3*n*(n+1))
  Dn <- cn*(3*n+2) ## Delta_{n}
  an <- cn*(2*n+1) ## a_{n}
  ## x(j) = an + (j-1)*Dn :
  an + (j-1)*Dn
}

