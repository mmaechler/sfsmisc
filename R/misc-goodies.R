#### $Id: misc-goodies.R,v 1.30 2004/11/05 07:48:19 maechler Exp $
#### misc-goodies.R
#### ~~~~~~~~~~~~~~  SfS - R - goodies that are NOT in
####		"/u/sfs/R/SfS/R/u.goodies.R"
####		"/u/sfs/R/SfS/R/p.goodies.R"

###--- Original: From `S' in /u/sfs/S/misc-goodies.S
###--- ========  But start doing *less* here !

###==================================================================
###  Functions <<<<<<<< Please use a few subsections  like "Plotting"...
###==================================================================

### ___Note___ we have some of these headers __MESS__
### But we leave it because of RCS {rather dismantle everything into 4-6 pieces

##-#### Vector, Matrix (or higher Array) stuff ########
##-###  -------------------------------------- ########

last <- function(x, length.out = 1, na.rm = FALSE)
{
    ## Purpose: last element(s) of a vector
    ## Author: Werner Stahel, Date:  Tue Jan 21 17:29:42 1992
    ## ----------------------------------------------------------------
    ## Arguments:
    ##   x:          vector
    ##   length.out: if positive, return the  length.out last elements of x,
    ##               if negative, the last  length.out  elements are dropped
    ## ----------------------------------------------------------------
    if (na.rm)
        x <- x[!is.na(x)]
    n <- length(x)
    x[sign(length.out)*(n-abs(length.out)+1):n]
}

empty.dimnames <- function(a)
{
    ## 'Remove' all dimension names from an array for compact printing.
    n <- length(da <- dim(a))
    if(n == 0) return(a)
    dimnames(a) <- lapply(1:n, function(i) rep("", da[i]))
    a
}


##-#### Plot / Devices  related stuff        ########
##-###  -----------------------------        ########
##-### >>>>> "p.goodies.S" or "ps.goodies.S" ########

errbar <- function(x, y, yplus, yminus, cap = 0.015,
                   xlab = deparse(substitute(x)),
                   ylab = deparse(substitute(y)), ... )
{
  ## Purpose: Makes a plot with error bars
  ## Authors: Charles Geyer, Statistics, U. Chicago, geyer@galton.uchicago.edu
  ## 	  Martin Maechler, Date:  11 Apr 91  and  Mar 27 1992, 12:32
  ## ----------------------------------------------------------------
  ## Arguments: --- see  help(..) page --->  ?errbar
  ## ----------------------------------------=======

  plot( x, y, ylim = range(y,yplus,yminus), xlab = xlab, ylab = ylab, ... )
  xcoord <- par()$usr[1:2]
  segments( x, yminus, x, yplus )
  smidge <- cap * ( xcoord[2] - xcoord[1] ) / 2
  segments( x - smidge, yminus, x + smidge, yminus )
  segments( x - smidge, yplus, x + smidge, yplus )
}
## C.Monatsname , etc..  sind jetzt bei der zugehoerigen Funktion
##		u.Datumvonheute  in  /u/sfs/S/u.goodies.S

boxplot.matrix <- function(x, use.cols = TRUE, ...)
{
  ## Purpose: Boxplot for each column or row [use.cols= TRUE / FALSE] of a matrix
  ## -------------------------------------------------------------------------
  ## Arguments: x: a numeric matrix;	  use.cols: logical, columns (T) or rows (F)
  ##		...: further arguments to 'boxplot(r, ...)':
  ##			range=NULL, width=NULL, varwidth=FALSE,
  ##			notch=FALSE, names=NULL, plot=TRUE, old=FALSE
  ##
  ## use.cols = F is 10% slower (for 3 grps a 50 obs. each, on a Sparc 1) ---
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler@stat.math.ethz.ch , 1995

  ## NOTE: For the case 'use.cols=TRUE', you can use
  ##	 boxplot(as.list(as.data.frame(x)), ...)    [Renaud, S-news, 9/96]
  groups <- if(use.cols)  split(x, rep.int(1:ncol(x),
					 rep.int(nrow(x), ncol(x))))
  else  split(x, seq(nrow(x)))
  ##-- Make use of col/row names if present
  ##if (!is.null(nam <- dimnames(x)[[1+use.cols]])) names(groups) <- nam
  if (0 < length(nam <- dimnames(x)[[1+use.cols]])) names(groups) <- nam
  invisible(boxplot(groups, ...))
}

cum.Vert.funkt <- function(x, Quartile = TRUE, titel = TRUE, Datum = TRUE,
                           rang.axis = n <= 20, xlab = "", main = "", ...)
{
  ## Ziel: Kumulative Verteilung von x aufzeichnen, auf Wunsch auch Median
  ##       und Quartile
  op <- par(xaxs = "r", yaxs = "r", las = 1)# the default anyway
  on.exit(par(op))
  r <- plotStep(x, xlab = xlab, main = main, ...)
  #### FIXME : stepfun() / ecdf() instead
  n <- length(x)
  if(rang.axis)
      axis(4, at = (0:n)/n, labels = 0:n, pos = par("usr")[1])#, las = 1)
  if(titel) mtext("Kumulative Verteilungsfunktion", 3, line = 0.5)
  if(Quartile) for(i in 1:3) abline(h = i/4, lty = 2)
  if(Datum) p.datum()
  invisible(r)
}


## This was "plot.step()" but that's in conflict with S3 methods
plotStep <- function(ti, y,
		      cad.lag = TRUE,
		      verticals = !cad.lag,
		      left.points = cad.lag,
		      right.points = FALSE,
		      end.points = FALSE,

		      add = FALSE,

		      pch = par('pch'),
		      xlab = deparse(substitute(ti)),
		      ylab = deparse(substitute(y)),
		      main = NULL,
		      ...)

#####- FIXME ----------- use stepfun(), plot.stepfun() etc !!! ----------------

{
  ## Purpose: plot step-function  f(x)= sum{ y[i] * 1_[ t[i-1], t[i] ] (x) }
  ## -------------------------------------------------------------------------
  ## Arguments: for missing 'y', do empirical CDF; ==> ON-LINE Help "?plot.step"
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1990, U.Washington, Seattle; improved -- Dec.1993
  ##
  ## EXAMPLE: ##-- Plot empirical cdf  Fn(x)  for a small n:
  ## 	      xx <- runif(20); plot.step(xx); plot.step( xx, cad.lag = F )
  ##	      plot.step( runif(20), add=T, cad.lag=F)
  xlab
  ylab
  if(missing(y)) {
    if(is.vector(ti) && is.numeric(ti)) {   # -- Do empirical CDF --
      nt <- length(ti)
      ti <- sort(ti)
      dt <- (ti[nt] - ti[1])/20
      ti <- c(ti[1] - dt, ti, ti[nt] + dt)
      n <- nt + 1
      y <- (0:nt)/nt
    } else {
      xy <- xy.coords(ti,NULL,NULL,NULL)
      ti <- c(xy$x[1], xy$x)
      y <- xy$y
      n <- length(y)
    }
  } else {
    n <- length(y)
    if(length(ti) != (n + 1))  stop("length(ti) MUST == length(y) + 1")
  }
  if(length(ti) != (n + 1) || length(y) != n)
    stop("NEVER CALLED! --length(ti) MUST == length(y) + 1")
  if(missing(main))  main <- deparse(sys.call())

  n1 <- n+1
  ##-- horizontal segments:
  if (add) segments(ti[-n1], y, ti[-1], y, ...)
  else {
    plot(ti, c(y[1],y), type = 'n', xlab = xlab, ylab = ylab, main = main, ...)
    segments(ti[-n1], y, ti[-1], y)
  }
  if(left.points)  points(ti[-n1],y, pch = pch)
  if(right.points) points(ti[-1], y, pch = pch)
  ##-- col=0 <==> "erase" :
  if(! end.points) points(ti[c(1,n1)], y[c(1,n)], pch = pch, col = 0)
  if(verticals) {
    if (add) segments(ti[2:n], y[-n], ti[2:n], y[-1], ...)
    else     segments(ti[2:n], y[-n], ti[2:n], y[-1])
  }
  invisible(list(t = ti, y = y))
}



##-#### Print & Strings  ########
##-###  ===============  ########

ccat <-  ## character 'concat'
  function(...)     paste(..., collapse = "", sep = "")
vcat <- ## (numeric) vector 'concat'
  function(vec, sep = " ") paste(vec, collapse = sep)

paste.vec <- function(name, digits = options()$digits)
{
  ## Purpose: Utility for "showing vectors"
  ## -------------------------------------------------------------------------
  ## Example: x <- 1:4;  paste.vec(x)   ##->  "x = 1 2 3 4"
  paste(paste(deparse(substitute(name))), "=",
	paste(format(name, digits = digits), collapse = " "))
}
signi <- function(x, digits = 6) round(x, digits - trunc(log10(abs(x))))

bl.string <- function(no) paste(rep(" ", no), collapse = "")

### symnum :  standard R function !!


##-#### "Calculus" Mathematical stuff ########
##-###  ----------------------------- ########

polyn.eval <- function(coef, x)
{
  ## Purpose: compute coef[1] + coef[2]*x + ... + coef[p+1]* x^p
  ##	if coef is vector, x can be any array; result      : of same dim. as x
  ##	if coef is matrix, x must be vector;   dim(result) = len(x) * nrow(coef)
  ##	    coef = matrix: evaluate SEVERAL polynomials (of same degree)
  ##	    ----   contains coefficient-vectors as ROWS ==> coef[,i] <-> x^{i-1}
  ## Author: Martin Maechler <maechler@stat.math.ethz.ch>
  if(is.null(dim(coef))) {
    lc <- length(coef)
    if (lc == 0) 0  else {
      r <- coef[lc]
      if (lc > 1)
	for (i in (lc-1):1) r <- coef[i] + r*x
      r
    }
  } else { #-- coef is MATRIX --
    dc <- dim(coef)
    lc <- dc[2]; dc <- dc[1]
    n <- length(x)
    if (lc == 0) matrix(0, n, dc) else {
      r <- matrix(coef[,lc], n, dc, byrow = TRUE)
      if (lc > 1)
	for (i in (lc-1):1) r <- r*x + matrix(coef[,i], n, dc, byrow = TRUE)
      r
    }
  }
}

digitsBase <- function(x, base = 2, ndigits = 1 + floor(log(max(x),base)))
{
    ## Purpose: Give the vector A of the base-_base_ representation of _n_:
    ## -------  n = sum_{k=0}^M  A_{M-k} base ^ k ,   where  M = length(a) - 1
    ## Value: MATRIX  M where  M[,i]  corresponds to  x[i]
    ## Author: Martin Maechler, Date:  Wed Dec  4 14:10:27 1991
    ## ----------------------------------------------------------------
    ## ---->  help(digitsBase) !
    ## ------------------------------
    if(any((x <- as.integer(x)) < 0))
        stop("`x' must be non-negative integers")
    r <- matrix(0, nrow = ndigits, ncol = length(x))
    if(ndigits >= 1) for (i in ndigits:1) {
        r[i,] <- x %% base
        if (i > 1) x <- x %/% base
    }
    r
}

sHalton <- function(n.max, n.min = 1, base = 2, leap = 1)
{
    ## Purpose: Halton sequence H(k,b) for k=n.min:n.max -- for Quasi Monte Carlo
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 29 Jul 2004, 21:34

    stopifnot((leap <- as.integer(leap)) >= 1)
    ## now do this via digitsBase(), later go directly
    nd <- as.integer(1 + log(n.max, base))
    dB <- digitsBase(if(leap == 1) n.min:n.max else seq(n.min, n.max, by=leap),
                     base = base, ndig = nd)
    colSums(dB/base^(nd:1))
}

QUnif <- function(n, min = 0, max = 1, n.min = 1, p, leap = 1)
{
  ## Purpose: p-dimensional ``Quasi Random'' sample in  [min,max]^p
  ## ----------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 29 Jul 2004, 21:43
  ## Example: plot(QUnif(1000, 2), cex=.6, pch=20, xaxs='i', yaxs='i')
    stopifnot(1 <= (p <- as.integer(p)),
              1 <= (n <- as.integer(n)),
              1 <= (leap <- as.integer(leap)),
              1 <= (n.min <- as.integer(n.min)))
    stopifnot((n.max <- n.min + (n - 1:1)*leap) < .Machine$integer.max)
    pr. <- c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,
             89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,
             179,181,191, 193,197,199,211,223,227,229,233,239,241,251,257,263,
             269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,
             367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457)
    if(length(pr.) < p) stop("primes not yet available for p=",p)
    pr <- pr.[1:p]
    if(leap > 1 && any(leap == pr) && length(pr.) >= p+1)
        pr <- c(pr[leap != pr], pr.[p+1])
    stopifnot(length(max) == p || length(max) == 1,
              length(min) == p || length(min) == 1)
    max <- rep.int(max, p)
    min <- rep.int(min, p)
    dU <- max - min
    r <- matrix(0., n, p)
    for(j in 1:p)
	r[,j] <- min[j] + dU[j] *
	    sHalton(n.max, n.min, base = pr[j], leap = leap)
    r
}



chars8bit <- function(i = 0:255)
{
    ## Purpose: Compute a character vector from its "ASCII" codes.
    ## We seem to have to use this complicated way thru text and parse.

    ## Author: Martin Maechler, Original date: Wed Dec 4, 1991
    ## this is an improved version of  make.ASCII() from ~/S/Good-string.S !
    ## ----------------------------------------------------------------
    i <- as.integer(i)
    if(any(i < 0 | i > 255)) stop("`i' must be in 0:255")
    i8 <- apply(digitsBase(i, base = 8), 2, paste, collapse="")
    c8 <- paste('"\\', i8, '"', sep="")
    eval(parse(text = paste("c(",paste(c8, collapse=","),")", sep="")))
}

strcodes <- function(x, table = chars8bit(0:255))
{
    ## Purpose: R (code) implementation of old S's ichar()
    ## ----------------------------------------------------------------------
    ## Arguments: x: character vector
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 23 Oct 2003, 12:42

    match0 <- function(x, table) match(x, table) - 1:1
    lapply(strsplit(x, ""), match0, table = table)
}

## S-PLUS has  AsciiToInt() officially, and   ichar() in  library(examples):
AsciiToInt <- ichar <- function(strings) unname(unlist(strcodes(strings)))





##-#### "Miscellaneous" (not any other category) ########
##-###   ============= ------------------------- ########

uniqueL <- function(x, isuniq = !duplicated(x)) {
    ## return list(ix, uniq)
    ## such that   all(x == uniq[ix])  and (of course)	uniq == x[isuniq]
    need.sort <- is.unsorted(x)
    if(need.sort) {
	xs <- sort(x, index.return = TRUE)
	ixS <- xs $ ix
	isuniq <- isuniq[ixS]
	x <- xs$x
    }
    ix <- as.integer(cumsum(isuniq))
    if(need.sort)
	ix <- ix[sort.list(ixS)]
    list(ix = ix, xU = x[isuniq])
}

###
### autoreg(),  mean.cor()  etc ... not yet
###
### if  we take them, use different file !!

hist.bxp <- function(x, nclass, breaks, probability = FALSE, include.lowest = TRUE,
		     xlab = deparse(substitute(x)), ..., width = 0.2,
		     boxcol = 3, medcol = 2, medlwd = 5, whisklty = 2, staplelty = 1)
{
  ## Purpose:   Plot a histogram and a boxplot
  ## -------------------------------------------------------------------------
  ## Arguments: ---> see help(hist.bxp) !
  ## -------------------------------------------------------------------------
  ## Authors: Christian Keller, Date: 10 Nov 95,  (Martin Maechler, Jan 96)
  ##						calls  p.hboxp(.) !

  ## determine the height of the plot
  if(missing(breaks)){
    if(missing(nclass))
      h <- hist(x, probability = probability, include.lowest = include.lowest,
		plot = FALSE)
      else
	h <- hist(x, nclass = nclass, probability = probability,
		  include.lowest = include.lowest, plot = FALSE)
  }
    else
      h <- hist(x, breaks = breaks, probability = probability,
		include.lowest = include.lowest, plot = FALSE)
  ymax <- max(h$counts)
  ymin <-  - ymax * width # range:  (-w,1)*ymax  instead of  (0,1)*ymax

  ##------- drawing the histogram -------------
  hist(x, breaks = h$breaks, probability = probability,
       include.lowest = include.lowest, plot = TRUE, xlab = xlab,
       ylim = c(ymin, ymax), axes = FALSE, ...)
  axis(1)
  axis(2, at = pretty(c(0,ymax), n = 5), srt = 90) ## ph, 8.5.00: n instead of nint
  abline(h = 0)				#
  ##-------- drawing the boxplot --------------

  ##-- scale a range
  scale.r <- function(x1,x2, fact = 1.1)
    (x1+x2)/2 + c(-fact,fact) * (x2-x1)/2

  ##-- since 4% extra space above x-axis (just below ymin):
  ##-   cat("par$usr[3:4]:", par("usr")[3:4],
  ##- 	    "  ymin -.04 *(ymax-ymin)",ymin -.04 *(ymax-ymin),"\n")
  ##-- NOTE: Always have (seemingly): par("usr")[3] == ymin -.04 *(ymax-ymin)

##-O- ORIGINAL VERSION (Keller & Keller) :
##-O-   p.hboxp(x, ymin, -.04 *(ymax-ymin),
##-O- 	  boxcol=boxcol, medcol=medcol,
##-O- 	  medlwd=medlwd, whisklty=whisklty, staplelty=staplelty)

  ##---- This is  much better for width <=.1 or so...
  ##-- but you should leave some white space -> scale down
  ##-- The scaling factor is really a  KLUDGE but works for a wide range!
  p.hboxp(x, scale.r(par("usr")[3], 0, ## ph, 8.5.00: changed f=.9 to f=.8
		     f = .8 - max(0, .15 - width)*(1+(par("mfg")[3] >= 3))),
	  boxcol = boxcol, medcol = medcol,
	  medlwd = medlwd, whisklty = whisklty, staplelty = staplelty)
}



####========== This is from /u/maechler/S/Good.S =============
####========== --------------------------------- =============

##-#### Plot / Devices  related stuff ########
##-### ----------------------------- ########

mpl <- function(mat, ...) {
  matplot(1:nrow(mat), mat, xaxt = 'n',...)
  if(0 == length(dn <- dimnames(mat)[[1]]))
    axis(1) else
    axis(1, at = 1:nrow(mat), labels = dn)
}

pl.ds <-
function(x, yd, ys, xlab = "", ylab = "", ylim = rrange(c(yd, ys)),
         xpd = TRUE, do.seg = TRUE, seg.p = .95,
         segP = list(lty = 2, lwd = 1,   col = 2),
         linP = list(lty = 1, lwd = 2.5, col = 3), ...)
{
    ## Purpose:   Plot Data & Smooth
    ## -------------------------------------------------------------------------
    ## Arguments: do.seg: logical, plot "residual segments" iff T (= default).
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, 1990-1994
    if(is.unsorted(x)) {
        i <- sort.list(x)
        x <- x[i]
        yd <- yd[i]
        ys <- ys[i]
    }
    plot(x, yd, xlab = xlab, ylab = ylab, ylim = ylim, ...) #pch = pch,
    lines(x, ys, xpd = xpd, lty = linP$lty, lwd = linP$lwd, col = linP$col)
    if(do.seg)
        segments(x, seg.p*ys + (1-seg.p)*yd, x, yd,
                 xpd = xpd, lty = segP$lty, lwd = segP$lwd, col = segP$col)
    invisible()
}



##-#### Matrix (or higher Array) stuff ########
##-### ------------------------------ ########

colcenter <- function(mat)  sweep(mat,2, apply(mat,2,mean))

col01scale <- function(mat, scale.func = function(x) diff(range(x)),
		       location.func = mean)
{
  ##-- See also 'scale' (std. S func) --
  mat <-  sweep(mat,2, apply(mat,2, location.func))
  sweep( mat, 2, apply(mat,2, scale.func), "/")
}

pmax.sa <- function(scalar, arr)
{
    ##-- special system "pmax" which gives back more-dim. arrays --
    if(is.na(scalar))
        arr[] <- scalar
    else {
        l <- scalar > arr
        l[is.na(arr)] <- FALSE
        arr[l] <- scalar
    }
    arr
}

pmin.sa <- function(scalar, arr)
{
    ##-- special system "pmin" which gives back more-dim. arrays --
    if(is.na(scalar))
        arr[] <- scalar
    else {
      l <- scalar < arr
      l[is.na(arr)] <- FALSE
      arr[l] <- scalar
    }
    arr
}

## diag.ex <- function(n)  --- now renamed :
diagX <- function(n)
{
  ## Purpose: Returns "the other diagonal" matrix
  ## Author: Martin Maechler, Date: Tue Jan 14 1992; Nov.2002
  ## ----------------------------------------------------------------
  ## Arguments: n: integer dimension of matrix
  ## ----------------------------------------------------------------
    m <- numeric(n * n)
    m[1+ (n-1)* (1:n)] <- 1
    dim(m) <- c(n,n)
    m
}

xy.grid <- function(x,y)
{
  ## Purpose: Produce the grid used by  persp, contour, .. as  N x 2 matrix
  nx <- length(x)
  ny <- length(y)
  cbind(rep(x,rep.int(ny,nx)),	rep(y,nx))
}

rot2 <- function(xy, phi)
{
  ## Purpose:  rotate xy-points by angle  'phi' (in radians)
  ## -------------------------------------------------------------------------
  ## Arguments: xy :  n x 2 matrix;   phi: angle (in [0, 2pi])
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 26 Oct 94, 22:16
  co <- cos(phi); s <- sin(phi)
  xy %*% t( matrix(c(co,s, -s, co), 2,2) )
}

tapplySimpl <- function(X, INDEX, FUN, ...)
{
  ## Purpose: Nicer result for tapply(..) when Function returns
  ## 	      vector AND there is >= 2 "INDEX", i.e., categories.
  ## -------------------------------------------------------------------------
  ## Arguments: as for tapply,
  ##	FUN: Must return [named, if possible] FIXED length vector
  ##	      [num/char]   EVEN for  NULL and NA !
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 14 Jun 93, 17:34
  rl <- tapply(X, INDEX, FUN, ..., simplify = TRUE)
  if (is.list(rl)) { #-- when  >=2 indices  AND  length(FUN(x)) > 1  ---
    if(any(Nas <- unlist(lapply(rl, is.null))))
      rl[Nas]  <- list(FUN(NULL))
    array(unlist(rl),
	  dim = c(length(rl[[1]]), dim(rl)),
	  dimnames = c(list(names(rl[[1]])), dimnames(rl)) )
  } else rl
}


##-#### "Calculus" Mathematical stuff ########
##-### ----------------------------- ########

u.log <- function(x, c = 1)
{
  ## Purpose:  log(.) only for high x- values ... identity for low ones
  ##  This  f(x) is  continuously differentiable (once).
  ##  f(x) = x				  for |x| <= c
  ##  f(x) = sign(x)*c*(1 + log(|x|/c))       for |x| >= c
  ## -------------------------------------------------------------------------
  ## Arguments: x: numeric vector;  c: scalar > 0
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 24 Jan 95, 17:28
  if(!is.numeric(c)|| c < 0) stop("'c' must be positive number")
  r <- x
  to.log <- abs(x) > c ; x <- x[to.log]
  r[to.log] <- sign(x) * c * (1 + log(abs(x/c)))
  r
}

xy.unique.x <- function(x,y, w, fun.mean = mean)
{
    ## Purpose: given 'smoother data' (x_i, y_i) [and maybe weight  w_i]
    ##	      with multiple x_i, use unique x's, replacing y's by their mean
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  8 Mar 93, 16:36
    ##--*--*--*--*--*--*--*--*--*-- x,y,w  treatment --*--*--*--*--*--*--*--*--
    if(missing(x)) x <- time(y)  else
    if(missing(y)) {
        if(is.list(x)) {
            if(any(is.na(match(c("x", "y"), names(x)))))
                stop("cannot find x and y in list")
            y <- x$y; x <- x$x; if(!is.null(x$w)) w <- x$w
        } else if(is.complex(x)) {
            y <- Im(x); x <- Re(x)
        } else if(is.matrix(x) && ncol(x) == 2) {
            y <- x[, 2]; x <- x[, 1]
        } else if(is.matrix(x) && ncol(x) == 3) {
            y <- x[, 2]; w <- x[, 3]; x <- x[, 1]
        } else {
            y <- x; x <- time(x)
        }
    }
    n <- length(x)
    if(n != length(y)) stop("lengths of x and y must match")
    if(missing(w))  w <- rep(1,n)
    else if(n != length(w)) stop("lengths of x and w must match")
    ##--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--
    gr <- match(x,unique(x))
    cbind(x = unique(x),
          y = tapply(y, gr, FUN = fun.mean),
          w = tapply(w, gr, FUN = sum))
}



##-#### Non-calculus ("Discrete") Mathematical stuff ########
##-### -------------------------------------------- ########

lseq <- function(from, to, length)
{
    ## Purpose: seq(.) : equidistant on log scale
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  3 Feb 2005, 08:34
    2^seq(log2(from), log2(to), length.out = length)
}

inv.seq <- function(i) {
  ## Purpose: 'Inverse seq': Return a short expression for the 'index'  `i'
  ## --------------------------------------------------------------------
  ## Arguments: i: vector of (usually increasing) integers.
  ## --------------------------------------------------------------------
  ## Author: Martin Maechler, Date:  3 Oct 95, 18:08
  ## --------------------------------------------------------------------
  ## EXAMPLES: cat(rr <- inv.seq(c(3:12, 20:24, 27, 30:33)),"\n"); eval(rr)
  ##           r2 <- inv.seq(c(20:13, 3:12, -1:-4, 27, 30:31)); eval(r2); r2
  li <- length(i <- as.integer(i))
  if(li == 0) return(expression(NULL))
  else if(li == 1) return(as.expression(i))
  ##-- now have: length(i) >= 2
  di1 <- abs(diff(i)) == 1	#-- those are just simple sequences  n1:n2 !
  subseq <- cbind(i[!c(FALSE,di1)], i[!c(di1,FALSE)]) #-- beginnings and endings
  mk.seq <- function(ij)
    if(ij[1] == ij[2]) as.character(ij[1]) else paste(c(ij),collapse = ":")
  parse(text =
	paste("c(", paste(apply(subseq, 1, mk.seq), collapse = ","), ")", sep = ""))
}

iterate.lin.recursion <- function(x, coeff, delta = 0, nr.it)
{
  r <- c(x, numeric(nr.it))
  n <- length(x)
  ic <- length(coeff):1
  for(i in 1:nr.it)
    r[n + i] <- delta + c(coeff %*% r[n + i - ic])
  r
}

quadrant <- function(x,y=NULL) {
    xy <- xy.coords(x,y); x <- xy$x; y <- xy$y
    Sgn <- function(u) ifelse(u >= 0, 1, -1)
    y <- Sgn(y); 2 - y + (y != Sgn(x))
}

n.code <- function(n, ndig = 1, dec.codes = c("","d","c","k"))
{
  ##-- convert "round integers" to short char.strings
  ##-- useful to build-up  variable names in simulations
  ##-- e.g.,
  nd <- length(dec.codes)
  e10 <- pmin(floor(log10(n) + 1e-12), nd - 1)
  if (any(e10 < 0)) {
      e10 <- pmax(0, e10) ; warning("some `n' too small")
  }
  ## IDEA: Things like
  ## ---- n.code(c(2000,1e4,5e4,6e5,7e6,8e7),
  ##             dec. = c("","d","c","k","-","-","M"))
  ## could work;  (not quite yet, see ex. above)
##-   if(any(id <- is.na(dec.codes) | dec.codes == "-")) {
##-       ## then use previous code for these (things like "20k", "300k")
##-       ## sequentially from the left:
##-       for(k in which(id)) {
##-           dec.codes[k] <- dec.codes[k - 1]
##-           ii <- 1+e10 == k
##-           e10[ii] <- e10[ii] - 1
##-       }
##-   }
  paste(round(n/ 10^(e10 + 1 - ndig)), dec.codes[1 + e10],  sep = "")
}

code2n <- function(ncod, ndig = 1, dec.codes = c("","d","c","k"))
{
  ## The inverse function to n.code
  le <- nchar(ncod)
  cod <- substring(ncod, le, le)
  as.integer(substring(ncod, 1, le-1)) * 10^(match(cod, dec.codes)-1)
}

nr.sign.chg <- function(y)
{
  ## Purpose:  Compute number of sign changes in sequence
  ## Be careful with y[i] that were 0 !!
  y <- sign(c(y))
  y <- y[y != 0]
  sum(as.integer(y[-1] != y[-length(y)]))
}

unif <- function(n, round.dig = 1 + trunc(log10(n)))
{
  ## Purpose: Give regular points on [-c,c] with mean 0 and variance ~= 1
  if(n %% 2 == 0) {
    if(n > 0) round((2 * 1:n - (n + 1)) * sqrt(3/(n * (n + 1))), round.dig)
  } else {
    m <- n %/% 2 #--> m+1 = (n+1)/2
    ( - m:m) * round(sqrt(6/((m + 1) * n)), round.dig)
  }
}

prt.DEBUG <- function(..., LEVEL = 1)
  if (exists("DEBUG", w = 1) && DEBUG >= LEVEL )#
  ##                  ~~~
  cat(paste("in `", sys.call(sys.nframe()-1)[1], "':", sep = ""), ..., "\n")

##- ## Not w=1:
##- prt.DEBUG <- function(...)
##-   if (exists("DEBUG") && DEBUG )
##-         cat(paste("in `", sys.call(sys.nframe()-1)[1], "':", sep=""),
##- 	    ..., "\n")
#-- do NOT use  sep="" in cat(..)  --> fouls up  vectors of numbers
