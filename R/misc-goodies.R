#### $Id: misc-goodies.R,v 1.15 2002/09/30 17:33:46 sfs Exp $
#### misc-goodies.R
#### ~~~~~~~~~~~~~~  SfS - R - goodies that are NOT in
####		"/u/sfs/R/SfS/R/u.goodies.R"
####		"/u/sfs/R/SfS/R/p.goodies.R"

###--- Original: From `S' in /u/sfs/S/misc-goodies.S
###--- ========  But start doing *less* here !

###==================================================================
###  Functions <<<<<<<< Please use a few subsections  like "Plotting"...
###  Functions <<<<<<<< See  --> "/u/maechler/S/Good.S"
###==================================================================
###
###	o bl.string 		# blabla string
###
### ==========================================================================


##-#### First, attach(.),... --- "Umgebung anschauen, modifizieren" #########
##-###  ~~~~~~~~~~~~~ ---------------------------------------------- ########
##-###  NOTA BENE: --> "First.S" contains  .First(.), etc.
##-###  ~~~~~~~~~       =======  be careful there !

sys <- function(...) system(paste(..., sep = ""))

unix.true <- function(command)  sys("if ", command,
				    "; then echo 1; else echo 0; fi")

is.file <- function(file) system(paste("test -f", file), output = FALSE) == 0


## apropos() : in standard R

get.sys.default <- function(obj.nam, verbose = TRUE)
{
  ## Purpose: get(..) the 'S-plus' system version of  'obj.nam'
  ## -------------------------------------------------------------------------
  ## Arguments: obj.nam: [character] name of object to get
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 18 Oct 96, 16:37
  if(is.null(fnd <- find(obj.nam, num = TRUE)))
    stop(paste('Object ', obj.nam, ' not found', sep = '"'))
  ## else
  if(!is.character(obj.nam))  obj.nam <- as.character(substitute(obj.nam))
  if(verbose) cat(" .. found '",obj.nam,"'  ", length(fnd), " times\n", sep = "")
  n.fnd <- names(fnd)
  if(0 == length(fnd.nr <- string.match("splus[3-9]", n.fnd)))
    stop(paste(" >> found '", obj.nam, "'  only in NON-splus paths:",
	       paste(n.fnd, collapse = ","), sep = ""))
  if(verbose) cat(" -- get()ing nr.", fnd.nr,":", n.fnd[fnd.nr],"\n")
  get(obj.nam, wh = fnd[fnd.nr])
}



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
  d <- list(); l <- 0
  for(i in dim(a)) d[[l <- l + 1]] <- rep("", i)
  dimnames(a) <- d
  a
}

## unname <- function(obj)     is in standard R, since version 0.90.1

## which() <- function(x,...)  is in standard R !!


nna <- function(data)
{
  ## Purpose: "No NA" :  throw out NA s, also for MATRIX data
  ## Author:   WSt, Date: Dec 89; simplified,improved: M.Maechler, Nov.93, 94
  ## --------------------------------------------------------------------
  ## ------ NOTE:  na.omit(.) is VERY SIMILAR (but funny for vectors !).
  Error <- "'nna(.)' is defined for vectors, matrices & data.frames only"
  if(is.atomic(data) || is.data.frame(data)) {
    ##-- should work for 'named vectors' [is.vector -> F !], data.frames,..
    if(is.null(dim(data))) data[!is.na(data)]
    else if(is.matrix(data))  data[!apply(is.na(data), 1, any), ]
    else stop(Error)
  } else stop(Error)
}


##-#### Plot / Devices  related stuff        ########
##-###  -----------------------------        ########
##-### >>>>> "p.goodies.S" or "ps.goodies.S" ########

subtit <- function(t) mtext(t, side = 3, line = 0)

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

boxplot.matrix <- function(x, cols = TRUE, ...)
{
  ## Purpose: Boxplot for each column [cols = T]  or row [cols = F]  of a matrix
  ## -------------------------------------------------------------------------
  ## Arguments: x: a numeric matrix;	  cols: logical, columns (T) or rows (F)
  ##		...: further arguments to 'boxplot(r, ...)':
  ##			range=NULL, width=NULL, varwidth=FALSE,
  ##			notch=FALSE, names=NULL, plot=TRUE, old=FALSE
  ##
  ## cols = F is 10% slower (for 3 grps a 50 obs. each, on a Sparc 1) ---
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler@stat.math.ethz.ch , 1995

  ## NOTE: For the case 'cols=TRUE', you can use
  ##	 boxplot(as.list(as.data.frame(x)), ...)    [Renaud, S-news, 9/96]
  groups <- if(cols)  split(x, rep.int(1:ncol(x),
					 rep.int(nrow(x), ncol(x))))
  else  split(x, seq(nrow(x)))
  ##-- Make use of col/row names if present
  ##if (!is.null(nam <- dimnames(x)[[1+cols]])) names(groups) <- nam
  if (0 < length(nam <- dimnames(x)[[1+cols]])) names(groups) <- nam
  invisible(boxplot(groups, ...))
}

cum.Vert.funkt <- function(x, Quartile = TRUE, titel = TRUE, Datum = TRUE,
                           rang.axis = TRUE, xlab = "", main = "", ...)
{
  ## Ziel: Kumulative Verteilung von x aufzeichnen, auf Wunsch auch Median
  ##       und Quartile
  op <- par(xaxs = "r", yaxs = "r"); on.exit(par(op))# is the default anyway
  plot.step(x, xlab = xlab, main = main, ...)
  #### FIXME : Use  package "stepfun" instead
  n <- length(x)
  if(rang.axis) axis(4, pos = par("usr")[1], at = (0:n)/n, labels = 0:n)
  if(titel) mtext("Kumulative Verteilungsfunktion", 3, line = 0.5)
  if(Quartile) for(i in 1:3) abline(h = i/4, lty = 2)
  if(Datum) p.datum()
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

#####- FIXME ----------- use library(stepfun), etc !!! ----------------

{
  ## Purpose: plot step-function  f(x)= sum{ y[i] * 1_[ t[i-1], t[i] ] (x) }
  ## -------------------------------------------------------------------------
  ## Arguments: for missing 'y', do empirical CDF; ==> ON-LINE Help "?plot.step"
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1990, U.Washington, Seattle; improved -- Dec.1993
  ##
  ## EXAMPLE: ##-- Plot empirical cdf  Fn(x)  for a small n:
  ## 	      xx_ runif(20); plot.step(xx); plot.step( xx, cad.lag = F )
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
  ## Author: Martin Maechler, ~ 1992 -- old name `str.vec()'
  ## Example: x <- 1:4;  paste.vec(x)   ##->  "x = 1 2 3 4"
  paste(paste(deparse(substitute(name))), "=",
	paste(format(name, digits = digits), collapse = " "))
}
signi <- function(x, digits = 6) round(x, digits - trunc(log10(abs(x))))

bl.string <- function(no) paste(rep(" ", no), collapse = "")

### symnum :  standard R function !!


##-#### Classes / Attributes, etc.   ########
##-### ----------------------------- ########



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

digits <- function(n, base = 10)
{
  ## Purpose: Give the vector A of the base-BASE representation of N:
  ##	      n = sum_{k=0}^M  A_{M-k} base ^ k ,   where  M = length(a) - 1
  ## Author: Martin Maechler
  if(n == 0) return(0)
  n <- abs(n) + 0.5
  powers <- round(base^((ceiling(log(n, base)):1) - 1))
  (n %/% powers) %% base
}

digits.v <- function(nvec, base = 2, num.bits = 1 + floor(log(max(nvec),base)))
{
  ## Purpose: Give the vector A of the base-_base_ representation of _n_:
  ## -------  n = sum_{k=0}^M  A_{M-k} base ^ k ,   where  M = length(a) - 1
  ## Value: MATRIX  M where  M[,i]  corresponds to  nvec[i]
  ##	c( result ) then contains the blocks in proper order ..
  ## Author: Martin Maechler, Date:  Wed Dec  4 14:10:27 1991
  ## ----------------------------------------------------------------
  ## Arguments: nvec: vector of POSITIVE integers
  ##	base: Base for representation
  ##	num.bits: Number of "bits"/digits to use
  ## EXAMPLE: digits.v(1:24, 8) #-- octal representation
  ## ----------------------------------------------------------------
  r <- matrix(0, nrow = num.bits, ncol = length(nvec))
  for (i in num.bits:1) {
    r[i,] <- nvec %% base
    if (i > 1) nvec <- nvec %/% base
  }
  r
}


##-#### "Miscellaneous" (not any other category) ########
##-###   ============= ------------------------- ########

table.mat <- function(mat, order.rows = TRUE)
{
  ##-- From S-news, Feb.1998, Phil Spector:

  ##	DATE INSTALLED:  30 Nov 1993             LAST REVISED:  5 Dec 1994
  ##    AUTHOR:  Phil Spector  (spector@stat.berkeley.edu)
  ##    REVISED BY: Scott D. Chasalow (sasssc@scri.sari.ac.uk)
  ##
  ##    PURPOSE:  Count occurrences of rows in a data.frame or matrix
  ##    ARGUMENTS:
  ##       mat:  a matrix, data frame, or vector
  ##       order.rows:  a logical value.  If true,  rows of result are sorted.
  ##    VALUE:   A data frame with a column for each column in
  ##             data.frame(mat),  and a final column of counts appended;
  ##             rows are the UNIQUE rows of mat.  Similar result,
  ##             but as a multi-way array,  may be obtained with the
  ##             call,  do.call("table",as.data.frame(mat)).
  ##
  ##    ***NOTE***
  ##             This function MAY fail to work correctly if any elements of
  ##             mat are character strings containing white space!
  ##             A possible fix, using an argument, "sep",  may be found in
  ##             function match.mat().
  ##    SEE ALSO:
  ##             match.mat, unique.mat
  ##
  nms <- NULL
  if(!is.data.frame(mat)) {
    nms <- dimnames(mat)[[2]]
    mat <- as.data.frame(mat)
  }
  if(any(sapply(mat, is.matrix))) {
    mat <- as.data.frame(as.matrix(mat), optional = TRUE)
    if(!is.null(nms))
      names(mat) <- nms
  }
  pmat <- do.call("paste", mat)
  which <- !duplicated(pmat)
  mat.use <- mat[which,  , drop = FALSE]
  mat.tab <- table(pmat)
  mat.use$Count <- mat.tab[match(pmat[which], names(mat.tab))]
  if(order.rows)
    mat.use <- mat.use[do.call("order", mat.use),  ]
  row.names(mat.use) <- paste(1:dim(mat.use)[1])
  mat.use
}

###
### autoreg(),  mean.cor()  etc ... not yet
###
### if  we take them, use different file !!

hist.bxp <- function(x, nclass, breaks, probability = FALSE, include.lowest = TRUE,
		     xlab = deparse(substitute(x)), ..., width = 0.2,
		     boxcol = 3, medcol = 0, medlwd = 5, whisklty = 2, staplelty = 1)
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


### The following 2 functions should be one !! -- OKAY, eliminated  'pl' !
## NO MORE: pl <- function(...) plot(..., type = "b", xlab="", ylab="")

### Put   p.xy(.)  and  p.t(.)  into  SFS - Goodies  "/u/sfs/S/p.goodies.S"


##m.pl <- function(mat, ...)   matplot(mat[, 1], as.matrix(mat[, -1]), ...)
##m.pl_ function(mat, ...) cat("\n>>> USE FUNCTION p.m  instead of m.pl !!\n\n")

mpl <- function(mat, ...) {
  matplot(1:nrow(mat), mat, xaxt = 'n',...)
  if(0 == length(dn <- dimnames(mat)[[1]]))
    axis(1) else
    axis(1, at = 1:nrow(mat), labels = dn)
}

##Splus: is.TS <- function(x) is.ts(x) || is.rts(x) || is.cts(x) || is.its(x)
is.TS <- is.ts#.Alias  shouldn't be used

pl.ds <- function(x, yd, ys, xlab = "", ylab = "", ylim = rrange(yd, ys),
                  xpd = TRUE, do.seg = TRUE,
                  lwd = 2.5, seg.p = .95, seg.lty = 2,
                  seg.col = if(.Device == "postscript") 1  else 2,
                  lin.col = if(.Device == "postscript") 1  else 3, lin.lty = 1,
                  ...)
{
  ## Purpose:   Plot Data & Smooth ---
  ## -------------------------------------------------------------------------
  ## Arguments:
  ##            do.seg: logical, plot "residual segments" iff T (= default).
  ##            further arguments to plot(x,yd,...), e.g.,  pch ='.'
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1990-1994

  plot(x, yd, xlab = xlab, ylab = ylab, ylim = ylim, ...) #pch = pch,
  lines(x, ys, lwd = lwd, xpd = xpd, lty = lin.lty, col = lin.col)
  if(do.seg)
    segments(x, seg.p*ys + (1-seg.p)*yd, x, yd, col = seg.col,
             xpd = xpd, lty = seg.lty)
}

p.panelL <- function(x,y)      { text(x,y);lines(lowess(x,y, f = .4),col = 2) }
p.panelS <- function(x,y,df = 4) { text(x,y);lines(smooth.spline(x,y,df = df),col = 2) }

test.par <- function()
{
  ## Things not yet  shown / proved below:
  ## 1) mai == mar * csi
  ## 2) fin == pin + c( mai[2]+mai[4], mai[1]+mai[3])
  ## 3) pin / fin == c(plt[2]-plt[1], plt[4]-plt[3])
  ##
  ## 4)  csi =?= cin[2]

  ## 2) ==> mai[2] + pin[1] + mai[4] ==  fin[1]
  ##        mai[1] + pin[2] + mai[3] ==  fin[2]
  ## 2)+3) ==> /  mai[1:2] ==    plt[c(1,3)] * fin
  ##           \  mai[4:3] == (1-plt[c(2,4)])* fin
  .Options$digits <- 3
  plot(1:10,(1:10)^2, xlab = "")
  pr <- par()
  u <- pr$ usr;  uy <- u[3:4];  ux <- u[1:2]
  cxy <- pr$ cxy;  em1 <- pr$"1em"
  mtext(paste("par(\"usr\")=: c(ux,uy)=",
              paste(format(u),  collapse = " "),
              if(all.equal(pr$ pin,    pr$ uin * c(diff(ux),diff(uy)),tol = 1e-6))
                  "--- par(\"pin\" = par(\"uin\") * c(diff(ux),diff(uy))"
              ), line = 3)
  mtext(paste("par(\"cxy\")=", paste(format(cxy),collapse = " "),
              "    par(\"1em\")=", paste(format(em1),collapse = " ")), line = 2)
  mtext(paste("nx := diff(ux) / par(\"1em\")[1] = ",
              format(nx <- diff(ux) / em1[1])),side = 1, line = 2)
  mtext(paste("nx':= diff(ux) / par(\"cxy\")[1] = ",
              format(nx <- diff(ux) / cxy[1])),side = 1, line = 3)
  mtext(paste("ny := diff(uy) / par(\"1em\")[2] = ",
              format(ny <- diff(uy) / em1[2])),side = 1, line = 4)

  for(i in 1:ceiling(ny))
    mtext(paste("line=", i), line = -i, at = 1.8)
  for(i in 0:ceiling(nx))
    mtext(paste(i), side = 2, line = -i, at = uy[2] + em1[2]*i%%2)
  str <- paste(rep("123456789 ", ceiling(nx/10)), collapse = "")
  str <- substring (str, 1, ceiling(nx))
  mtext(str, side = 3, line = -2)
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
  ##-- 'pmax.sa' --- special system "pmax" which gives back more-dim. arrays --
  ##- FASTER than pmax
  if(is.na(scalar)) array(NA, dim = dim(arr))
    else {
      l <- scalar > arr
      l[is.na(arr)] <- FALSE
      arr[l] <- scalar
      arr
    }
}

pmin.sa <- function(scalar, arr)
{
  ##-- 'pmin.sa' --- special system "pmin" which gives back more-dim. arrays --
  ##- FASTER than pmax
  if(is.na(scalar)) array(NA, dim = dim(arr))
    else {
      l <- scalar < arr
      l[is.na(arr)] <- FALSE
      arr[l] <- scalar
      arr
    }
}

diag.ex <- function(n)
{
  ## Purpose: Returns "the other diagonal" matrix
  ## Author: Martin Maechler, Date:  Tue Jan 14 09:40:36 1992
  ## ----------------------------------------------------------------
  ## Arguments: n: integer dimension of matrix
  ## ----------------------------------------------------------------
  e <- e1 <- rep.int(0, n-1)
  e[n-1] <- 1
  e <- c(0, rep.int(e,n), e1)
  dim(e) <- c(n,n)
  e
}

xy.grid <- function(x,y)
{
  ## Purpose: Produce the grid used by  persp, contour, .. as  N x 2 matrix
  ## -------------------------------------------------------------------------
  ## Arguments: x,y : any vectors of same mode
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 26 Oct 94, 10:40
  ## Example:  plot(xy.grid(1:7, 10*(0:4)))
  ##
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

list2mat <- function(x, check = TRUE)
{
  ## Purpose:  list -> matrix
  ## -------------------------------------------------------------------------
  ## Arguments: x a list whose first 2 el.  MUST be equal length vectors
  ##		check: if T, check if lengths are ok.   F: "quick & dirty"
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 19 May 93, 09:46
  if(!is.list(x)) stop("Argument must be list !")
  if(!exists("unname", mode = "function"))
    unname <- function(x) { if(length(names(x))) names(x) <- NULL; x }
  if(!exists("which", mode = "function"))
    which <- function(logi) (1:length(logi))[logi]
  p <- length(x) #--> number of columns
  n <- length(x[[1]])
  if( !is.vector(unname(x[[1]])) ||
     (p > 1 && (!is.vector(unname(x[[2]])) || n != length(x[[2]]))))
    stop("First 2 list entries must be equal length vectors")
  if(check) { #-- carefully check ... --
    len <- unlist(lapply(x,length))
    err <- len != n
    if(any(err)) {
      warning(paste("Wrong lengths of list elements",
		    paste(which(err),collapse = " "), "  --> eliminated."))
      p <- length(x <- x[ !err ])
    }
  }
  nuet <- "" == (collabs <- names(x))
  if(any(nuet)) collabs[nuet] <- paste("L", which(nuet), sep = ".")
  x <- matrix(unlist(x), n,p)
  dimnames(x) <- list(NULL, collabs)
  class(x) <- "matrix"
  x
}

tapply.num <- function(y, indices, Func)
{
  ## Purpose: Nicer result for tapply(..) when Function returns numeric
  ## 	      vector AND there is >= 2 "indices", i.e., categories.
  ## -------------------------------------------------------------------------
  ## Arguments: as for tapply,
  ##	Func: Must return [named, if possible] FIXED length vector
  ##	      [num/char]   EVEN for  NULL and NA !
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 14 Jun 93, 17:34
  ## -------------------------------------------------------------------------
  ## Example: tapply.num(y, list(cat1, cat2), range)
  rl <- tapply(y,indices,Func)
  if (is.list(rl)) { #-- when  >=2 indices  AND  length(Func(x)) > 1  ---
    if(any(Nas <- unlist(lapply(rl, is.null))))
      rl[Nas]  <- list(Func(NULL))
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

xy.unique.x <- function(x,y,w, fun.mean = mean)
{
  ## Purpose: given 'smoother data' (x_i, y_i) [and maybe weight  w_i]
  ##	      with multiple x_i, use unique x's, replacing y's by their mean
  ## -------------------------------------------------------------------------
  ## Arguments:
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date:  8 Mar 93, 16:36
  ##--*--*--*--*--*--*--*--*--*-- x,y,w  treatment --*--*--*--*--*--*--*--*--
  if(missing(x)) x <- time(y)  else
  if(missing(y)) {
    if(is.list(x)) {
      if(any(is.na(match(c("x", "y"), names(x)))))
	stop("cannot find x and y in list")
      y <- x$y; x <- x$x; if(!is.null(x$w)) w <- x$w
    } else if(is.complex(x)) { y <- Im(x); x <- Re(x)
    } else if(is.matrix(x) && ncol(x) == 2) { y <- x[, 2];            x <- x[, 1]
    } else if(is.matrix(x) && ncol(x) == 3) { y <- x[, 2]; w <- x[, 3]; x <- x[, 1]
    } else { y <- x; x <- time(x)
    }
  }
  n <- length(x);  if(n != length(y)) stop("lengths of x and y must match")
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

iterate.lin.recursion <- function(x, coeff, nr.it = 10)
{
  r <- c(x, rep(0, nr.it))
  n <- length(x)
  ic <- length(coeff):1
  for(i in 1:nr.it)
    r[n + i] <- c(coeff %*% r[n + i - ic])
  r
}
## iterate.lin.recursion(0:1, c(1,1))
##	 [1]  0  1  1  2  3  5  8 13 21 34 55 89  ### Fibonacci ##
## iterate.lin.recursion( c(1,0,1), rep(1,3))
##	 [1]   1   0   1   2   3   6  11  20  37  68 125 230 423

quadrant <- function(x,y) { y <- sign(y); 2 - y + (y != sign(x)) }

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
  ## -------------------------------------------------------------------------
  ## Arguments: y:  numeric sequence
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 17 Feb 93, 18:04

  ## Be careful with y[i] that were 0 !!
  y <- sign(c(y)); y <- y[y != 0]
  sum(y[-1] != y[-length(y)])
}

unif <- function(n, round.dig = 1 + trunc(log10(n)))
{
  ## Purpose: Give regular points on [-c,c] with
  ##	      mean 0 (exact) and variance ~= 1  (very close for EVEN n)
  ## -------------------------------------------------------------------------
  ## Arguments: n: number of points,   round.dig: to many digits AFTER "."
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, ~ 1990  (TESTING: in "~/S/unif.S" !)
  ##
  ## NOTE: It is easy to prove that   Var(1,2,...,n) = n(n+1)/12
  if(0 == n %% 2) {
    if(n == 0) NULL
      else round((2 * 1:n - (n + 1)) * sqrt(3/(n * (n + 1))), round.dig)
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
