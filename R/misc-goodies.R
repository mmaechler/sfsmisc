#### misc-goodies.R
#### ~~~~~~~~~~~~~~  SfS - R - goodies that are NOT in
####		"/u/sfs/R/SfS/R/f.goodies.R"
####		"/u/sfs/R/SfS/R/u.goodies.R"
####		"/u/sfs/R/SfS/R/p.goodies.R"

###--- Original: From `S' in /u/sfs/S/misc-goodies.S
###--- ========  But start doing *less* here !

###>>>> ALSO take things from /u/maechler/S/Good.S
###			      ==================== at end

###==================================================================
###  Variables
###==================================================================

##- Browse <- T
##-
##- Dirname <- ""
##- Figdir <- "$PWD"
##-
##- Rm.Audit <- T
##- Rm.PostScript <- F
##- Save.dir <- "goodies.Data"
##-
##- file.prefix <- "unix('pwd')"

### Use those in u.dev.default:  Must set 'new = F' explicitly!
if(FALSE) {
DevDef.postscript<-{postscript(file="out.ps"); p_ par(); dev.off(); p$new_ F; p}
DevDef.x11 <- { x11("-geometry -9+9");  p_ par(); dev.off(); p$new_ F; p}
DevDef.motif <- .Alias(DevDef.x11)
}

## dirwst <- "/home/staff/stahel/S/.Data"

## obj <- "*[Dd]at*" ##-- Does anyone use this ?????

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

is.file <- function(file) system(paste("test -f", file), output = F) == 0


## apropos() : in standard R

get.sys.default <- function(obj.nam, verbose = T)
{
  ## Purpose: get(..) the 'S-plus' system version of  'obj.nam'
  ## -------------------------------------------------------------------------
  ## Arguments: obj.nam: [character] name of object to get
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 18 Oct 96, 16:37
  if(is.null(fnd <- find(obj.nam, num = T)))
    stop(paste('Object ', obj.nam, ' not found', sep='"'))
  ## else
  if(!is.character(obj.nam))  obj.nam <- as.character(substitute(obj.nam))
  if(verbose) cat(" .. found '",obj.nam,"'  ", length(fnd), " times\n", sep="")
  n.fnd <- names(fnd)
  if(0 == length(fnd.nr <- string.match("splus[3-9]", n.fnd)))
    stop(paste(" >> found '", obj.nam, "'  only in NON-splus paths:",
	       paste(n.fnd, collapse=","), sep=""))
  if(verbose) cat(" -- get()ing nr.", fnd.nr,":", n.fnd[fnd.nr],"\n")
  get(obj.nam, wh = fnd[fnd.nr])
}


##- Last <- function()
##- {
##-   ## Fn.name: Last
##-   ## Purpose: Loescht Audit-File beim Verlassen von Splus
##-   ## Author:  Caterina, Beat  , Date:  23/Nov/90, Martin: 14.Oct.91
##-   ## ----------------------------------------------------------------
##-   ## Arguments: Rm.Audit==T in unseren goodies gesetzt
##-   ## ----------------------------------------------------------------
##-   expression(if(exists(".Device")) .C("gr_wrap"))
##-   if(exists("Rm.Audit") && Rm.Audit)
##-     system("rm .Data/.Audi*", output = F)
##-   else system("rm -i .Data/.Audi*", output = F)
##- }



##-#### Vector, Matrix (or higher Array) stuff ########
##-###  -------------------------------------- ########

## "%in%"  in standard R


## The following two functions are especially useful if applied to a
## matrix: apply( mymatr, 1, first.max)
first.max <- function(x) min((1:length(x))[x == max(x)])
first.min <- function(x) max((1:length(x))[x == min(x)])

last <- function(data,length.out=1)
{
  ## Purpose: last element(s) of a vector
  ## Author: Werner Stahel, Date:  Tue Jan 21 17:29:42 1992
  ## ----------------------------------------------------------------
  ## Arguments:
  ## data: vector
  ## length: if positive, the result is the  length  last elements of data
  ##         if negative, the last  length  elements are dropped
  ## ----------------------------------------------------------------
  n <- length(data)
  data[sign(length.out)*(n-abs(length.out)+1):n]
}

empty.dimnames <- function(a)
{
  ## Purpose: 'Remove' all dimension names from an array for compact printing.
  ## Author: Bill Venables, Uni.Adelaide, OZ / Martin Maechler, Date: Sept 93
  ## ----------------------------------------------------------------
  ## Arguments: a: array, especially a matrix
  ## ----------------------------------------------------------------
  ## SEE ALSO:  unname
  ##
  ## Example:   empty.dimnames(diag(15)) ## looks much nicer
  d <- list(); l <- 0
  for(i in dim(a)) d[[l <- l + 1]] <- rep("", i)
  dimnames(a) <- d
  a
}

unname <- function(obj)
{
  ## Purpose: Remove the names of 'obj'
  ##         (sometimes the names "clobber" your screen)
  ## Does NOT alter a data.frame (since they NEED dimnames)!
  ## -------------------------------------------------------------------------
  ## Arguments: obj:  any object whose (dim)names should be removed
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1992
  ## -------------------------------------------------------------------------
  ## SEE ALSO:  empty.dimnames
  ##
  ## Example:    unname( conflicts() )
  if (length(names(obj))) names(obj) <- NULL
  if (length(dimnames(obj)) && !is.data.frame(obj)) dimnames(obj) <- NULL
  obj
}

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

### which() is in standard R !!


##-#### Plot / Devices  related stuff        ########
##-###  -----------------------------        ########
##-### >>>>> "p.goodies.S" or "ps.goodies.S" ########

subtit <- function(t) mtext(t, side = 3, line = 0)

rrange <- function(x, range = 1)
{
  ## Purpose: `Robust RANGE', using Tukey's notion of robust boxplot range
  ## -------------------------------------------------------------------------
  ## Arguments: [as with boxplot:] x: numeric data,
  ##		range: (default=1): Multiplier for ``1.5*(interquartile range)''
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1990

  boxplot.stats(x, coef=range)$stats[c(1,5)]
  ## S: (boxplot(..., plot = FALSE)$stats)[c(5, 1)]
}

give.xy.list <- function(x, y)
{
  stop("Use xy.coords() in R")
}

errbar <- function(x, y, yplus, yminus, cap=.015,
                   xlab= deparse(substitute(x)),
                   ylab= deparse(substitute(y)), ... )
{
  ## Purpose: Makes a plot with error bars
  ## Authors: Charles Geyer, Statistics, U. Chicago, geyer@galton.uchicago.edu
  ## 	  Martin Maechler, Date:  11 Apr 91  and  Mar 27 1992, 12:32
  ## ----------------------------------------------------------------
  ## Arguments: --- see  help(..) page --->  ?errbar
  ## ----------------------------------------=======

  plot( x, y, ylim= range(y,yplus,yminus), xlab=xlab, ylab=ylab, ... )
  xcoord <- par()$usr[1:2]
  segments( x, yminus, x, yplus )
  smidge <- cap * ( xcoord[2] - xcoord[1] ) / 2
  segments( x - smidge, yminus, x + smidge, yminus )
  segments( x - smidge, yplus, x + smidge, yplus )

}
## C.Monatsname , etc..  sind jetzt bei der zugehoerigen Funktion
##		u.Datumvonheute  in  /u/sfs/S/u.goodies.S

cum.Vert.funkt <- function(x, Quartile= TRUE, titel= TRUE, Datum= TRUE, rang.axis = TRUE,
			   xlab = "", main = "", ...)
{
  ## Ziel: Kumulative Verteilung von x aufzeichnen, auf Wunsch auch Median
  ##       und Quartile
  op <- par(xaxs = "r", yaxs = "r"); on.exit(par(op))# is the default anyway
  plot.step(x, xlab = xlab, main = main, ...)
  n <- length(x)
  if(rang.axis) axis(4, pos = par("usr")[1], at = (0:n)/n, labels = 0:n)
  if(titel) mtext("Kumulative Verteilungsfunktion", 3, line = 0.5)
  if(Quartile) for(i in 1:3) abline(h = i/4, lty = 2)
  if(Datum) p.datum()
}

boxplot.matrix <- function(mat, cols = TRUE, ...)
{
  ## Purpose: Boxplot for each column [cols = T]  or row [cols = F]  of a matrix
  ## -------------------------------------------------------------------------
  ## Arguments: mat: a numeric matrix;	  cols: logical, columns (T) or rows (F)
  ##		...: further arguments to 'boxplot(r, ...)':
  ##			range=NULL, width=NULL, varwidth=FALSE,
  ##			notch=FALSE, names=NULL, plot=TRUE, old=FALSE
  ##
  ## cols = F is 10% slower (for 3 grps a 50 obs. each, on a Sparc 1) ---
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler@stat.math.ethz.ch , 1995

  ## NOTE: For the case 'cols=TRUE', you can use
  ##	 boxplot(as.list(as.data.frame(mat)), ...)    [Renaud, S-news, 9/96]
  groups <- if(cols)  split(mat, rep.int(1:ncol(mat),
					 rep.int(nrow(mat), ncol(mat))))
  else  split(mat, seq(nrow(mat)))
  ##-- Make use of col/row names if present
  ##if (!is.null(nam <- dimnames(mat)[[1+cols]])) names(groups) <- nam
  if (0 < length(nam <- dimnames(mat)[[1+cols]])) names(groups) <- nam
  invisible(boxplot(groups,...))
}

plot.step <- function(ti, y,
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
    plot(ti, c(y[1],y), type= 'n', xlab= xlab, ylab= ylab, main= main, ...)
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

n.plot <- function(x, y, nam = NULL, abbr = n>=20 || max(nchar(nam))>=8, ...)
{
  ## Purpose: "Name Plot"; Names (or numbers) instead of points in plot(..)
  ## -------------------------------------------------------------------------
  ## Arguments: ALL as in  plot(...)  [ !! ]
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 16 Nov 92, Dec.93
  #- prt.DEBUG(x);
  if(exists("DEBUG") && DEBUG) str(x)
  if(missing(y)) {
    if(!is.null(clx <- class(x)) && clx == "formula") {
      warning("n.plot(.) does NOT yet work with  formula object !!")
      plot.formula(x, ...)
      return(invisible(x))
    } else if (is.list(x) & length(x)==2) { y <- x[[2]]; x <- x[[1]]
    } else if (is.matrix(x) & ncol(x)==2) { y <- x[,2] ; x <- x[,1]
    } else { y <- x; x <- seq(x) }
  }
  n <- length(x)
  plot(x, y, type = 'n', ...,
       xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))
  if(is.null(nam)) {
    nam <- names(x)
    if (is.null(nam)) {
      nam <- names(y)
      if (is.null(nam)) {
	nam <- paste(1:length(x)) #- Use 1,2,.. if no names
  }}}
  if(abbr) nam <- abbreviate(nam, min=1)
  if(!is.na(maybe.cex <- match("cex", names(list(...)))))
    cex <- list(...)[[maybe.cex]]
  else  cex <- par("cex")
  text(x, y, labels=nam, cex=cex)
}

TA.plot <- function(lm.res, fit = fitted(lm.res),
		    res = residuals(lm.res, "pearson"),
		    labels = NULL, main = mk.main(),
		    draw.smooth = n>=10, show.call = TRUE,  show.2sigma = TRUE,
		    lo.iter = NULL, lo.cex = NULL,
		    ...)
{
  ## Purpose: Produce a Tukey-Anscombe plot of a linear model fit
  ##	      Note that residuals and fitted are UN-correlated (IFF intercept..)
  ## -------------------------------------------------------------------------
  ## Arguments: lm.res = RESult of lm(..)
  ##    res : (weighted) residuals by default,
  ##	labels = 'symbols' for point, default(NULL): extract names or use seq.nr
  ##             use '*' to get simple '*' symbols.
  ##
  ## --- see on-line help by  "?TA.plot" !!
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: Dec 92 / Nov.93
  if(missing(main)) {
    call0 <- call <- lm.res $ call
    if(is.call(call[["formula"]]) && any(c("lm", "aov") == call[[1]]))
      call <- call[["formula"]]
    else {  #-- only formula part; no extra  'ARG ='
      if (length(call) >= 3)
	call <- call[c(1, match("formula", names(call)))]
      names(call)[2] <- ""
    }
    mk.main <- function() {
      cal <- get("call", frame = sys.parent())
      if(is.null(cal)) 	"Tukey-Anscombe plot of ???"
	else {
	  nc <- nchar(ccal <- deparse(cal)[1])
	  prt.DEBUG("|cal|=", length(cal), "; nchar(ccal) =", nc,": '", ccal,
		    "'\n", sep="")
	  if(nc > 36)
	    warning("TA.plot: 'main' title is long; consider using  cex= .9")
	  ##-- now should even go further:
	  ##--  E.g. if  nc > 50,  use  cex = .8 in the call to n.plot below
	  paste(if(nc < 13) "Tukey-Anscombe plot of :  "
		else if(nc < 24) "T.A. plot of: " else "TA-pl:", ccal)
	}
    }
  }
  yl <- "Residuals"
  if(!is.null(lm.res$weights)&& any(abs(lm.res$resid- res) > 1e-6*mad(res)))
    yl <- paste("WEIGHTED", yl)
  n.plot(fit, res, nam = labels, xlab = "Fitted values", ylab = yl,
	 main = main, ...)
  if(show.call)
    mtext(deparse(match.call()), side = 3, line = 0.5, cex = 0.4, adj=1)
  abline(h = 0, lty = 2, col = 2)
  p.mgp <- par("mgp")[1:2] #-- line numbers of margin text: xlab & label
  if(missing(lo.cex))
    lo.cex <- max(.2, min(0.8*par("mex"), .9*-diff(p.mgp))/par("mfg")[4])
  m.line <- if(par("mfg")[4]==1) .2+ p.mgp[1] else
                              max(p.mgp[1] - .2*lo.cex, sum(p.mgp)/2)
  if(show.2sigma) {
    s2 <- c(-2,2) * mad(res, center=0)
    rr <- range(res)
    if(s2[1]< rr[1] || s2[2] > rr[2])
      mtext(paste("2 sigma = ", format(s2[2])),
	    side= 1, line= m.line, adj = 0, cex= lo.cex)
    abline(h= s2, lwd=1.8, lty=4, col=4)
  }
  n <- length(res)
  if(draw.smooth) {
    ##-- lo.iter: idea of Werner Stahel:  no robustness for 'glm'  residuals
    if (is.null(lo.iter))
      lo.iter <- if(inherits(lm.res, "glm")&& lm.res$family[1]!="Gaussian")
	0  else  3
    f <- max(0.2, 1.25 * n^-.2) #'-- Martin's very empirical formula...
    lines(lowess(fit, res, f = f, iter = lo.iter),
	  lwd = 0, lty = 3, col = 3)
    ##- mtext with 2 times a 'cex' gives BUG [change of cex in global par]!
    ##- --> save  ALL  par()s  to restore !
    ##Q&D op <- par()
    ##Q&D  par(err=-1) # eliminate warnings from using ... below;
    ##Q&D  ... is to   get cex=.. and other parameters; this is "quick&dirty"
    ##Q&D  ...  ELIMINATED !! (use them above for n.plot !!)
    mtext(paste("-.-.-.- : lowess smooth (f =", format(round(f,2)),
		if(lo.iter!=3) paste(", it=", lo.iter), ")"),
	  side = 1, line = m.line, cex = lo.cex, adj = 1)
    ##Q&D par(op)
  }
  ##-	 "Correlation:", formatC(cor(fit,res), dig=3),
  ## mtext(paste(" -- Rank corr.:", formatC(cor(rank(fit),rank(res)), dig=3)) )
  invisible()
}




##-#### Print & Strings  ########
##-###  ===============  ########

ccat <-  ## character 'concat'
  function(...)     paste(..., collapse = "", sep = "")
vcat <- ## (numeric) vector 'concat'
  function(vec, sep = " ") paste(vec, collapse = sep)

str.vec <- function(name, digits = options()$digits)
{
  ## Purpose:
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, ~ 1992
  ## Example: x <- 1:4;  str.vec(x)   ##->  "x = 1 2 3 4"
  paste(paste(deparse(substitute(name))), "=",
	paste(f.format(name, digits = digits), collapse = " "))
}
signi <- function(x, digits = 6) round(x, digits - trunc(log10(abs(x))))

bl.string <- function(no) paste(rep(" ", no), collapse = "")

### symnum :  standard R function !!


##-#### Classes / Attributes, etc.   ########
##-### ----------------------------- ########

if(version$major < 5) oldClass <- class

doc <- function(object) attr(object, "doc")

"doc<-" <- function(x, value)
{
  ##-- Create doc attribute or  PREpend  new doc to existing one.
  attr(x, "doc") <- c(value, attr(x, "doc"))
  x
}

tit <- function(object) attr(object, "tit")

"tit<-" <- function(x, value)
{
  attr(x, "tit") <- value
  x
}


##-#### "Calculus" Mathematical stuff ########
##-###  ----------------------------- ########

bincoef <- .Alias(choose)

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
    if (lc==0) 0  else {
      r <- coef[lc]
      if (lc>1)
	for (i in (lc-1):1) r <- coef[i] + r*x
      r
    }
  } else { #-- coef is MATRIX --
    dc <- dim(coef)
    lc <- dc[2]; dc <- dc[1]
    n <- length(x)
    if (lc==0) matrix(0, n, dc) else {
      r <- matrix(coef[,lc], n, dc, byrow=TRUE)
      if (lc>1)
	for (i in (lc-1):1) r <- r*x + matrix(coef[,i], n, dc, byrow=TRUE)
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
    if (i>1) nvec <- nvec %/% base
  }
  r
}


##-#### "Miscellaneous" (not any other category) ########
##-###   ============= ------------------------- ########

new.seed <- function()
{
  ## Purpose: Randomize the seed for Random numbers.
  ## ------- this mainly for teaching / demo:
  ##         To make sure that each user will use DIFFERENT random numbers,
  ##	add a 'new.seed()'  to each  .First  (or have them type 'new.seed()' !)
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date:  4 Oct 95, 10:33
  ## -------------------------------------------------------------------------
  rU1 <- as.numeric(unix("date +%S%M.%H%j"))/5959 # in [0,1]
  rU2 <- (as.numeric(unix("echo $$")) %% 17)/16
  if(!exists("ichar", mode = "function"))
    ichar <- function(x) { ##-- from   library(examples)
	x <- as.vector(x, mode = "character")
	.C("ichar", as.character(x), length(x),
	   codes = integer(sum(nchar(x))))$codes }
  rU3 <- mean(ichar(getenv("USER")))/255 # such that each user is different
  rU <- rU1 * rU2 * rU3
  ##-- last digits :
  rU <- mean(c(rU1, rU2, rU3, 1024 * (rU - floor(1024 * rU)/1024)))
  ## this 'rU' has an approximate mean(4 uniform)  distribution on [0,1]
  invisible(set.seed(as.integer(1000 * rU)))
}

print.tbl <- function(table2, digit = 3)
{
  ##-- 2-weg Kontingenztafel mit allem zusammen ... -- ruft  cat.con(.)  auf
  ##-- Urspruneglich fuer NDK-Uebungen 1992
  ##-- Verbessert und Fehler korrigiert! : M.Maechler, Feb.1993
  d <- dim(table2)
  if(length(d) !=2)
    stop("Argument muss numerische Matrix sein: Die (2-Weg) Kontingenz Tafel")
  N <- sum(table2)
  cat("\nKontingenz-Tafel mit Randsummen:\n")
  cat.con (table2, 0)
  cat("\nGemeinsame Verteilung mit Randverteilungen:\n")
  I <- d[1];  J <- d[2];  df <- (I-1)*(J-1)
  r <- cat.con (table2/N, digit)
  joint <- r[1:I, 1:J]
  xrand <- r[I+1, 1:J]
  yrand <- r[1:I, J+1]
  condy <- joint/yrand
  condx <- t(t(joint)/xrand)
  cat("Bedingte Verteilung gegen y:\n"); print(round(condy,digit)); cat("\n")
  cat("Bedingte Verteilung gegen x:\n"); print(round(condx,digit)); cat("\n")
  exp.ind <- N * outer(yrand,xrand)#- Expected under INDEPendence: n * p_i * p_j
  cat("Freiheitsgrade: df =",df,"\n")
  cat("Chi^2 - Annahmebereich: [0,", round(qchisq(0.95,df),1),
      "] (alpha=0.05)\n\n\n", sep="")
  test.chisq <- sum((as.matrix(table2)-exp.ind)^2/exp.ind)
  cat("Testwerte unter der Unabhaengigkeitshypothese:\n")
  cat("  Test mit Chi^2: ",format(round(test.chisq,2)),
      " (P-Wert: ",round(1-pchisq(test.chisq,df),4),")\n",sep="")
  is.pos <- table2 != 0
  test.deviance <- 2*sum(table2[is.pos]*log(table2[is.pos]/exp.ind[is.pos]))
  cat("  Test mit Devianz:  ",format(round(test.deviance,2)),
      " (P-Wert: ",round(1-pchisq(test.deviance,df),4),")\n\n",sep="")
  invisible(list(p.condx=condx, p.condy=condy, expected.indep = exp.ind,
		 df=df, chisq.test=test.chisq, deviance=test.deviance))
}

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

cat.con <- function(mat, digi=3)
{
  ##-- "CAT CONtingency table"  mit RAND-SUMMEN + "Verzierung"
  ##-- Korrigiert fuer UNsymmetr. Kont.tafeln und stark vereinfacht: M.Maechler
  ## Gibt Resultat zurueck !
  ##>>> Hilfsfunktion fuer 'print.tbl' <<<
  mat <- as.matrix(mat)
  d <- dim(mat);  N <- d[1];  M <- d[2]
  mat <- rbind(cbind(mat, mat %*% rep(1, M)),
	       c(rep(1,N) %*% mat,  sum(mat)))
  out <- format(round(mat, digi))
  "--" <- paste(rep("-", max(nchar(out))), collapse = "")
  out <- cbind(rbind(out, get("--")), "|")
  print(out[c(1:N,N+2,N+1), c(1:M,M+2,M+1)], quote=FALSE)
  invisible(mat)			#--- die erweiterte Matrix --
}

###
### autoreg(),  mean.cor()  etc ... not yet
###
### if  we take them, use different file !!

hist.bxp <- function(x, nclass, breaks, probability=FALSE, include.lowest=TRUE,
		     xlab = deparse(substitute(x)), ..., width=0.2,
		     boxcol=3, medcol=0, medlwd=5, whisklty=2, staplelty=1)
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
      h <- hist(x, probability=probability, include.lowest=include.lowest,
		plot=FALSE)
      else
	h <- hist(x, nclass=nclass, probability=probability,
		  include.lowest=include.lowest, plot=FALSE)
  }
    else
      h <- hist(x, breaks=breaks, probability=probability,
		include.lowest=include.lowest, plot=FALSE)
  ymax <- max(h$counts)
  ymin <-  - ymax * width # range:  (-w,1)*ymax  instead of  (0,1)*ymax

  ##------- drawing the histogram -------------
  hist(x, breaks=h$breaks, probability=probability,
       include.lowest=include.lowest, plot=TRUE, xlab=xlab,
       ylim=c(ymin, ymax), axes=FALSE, ...)
  axis(1)
  axis(2, at=pretty(c(0,ymax), nint=5), srt=90)
  abline(h=0)				#
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
  p.hboxp(x, scale.r(par("usr")[3], 0,
		     f = .9 - max(0, .15 - width)*(1+(par("mfg")[3]>=3))),
	  boxcol=boxcol, medcol=medcol,
	  medlwd=medlwd, whisklty=whisklty, staplelty=staplelty)


}


####========== This is from /u/maechler/S/Good.S =============
####========== This is from /u/maechler/S/Good.S =============
####========== This is from /u/maechler/S/Good.S =============


##-#### Plot / Devices  related stuff ########
##-### ----------------------------- ########


### The following 2 functions should be one !! -- OKAY, eliminated  'pl' !
## NO MORE: pl <- function(...) plot(..., type = "b", xlab="", ylab="")

### Put   p.xy(.)  and  p.t(.)  into  SFS - Goodies  "/u/sfs/S/p.goodies.S"


##m.pl <- function(mat, ...)   matplot(mat[, 1], as.matrix(mat[, -1]), ...)
m.pl_ function(mat, ...) cat("\n>>> USE FUNCTION p.m  instead of  m.pl !!\n\n")

mpl <- function(mat, ...) {
  matplot(1:nrow(mat), mat, xaxt='n',...)
  if(0== length(dn <- dimnames(mat)[[1]]))
    axis(1) else
    axis(1, at = 1:nrow(mat), labels = dn)
}

is.TS <- function(x) is.ts(x) || is.rts(x) || is.cts(x) || is.its(x)

p.ts <- function(x, nrplots = max(1, min(8, n%/%400)), overlap = nk %/% 16,
                 main.tit=NULL, quiet = FALSE, ...)
{
  ## Purpose: ts[.]plot with multi-plots + Auto-Title -- currently all on 1 page
  ## -------------------------------------------------------------------------
  ## Arguments: x      : timeseries [ts,rts,its,cts] or numeric vector
  ##            nrplots: number of sub-plots    [DEFAULT: in {1..8}, ~= n/400]
  ##            overlap: how much should subsequent plots overlap [DEFAULT:...]
  ##            main.tit: MAIN title (over all plots)
  ##            ...    : further graphic parameters for tsplot(.)
  ## -------------------------------------------------------------------------
  ## Examples: p.ts(sunspots);  p.ts(sunspots, nr=1) # == tsplot(..)
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date:  1 Jul 1994; 18.Dec,1998.

  if(is.null(main.tit)) main.tit <- paste(deparse(substitute(x)))
  n <- length(x)
  if(nrplots==1) ts.plot(x, ..., main = main.tit)
  else {
    if(nrplots <= 0) return(nrplots)
    if(n<=1) stop("'x' must have at least two points!")
    do.dates <- !is.null(class(x)) && class(x) == "cts"
    if(do.dates) x <- as.rts(x)# dates() as below fails [S+ 3.4]
    scal <- (end(x) - (t1 <- start(x)))/(n-1)
    nk <- n %/% nrplots
    yl <- range(pretty(range(x, na.rm = TRUE)))
    mult.fig(mfrow=c(nrplots,1), main=main.tit, quiet = TRUE)
    for(i in 1:nrplots) {
      i0   <- max(0, (-overlap+(i-1)*nk)-1)
      in1 <- min(n, i*nk + overlap)-1
      st <- t1 + scal*i0 ##;  if(do.dates) st <- dates(st)
      en <- t1 + scal*in1##; if(do.dates) en <- dates(en)
      if(!quiet) cat(i," -- start= ", format(st), "; end  =", format(en),"\n")
      ts.plot(window(x, start= st, end  = en), ylim= yl, ...)
    }
    invisible(par(old.par))# global from 'mult.fig'
  }
}

pl.ds <- function(x, yd, ys, xlab = "", ylab = "", ylim = rrange(yd, ys),
                  xpd = TRUE, do.seg = TRUE,
                  lwd = 2.5, seg.p = .95, seg.lty = 2,
                  seg.col= if(.Device=="postscript") 1  else 2,
                  lin.col= if(.Device=="postscript") 1  else 3, lin.lty = 1,
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
  lines(x, ys, lwd = lwd, xpd = xpd, lty=lin.lty, col=lin.col)
  if(do.seg)
    segments(x, seg.p*ys + (1-seg.p)*yd, x, yd, col = seg.col,
             xpd = xpd, lty = seg.lty)
}

p.panelL <- function(x,y)      {text(x,y);lines(lowess(x,y, f=.4),col=2) }
p.panelS <- function(x,y,df=4) {text(x,y);lines(smooth.spline(x,y,df=df),col=2)}

pairs.title <- function(main, adj = NULL, cex = 1, lineP = 0, ...)
{
  ## Purpose:  Add a "title" to a pairs plot
  ## -------------------------------------------------------------------------
  ## Arguments: main:  title string to add
  ##            adj:  adjustment of string ~ [0,1] = [left, right]
  ##            cex:  character expansion to use.
  ##            lineP: 'line PLUS': extra line numbers (to shift UPwards)
  ##            ...  : FURTHER arguments to  "mtext(..)", e.g., 'col'
  ##-- Experiments: What is good when:
  ##  Dim.|    Device       |
  ##  p   | motif | PostSc. |
  ## -------------------------------------------------------------------------
  ##  4   |   -19 |     0   |
  ##      |       |         |
  ##      |       |         |
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 12 Aug 96, 15:57
  if(!exists('stringwidth', mode='function'))
    attach("/u/sfs/S/Statlib/postscriptfonts/.Data")
  nc <- stringwidth(main, inches = FALSE)
  ##-- the coord.system is in the middle ...
  lin <- switch(.Device,
                motif = 20,
                postscript = 1,
                0) + as.numeric(lineP)
  ##-- adj should actually depend on 'nc' : smaller for larger nc !
  ##-- ~~~ and very much on #{ncol} of pairs(.) but this is UNKNOWN here (!?)
  if(is.null(adj))
    adj <- switch(.Device,
                  motif = .036,
                  postscript = - .15,
                  0)
  mtext(main, line = lin, cex=cex, adj=adj, ...) #-- upper left
  mtext(paste(rep("_", ceiling(1.05* nc / stringwidth("_", inches=FALSE))),
              collapse=''), line = lin - .2*cex, cex=cex, adj=adj, ...)
  c(nc= nc, lin= lin, adj=adj)
}

nrpl.2.mfrow <- function(nr.plots)
{
  ## Purpose: 'NumbeR' of PLots   'to' 'mfrow'
  ##          Give goods 'default mfrow' for given number of plots
  ## -------------------------------------------------------------------------
  ## Arguments: nr.plots : integer
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1990 (UW, Seattle); 1996

  if      (nr.plots<= 3)  c(nr.plots,1) #-- 1, 2, 3
  else if (nr.plots<= 6)  c((nr.plots+1)%/%2,2)  #-- nr.plots = 4,5,6
  else if (nr.plots<=12)  c((nr.plots+2)%/%3,3)
  else c(nrow <-  ceiling(sqrt(nr.plots)), ceiling( nr.plots / nrow))
}

###-- This version from /u/maechler/R/MM-Goodies/mult.fig.R :
mult.fig <- function(nr.plots, mfrow, mfcol, marP = rep(0,4), mgp = c(1.5,.6,0),
                     main = NULL, quiet = .Device == "postscript",
                     tit.wid = if (is.null(main)) 0 else 4, tit.cex= 1.5, ...)
{
  ## Purpose: 'MULTiple FIGures' incl. TITLE and other good defaults
  ## -------------------------------------------------------------------------
  ## Arguments: tit.wid : heigth in 'cex' of title; use 4*k
  ##             -- Either ONE of the first 3 arguments --
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1990 (UW, Seattle) -- 1995
  ## -------------------------------------------------------------------------
  ## >>> calls  nrpl.2.mfrow(.) !
  mar <- marP + .1 + c(4,4,2,1) #-- my default 'mar'gins
  use.row <- missing(mfcol)
  if (use.row)
    if (missing(mfrow)) {
      if (missing(nr.plots))
        stop("must either specify 'nr.plots', 'mfrow' or 'mfcol' !")
      else  mfrow <- nrpl.2.mfrow (nr.plots)
    }
  Nrow <- (if(use.row) mfrow else mfcol)[1]
  if(Nrow > 2) { #-- Then R uses a much smaller character size --
    if(!is.null(main)) tit.cex <- tit.cex * 1.5
    tit.wid <- tit.wid * 1.5
  }
  oma <- c(0, 0, tit.wid, 0)
  old.par <<-
    if(use.row) par(mfrow = mfrow, oma= oma, mar = mar, mgp= mgp)
    else        par(mfcol = mfcol, oma= oma, mar = mar, mgp= mgp)
  if(!quiet) cat("Execute\n\t par(old.par) \n later to restore graphical par\n")
  ##---- now go ahead :
  ## nomore in R : frame()
  if (!is.null(main)) {# Do title *before* first plot!
      plot.new()
      mtext(main, side = 3, line = tit.wid-4, cex = tit.cex, outer = TRUE,
            font = par("font.main"), ...)
      par(new=TRUE)# reverse `plot.new()' above
  }
}

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
  plot(1:10,(1:10)^2, xlab="")
  pr <- par()
  u _ pr$ usr;  uy _ u[3:4];  ux _ u[1:2]
  cxy _ pr$ cxy;  em1 _ pr$"1em"
  mtext(paste("par(\"usr\")=: c(ux,uy)=",
              paste(format(u),  collapse=" "),
              if(all.equal(pr$ pin,    pr$ uin * c(diff(ux),diff(uy)),tol=1e-6))
                  "--- par(\"pin\" = par(\"uin\") * c(diff(ux),diff(uy))"
              ), line=3)
  mtext(paste("par(\"cxy\")=", paste(format(cxy),collapse=" "),
              "    par(\"1em\")=", paste(format(em1),collapse=" ")), line=2)
  mtext(paste("nx := diff(ux) / par(\"1em\")[1] = ",
              format(nx _ diff(ux) / em1[1])),side=1, line=2)
  mtext(paste("nx':= diff(ux) / par(\"cxy\")[1] = ",
              format(nx _ diff(ux) / cxy[1])),side=1, line=3)
  mtext(paste("ny := diff(uy) / par(\"1em\")[2] = ",
              format(ny _ diff(uy) / em1[2])),side=1, line=4)

  for(i in 1:ceiling(ny))
    mtext(paste("line=", i), line= -i, at = 1.8)
  for(i in 0:ceiling(nx))
    mtext(paste(i), side = 2, line= -i, at = uy[2] + em1[2]*i%%2)
  str <- paste(rep("123456789 ", ceiling(nx/10)), collapse="")
  str <- substring (str, 1, ceiling(nx))
  mtext(str, side = 3, line = -2)
}
