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

if(!exists("unname", mode="function")) # is in 0.90.1 (and on)
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

cum.Vert.funkt <- function(x, Quartile= TRUE, titel= TRUE, Datum= TRUE, rang.axis = TRUE,
			   xlab = "", main = "", ...)
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
	paste(f.format(name, digits = digits), collapse = " "))
}
signi <- function(x, digits = 6) round(x, digits - trunc(log10(abs(x))))

bl.string <- function(no) paste(rep(" ", no), collapse = "")

### symnum :  standard R function !!


##-#### Classes / Attributes, etc.   ########
##-### ----------------------------- ########

##if(version$major < 5) oldClass <- class

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

if(!is.R()) { 
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
  axis(2, at=pretty(c(0,ymax), n=5), srt=90) ## ph, 8.5.00: n instead of nint
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
  p.hboxp(x, scale.r(par("usr")[3], 0, ## ph, 8.5.00: changed f=.9 to f=.8
		     f = .8 - max(0, .15 - width)*(1+(par("mfg")[3]>=3))),
	  boxcol=boxcol, medcol=medcol,
	  medlwd=medlwd, whisklty=whisklty, staplelty=staplelty)


}



####========== This is from /u/maechler/S/Good.S =============
####========== This is from /u/maechler/S/Good.S =============


##-#### Plot / Devices  related stuff ########
##-### ----------------------------- ########


### The following 2 functions should be one !! -- OKAY, eliminated  'pl' !
## NO MORE: pl <- function(...) plot(..., type = "b", xlab="", ylab="")

### Put   p.xy(.)  and  p.t(.)  into  SFS - Goodies  "/u/sfs/S/p.goodies.S"


##m.pl <- function(mat, ...)   matplot(mat[, 1], as.matrix(mat[, -1]), ...)
##m.pl_ function(mat, ...) cat("\n>>> USE FUNCTION p.m  instead of m.pl !!\n\n")

mpl <- function(mat, ...) {
  matplot(1:nrow(mat), mat, xaxt='n',...)
  if(0== length(dn <- dimnames(mat)[[1]]))
    axis(1) else
    axis(1, at = 1:nrow(mat), labels = dn)
}

##Splus: is.TS <- function(x) is.ts(x) || is.rts(x) || is.cts(x) || is.its(x)
is.TS <- .Alias(is.ts) # shouldn't be used

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
      l[is.na(arr)] <- F
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
      l[is.na(arr)] <- F
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

list2mat <- function(x, check=TRUE)
{
  ## Purpose:  list -> matrix
  ## -------------------------------------------------------------------------
  ## Arguments: x a list whose first 2 el.  MUST be equal length vectors
  ##		check: if T, check if lengths are ok.   F: "quick & dirty"
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 19 May 93, 09:46
  if(!is.list(x)) stop("Argument must be list !")
  if(!exists("unname", mode="function"))
    unname <- function(x) { if(length(names(x))) names(x) <- NULL; x }
  if(!exists("which", mode="function"))
    which <- function(logi) (1:length(logi))[logi]
  p _ length(x) #--> number of columns
  n _ length(x[[1]])
  if( !is.vector(unname(x[[1]])) ||
     (p>1 && (!is.vector(unname(x[[2]])) || n!= length(x[[2]]))))
    stop("First 2 list entries must be equal length vectors")
  if(check) { #-- carefully check ... --
    len _ unlist(lapply(x,length))
    err _ len != n
    if(any(err)) {
      warning(paste("Wrong lengths of list elements",
		    paste(which(err),collapse=" "), "  --> eliminated."))
      p _ length(x _ x[ !err ])
    }
  }
  nuet _ "" == (collabs _ names(x))
  if(any(nuet)) collabs[nuet] <- paste("L", which(nuet), sep=".")
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

trace1.ms <- function(info, theta, grad, scale, flags, fit.pars)
{

  ## Purpose:  1-line -- trace function for 'ms' -- instead of trace.ms
  ## -------------------------------------------------------------------------
  ## Arguments: ... the same as 'trace.ms';
  ## ~~~~~~~~~  info is c(niter, nfun, fvalue, tracelevel, deltaf, pred.deltaf,
  ##	                  rel.deltatheta, step.scale, d*step.scale)
  ## -------------------------------------------------------------------------
  ## >>> calls 'formatC' which needs dyn.load
  ##
  ## ---> see also  MM.trace.ms(..)  which is somewhat different
  ##
  ## Author: Martin Maechler, 1994.
  ## -------------------------------------------------------------------------
  ## Example: ms( ~ ......, trace = trace1.ms) #--> ?ms for ex.

  cat("It.",formatC(info[1],w=2), ",",
      formatC(info[2],w=4), " f.ev, F=",
      formatC(info[3], w=11, dig=8),
      " Par.:", paste(formatC(theta,dig=6,w=9),collapse=" "),"\n", sep="")
  ##-S: cat("Iteration: ", info[1], ", ", info[2], " function calls, F= ",
  ##-S: 	format(info[3]), "\nParameters:\n")
  invisible(theta)
}


trace.ms.MM <- function(info, theta, grad, scale, flags, fit.pars)
{
  ## Purpose:  Much improved  trace.ms
  ## -------   Modified from STANDARD S -- Only ONE  line of output per iter.
  ##
  ## -------------------------------------------------------------------------
  ## Arguments: ... the same as 'trace.ms';
  ## ~~~~~~~~~ info[] = c(niter, nfun, fvalue, tracelevel, deltaf, pred.deltaf,
  ##	                  rel.deltatheta, step.scale, d*step.scale)
  ## -------------------------------------------------------------------------
  ## >>> calls 'formatC' which needs dyn.load
  ##
  ## ---> see also  trace1.ms(..)  which has no header and is somewhat different
  ##
  ## Author: Martin Maechler, May 1993
  ## -------------------------------------------------------------------------
  ## Example: ms( ~ ......, trace = trace.ms.MM) #--> ?ms for ex.

  w.th <- 6 #- print width of a theta[] component
  if(info[1]==0) {
    p <- length(theta)
    twt <- (w.th + 1) *p # Total width of theta[], including space (' ')
    cat(" It Fns relDpar  delta.F F(theta) theta",
        if(p>1) paste("[1..",p,"] ", paste(rep("-",twt-7), collapse=""),sep=""),
        "\n",
	" -- --- -------  ------- -------- ~~~~~~~~",
        if(p>1) paste(rep("~", twt), collapse=""),
        "\n",sep="")
  }
  cat(formatC(as.integer(info[1:2]),w=3),
      formatC(info[c(5,7)],w=8, dig=3-1,format='e'),
      formatC(info[3], dig=5, flag="#"),"",
      paste(formatC(theta,dig=4, flag="#"),collapse=" "),"\n")
  invisible(theta)
}

trace.nls.MM <- function(inner.outer, iteration, step.factor,
                         conv.criterion, objective, parameters, increment)
{
  ## Purpose: Improved  trace.nls
  ## -------  Modified from STANDARD S-plus
  ##
  ## -------------------------------------------------------------------------
  ## Arguments: ... the same as 'trace.nls';
  ## ~~~~~~~~~ info[] = c(niter, nfun, fvalue, tracelevel, deltaf, pred.deltaf,
  ##	                  rel.deltatheta, step.scale, d*step.scale)
  ## -------------------------------------------------------------------------
  ## >>> calls 'formatC' which needs dyn.load
  ##
  ## Author: Martin Maechler, May 1993
  ## -------------------------------------------------------------------------
  ## Example: nls( ~ ......, trace = trace.nls.MM) #--> ?nls for ex.

  if(inner.outer) {
    w.p <- 6 ##- print width of a param[] component
    if(iteration==1) {
      p <- length(parameters)
      twt <- (w.p + 1) *p ## Total width of parameters[], including space (' ')
      ##i cat(" It conv.crit step.fac increment | F(par.) param",
      cat(" It conv.cr. step.fac | F(param)  param",
          if(p>1) paste("[1..",p,"] ", paste(rep("-",twt-5), collapse=""),
                        sep=""),
          "\n",
          " -- -------- -------- | --------  ~~~~~~~~",
          if(p>1) paste(rep("~", twt), collapse=""),
          "\n",sep="")
    }
    cat(formatC(iteration,      w=3, format='d'),
        formatC(conv.criterion, w=8, dig=3-1,format='e'),
        formatC(step.factor,    w=8, dig=3-1,format='e'),
        ## increment is also [1:p] !!
        ##i -- don't really know what it is (delta.par / step.fac ? )
        ##i formatC(increment,      w=8, dig=3-1,format='e'),
        " ", formatC(objective, dig=5, w=8,flag="- #"),"",
        paste(formatC(parameters,dig=4, w=w.p, flag="#"),collapse=" "),"\n")

    ##-- trace.nls  magic
    assign("last.iteration", iteration, frame = 1)
    assign("it.row", c(objective, conv.criterion, parameters), frame = 1)
    ## [from nls.trace(): this only works if nls() is hacked :
    ##>>> eval(trace.expr, local = F)
  }
  return(list())
}


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
  if(!is.numeric(c)|| c<0) stop("'c' must be positive number")
  r <- x
  to.log <- abs(x) > c ; x <- x[to.log]
  r[to.log] <- sign(x) * c * (1 + log(abs(x/c)))
  r
}

###------- Numerical Derivatives ------------------------------------------

### Test Programs and examples for those two are in
### -->  "./NUMERICS/D1-tst.S"
###
### For 'optimal' 2nd Deriv.:  d2.est(..)
###  --> "./NUMERICS/diff2.S"  "./NUMERICS/diff2-user.S"

d1 <- function(y, x = 1)
{
  ## Purpose:  discrete trivial estimate of 1st derivative.
  ## -------------------------------------------------------------------------
  ## Arguments: x is optional
  ## -------------------------------------------------------------------------
  ##--> See also D1.naive in ~/S/D1-tst.S (and the (smoothing) one: 'D1') !
  ## Author: Martin Maechler, ~ 1990
  n <- length(y)
  if(length(x) == 1)
    c(y[2] - y[1], 0.5 * (y[-(1:2)] - y[-((n-1):n)]), y[n] - y[n-1])/x
  else {
    if(n != length(x)) stop("lengths of 'x' & 'y' must equal")
    if(!is.sorted(x))  stop("'x' must be sorted !")
    c(y[2] - y[1], 0.5 * (y[-(1:2)] - y[-((n-1):n)]), y[n] - y[n-1]) /
      c(x[2] - x[1], 0.5 * (x[-(1:2)] - x[-((n-1):n)]), x[n] - x[n-1])
  }
}

D1 <- function(x, y, xout = x, fudge.fact = 10)
{
  ## Purpose: Numerical first derivatives of  f() for   y_i = f(x_i) + e_i.
  ## Find  f'(xout)  -- using smoothing splines with GCV'
  ## Author: Martin Maechler, Date:  6 Sep 92, 00:04
  ## -------------------------------------------------------------------------
  ## Arguments: x = { x_i } MUST be sorted increasingly // y = { y_i }
  ## -------------------------------------------------------------------------
  sp <- smooth.spline(x,y)
  sp <- smooth.spline(x,y, spar = fudge.fact * sp $ spar)
  predict(sp, xout, deriv = 1) $ y
}

D1D2 <- function(x, y, xout = x, fudge.fact = 10, deriv=1:2, spl.spar=NULL)
{
  ## Purpose: Numerical first derivatives of  f() for   y_i = f(x_i) + e_i.
  ## Find  f'(xout) & f''(xout) -- using smoothing splines with GCV'
  ## Author: Martin Maechler, Date:  23 Sep 92, 9:40
  ## -------------------------------------------------------------------------
  ## Arguments: x = { x_i } MUST be sorted increasingly // y = { y_i }
  ## -------------------------------------------------------------------------
  if(missing(spl.spar)) {
    sp <- smooth.spline(x,y)
    sp <- smooth.spline(x,y, spar = fudge.fact * sp $ spar)
  } else sp <- smooth.spline(x,y, spar = spl.spar)
  list(D1 = if(any(deriv==1)) predict(sp, xout, deriv = 1) $ y,
       D2 = if(any(deriv==2)) predict(sp, xout, deriv = 2) $ y )
}
D2ss <- function(x, y, xout = x, fudge.fact = 10, spl.spar=NULL)
{
  ## Purpose: Numerical 2nd derivative of  f() for   y_i = f(x_i) + e_i.
  ##          Find  f''(xout) -- using smoothing splines (with GCV) -- DOUBLY:
  ##          f --ss-> f' --ss-> f''
  ## -------------------------------------------------------------------------
  ## Arguments: x = { x_i } MUST be sorted increasingly // y = { y_i }
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 29 Jan 97, 17:55
  ## -------------------------------------------------------------------------
  use.fudge <- is.null(spl.spar)
  if(use.fudge) { ##-- use  GCV * 'fudge.factor' ---
    if(is.null(fudge.fact)) stop("must specify 'spl.spar' OR 'fudge.fact'!")
    lf <- length(fudge.fact)
    if(!is.numeric(fudge.fact) || lf == 0 || lf > 2)
      stop("'fudge.fact' must be numeric(1 or 2) !")
    if(lf == 1) fudge.fact <- rep(fudge.fact, 2)
    sp <- smooth.spline(x,y)
    sp <- smooth.spline(x,y, spar = fudge.fact[1] * sp $ spar)
    spl.spar <- numeric(2); spl.spar[1] <- sp $ spar
  } else {
    lf <- length(spl.spar)
    if(!is.numeric(spl.spar) || lf == 0 || lf > 2)
      stop("'spl.spar' must be numeric(1 or 2) !")
    if(lf == 1) spl.spar <- rep(spl.spar, 2)
    sp <- smooth.spline(x,y, spar = spl.spar[1])
  }

  D1 <- predict(sp, x, deriv = 1) $ y #-- 1st derivative ...

  if(use.fudge) { ##-- use  GCV * 'fudge.factor' ---
    sp <- smooth.spline(x, D1)
    sp <- smooth.spline(x, D1, spar = fudge.fact[2] * sp $ spar)
    spl.spar[2] <- sp $ spar
  } else {
    sp <- smooth.spline(x, D1, spar = spl.spar[2])
  }
  list(x=xout, y = predict(sp, xout, deriv = 1) $ y,
       spl.spar = spl.spar, fudge.fact = fudge.fact)
}

integrate.xy <- function(x,fx, a,b, use.spline = T)
{
  ## Purpose: Compute \int_a^b f(t) dt; where f(x) is just interpolating
  ##	      (x[i],fx[i])  --- using trapezoid formula
  ## -------------------------------------------------------------------------
  ## Arguments: x, fx: real vectors of same length
  ##		a, b : (optional) integration limits. DEFAULT (a,b)= range(x)
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 16 May 94, 15:09
  ##-- needs function 'is.sorted'

  f.match <- function(x,table) match(as.single(x), as.single(table))

  if(is.list(x)) {
    fx <- x$y; x <- x$x
    if(!(n <- length(x))) stop("list 'x' has no valid $x component")
  } else n <- length(x)
  if(n != length(fx)) stop("'fx' must have same length as 'x'")

  if(!is.sorted(x)) { i <- sort.list(x); x <- x[i]; fx <- fx[i] }
  if(any(i <- duplicated(x))) { x <- x[!i];  fx <- fx[!i]; n <- length(x) }
  if(any(diff(x)==0))
     stop("bug in 'duplicated'  killed 'me': have still multiple x[]!")

  if(missing(a)) a <- x[1]
    else if(any(a < x[1])) stop("'a' must NOT be smaller than min(x)")
  if(missing(b)) b <- x[n]
    else if(any(b > x[n])) stop("'b' must NOT be larger  than max(x)")
  if(length(a)!=1 && length(b)!=1 && length(a)!=length(b))
    stop("'a' and 'b' must have length 1 or same length !")
    else {
      k <- max(length(a),length(b))
      if(any(b < a))    stop("all elements of 'b' must be >= 'a'")
    }

  if(use.spline) {
    xy <- spline(x,fx, n=max(1024, 3*n))
    ##-- Work around spline(.) BUG:  (ex.:  range(spline(1:20,1:20,n=95)))
    if(xy$x[length(xy$x)] < x[n]) {
      xy$x <- c(xy$x,  x[n])
      xy$y <- c(xy$y, fx[n])
    }
    ## END if work around ----
    x <- xy$x; fx <- xy$y
    n <- length(x)
  }

  kk <- length(ab <- unique(c(a,b)))
  xtol <- std.tolerance()*max(b-a)
  BB <- abs(dx <- outer(x,ab,"-")) < xtol
  if(any(j <- 0 == apply(BB,2,sum))) { #the j-th element(s) of ab are not in x[]
    y <- approx(x,fx, xout= ab[j])$y
    x <- c(ab[j],x)
    i <- sort.list(x)
    x <- x[i];  fx <- c(y,fx)[i];  n <- length(x)
  }

  ##--- now we could use 'Simpson's formula IFF the x[i] are equispaced... --
  ##--- Since this may well be wrong, just use 'trapezoid formula':

  ai <- rep(f.match(a,x), length=k)
  bi <- rep(f.match(b,x), length=k)
  dfx <- fx[-c(1,n)] * diff(x,lag=2)
  r <- numeric(k)
  for (i in 1:k) {
    a <- ai[i];  b <- bi[i]
    r[i] <- (x[a+1] - x[a])*fx[a] + (x[b] - x[b-1])*fx[b] +
      sum(dfx[seq(a, length=max(0,b-a-1))])
  }
  r/2
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
  if(missing(x)) x_ time(y)  else
  if(missing(y)) {
    if(is.list(x)) {
      if(any(is.na(match(c("x", "y"), names(x)))))
	stop("cannot find x and y in list")
      y <- x$y; x <- x$x; if(!is.null(x$w)) w <- x$w
    } else if(is.complex(x)) { y <- Im(x); x <- Re(x)
    } else if(is.matrix(x) && ncol(x) == 2) { y <- x[, 2];            x_ x[, 1]
    } else if(is.matrix(x) && ncol(x) == 3) { y <- x[, 2]; w_ x[, 3]; x_ x[, 1]
    } else { y <- x; x <- time(x)
    }
  }
  n <- length(x);  if(n != length(y)) stop("lengths of x and y must match")
  if(missing(w))  w_ rep(1,n)
    else if(n != length(w)) stop("lengths of x and w must match")
  ##--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--*--
  gr <- match(x,unique(x))
  cbind(x= unique(x),
	y= tapply(y, gr, FUN = fun.mean),
	w= tapply(w, gr, FUN = sum))
}



##-#### Non-calculus ("Discrete") Mathematical stuff ########
##-### -------------------------------------------- ########

is.sorted <- function(x) (length(x)<=1) || all(diff(x) >= 0)

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
  if(li==0) return(expression(NULL))
  else if(li==1) return(as.expression(i))
  ##-- now have: length(i) >= 2
  di1 <- abs(diff(i)) == 1	#-- those are just simple sequences  n1:n2 !
  subseq <- cbind(i[!c(F,di1)], i[!c(di1,F)]) #-- beginnings and endings
  mk.seq <- function(ij)
    if(ij[1]==ij[2]) as.character(ij[1]) else paste(c(ij),collapse=":")
  parse(text=
	paste("c(", paste(apply(subseq, 1, mk.seq), collapse=","), ")", sep=""))
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

quadrant <- function(x,y) { y <- sign(y); 2 - y + (y!=sign(x))}

n.code <- function(n, ndig=1, dec.codes = c("","d","c","k"))
{
  ##-- convert "round integers" to short char.strings
  ##-- useful to build-up  variable names in simulations
  ##-- e.g., n.code( c(10,20,90, 100,500, 2000,10000))#-> "1d" "2d" "9d" "1c" ..
  e10 <- floor(log10(n))
  nd <- length(dec.codes)
  if ( any(e10 < 0) || any(e10 >= nd)) {
    e10 <- pmax(0, pmin(e10, nd-1))
    warning("some `n' out of range")
  }
  paste(round(n/ 10^(e10 + 1 - ndig)), dec.codes[1 + e10],  sep="")
}

code2n <- function(ncod, ndig=1, dec.codes = c("","d","c","k"))
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
  y _ sign(c(y)); y_ y[y!=0]
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



##-#### Session managing / Debugging, etc. ########
##-### ---------------------------------- ########

prt.DEBUG <- function(..., LEVEL = 1)
  if (exists("DEBUG", w=1) && DEBUG >= LEVEL )#
  ##                  ~~~
  cat(paste("in `", sys.call(sys.nframe()-1)[1], "':", sep=""), ..., "\n")

##- ## Not w=1:
##- prt.DEBUG <- function(...)
##-   if (exists("DEBUG") && DEBUG )
##-         cat(paste("in `", sys.call(sys.nframe()-1)[1], "':", sep=""),
##- 	    ..., "\n")
#-- do NOT use  sep="" in cat(..)  --> fouls up  vectors of numbers

getenv.or.default <- function(var = "", default = "")
{
  ## Purpose: An easy user interface for 'getenv()'
  ## Author: Martin Maechler, Date:  May 15 1992, 11:53
  ## ----------------------------------------------------------------
  ## Arguments: var: Name of ENVIRONMENT variable.
  ##	default: Value to be substituted   IF ENV. var is empty/non-existent
  ## ----------------------------------------------------------------
  v <- getenv(var)
  if(v == "" || v == " ") default else v
}

mtime <- function(expr)
{
  ## Purpose: customized version of unix.time(..) -- to be called more than once
  ## Author: Martin Maechler, Date:  Jan 30 1992, 11:49
  ## ----------------------------------------------------------------
  ## Arguments: expr: expression to be timed
  ## ----------------------------------------------------------------
  if (!exists(".mtime")) .mtime <<- NULL
  tim <- unix.time(expr)[1]
  .mtime <<- c(.mtime, tim)
  return(tim)
}

##- sort.time _ NULL;  nn <- 25*2^(0:6)
##- for (n in nn) {
##-    .mtime <- NULL
##-    for (i in 1:10)
##- 	mtime(for (i in 1:10) sort.list(runif(n)))
##-    cat("n:",n," sort times :", .mtime, "\n")
##-    sort.time <- cbind(sort.time,  .mtime)
##- }
##- dimnames(sort.time)[2] <- list( paste("n=", nn, sep=""))

sys.start.time <- function()
{
  ## Purpose: Returns the Date+Time when THIS S[plus] session was started
  ##          in the form  'Wed Jun 23 11:50:25 1993' ~= but != date()
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 23 Jun 93
  ## -------------------------------------------------------------------------
  sys("grep '^#~New session' ",audit.file(), " | tail -1  | sed 's/.*Time: //'")
}

.updated <- date() #--- used when 'sourced' by 'make' (--> Makefile)
