#### $Id: p.goodies.R,v 1.9 2002/04/19 15:40:48 sfs Exp $
#### Original is /u/sfs/S/p.goodies.S  [v 1.12 1999/05/06 10:17:00 sfs Exp ]
####
### p.goodies.S ---- SfS- S(plus) - Funktionen, welche
### ---------------- mit  'p.' (für "Plot") beginnen  [alte SfS-Tradition ..]
###                        ==        =
###
### see also    "/u/sfs/S/u.goodies.S"
###             "/u/sfs/S/f.goodies.S"
###             "/u/sfs/S/misc-goodies.S"
###

###     **********************
###     INHALT von p.goodies.S  (bitte jeweils ergaenzen):
###     **********************

### p.clear             Bildschirm "putzen"
### p.datum             Deutsches Datum "unten rechts"
### p.dchisq            \
### p.dgamma             > Dichten plotten
### p.dnorm             /
### p.pairs             'pairs' mit mehr Moeglichkeiten
### p.pllines
### p.lm.hyperb         --> ./linesHyberb.lm.R
### p.scales
### p.triangle          Dreiecks-Plot fuer 3-er Gehalte / Anteile
### p.two.forget
### p.two.res
### p.tst.dev           Show Lines, Points and Colors for the current device
### p.profileTraces     Profil-Spuren fuer Nichtlineare Regression
### p.hboxp		Horizontale Boxplots
### p.arrows		Nicer arrows(): FILLED arrow heads
###
### ==========================================================================

p.clear <- function()
{
  ## Ziel: Bildschirm "putzen"     (MM: I think this is a wrong concept)
  par(mfrow = c(1, 1))
  plot(0:1,0:1, type = "n", ann = FALSE, axes = FALSE)
}

## ===========================================================================
p.datum <- function(outer = FALSE,...)
 mtext(u.Datumvonheute(...), 4, cex = 0.6, adj = 0, outer = outer)


## ===========================================================================

## curve(.. xlim..) only satisfactory from R version 1.2 on ..
p.dchisq <- function(nu, h0.col = "light gray", ...) {
  curve(dchisq(x, nu), xlim= qchisq(c(1e-5,.999), nu),
        ylab = paste("dchisq(x, nu=",format(nu),")"), ...)
  abline(h=0, col = h0.col)
}

p.dgamma <- function(shape, h0.col = "light gray", ...) {
  curve(dgamma(x, shape), xlim= qgamma(c(1e-5,.999), shape),
        ylab = paste("dgamma(x, shape=",format(shape),")"), ...)
  abline(h=0, col = h0.col)
}

p.dnorm <- function(mu = 0, s = 1, h0.col = "light gray",
                    ms.lines = TRUE, ms.col = "gray", ...)
{
  f <- function(x) dnorm(x, mu, s)
  curve(f, xlim = qnorm(c(1e-5, 0.999), mu, s),
        ylab = substitute(phi(x, mu == m, sigma == ss),
                          list(m=format(mu), ss=format(s))), ...)
  abline(h=0, col = h0.col)
  if(ms.lines) {
    segments(mu,0, mu, f(mu), col=ms.col)
    f.ms <- f(mu-s)
    arrows(mu-s, f.ms, mu+s, f.ms, length= 1/8, code= 3, col=ms.col)
    text(mu+c(-s/2,s/2), f.ms, expression(-sigma, +sigma), adj=c(.5,0))
  }
}

p.m <- function(mat, ...)
  matplot(mat[, 1], mat[, -1, drop = FALSE], ...)


## ===========================================================================

p.pairs <- function(data, data2, pch='.', col=1, colors, vnames, range=TRUE,
                    labcex=1.5, ...)
{
  ## Purpose: pairs() for two DIFFERENT set of variables against each other.
  ##          ~~~~~
  ## also with different plotting characters, marks and/or colors
  ## NB: also consider  pairs(..., panel = function(..) {....})
  ## -------------------------------------------------------------------------
  ## Arguments:  data   data for rows    (y axis)
  ##             data2  data for columns (x axis); DEFAULT: data2 := data
  ##             pch    characters (or marks) to be used
  ##             col    color index for each observation
  ##             colors colors to be used
  ##             vnames labels of variables; DEFAULT: dimnames of data, data2
  ##             range  if T:          use robust range
  ##                    if F or NULL:  use  usual range
  ##                    if array(2*nv) use  given range
  ##             labcex size of axis labels
  ##             ...    extra arguments,  passed to  par(.)
  ## -------------------------------------------------------------------------
  ## Author: Werner Stahel, Date: 23 Jul 93; minor bug-fix+comments: M.Maechler
    ##if(Browse) on.exit(browser())
  ##---------------------- preparations --------------------------
  data <- as.matrix(data)
  nv <- dim(data)[2]
  if (missing(data2)) { data2 <- data; lv2 <- 0
  } else { data <- cbind(data, as.matrix(data2)); lv2 <- nv }
  nv2 <- dim(data2)[2]
  nvv <- dim(data) [2]
  if (missing(vnames)) vnames <- dimnames(data)[[2]]
  if (is.null(vnames)) vnames <- c(paste("V",1:nv), paste("VV",1:nv2))
  num.pch <- is.numeric(pch)&!is.factor(pch)
  pch <- factor(pch)
  if(num.pch) mrk <- as.numeric(levels(pch))
  pch <- as.character(pch)
  cval <- unique(col)
  n.color <- length(cval)
  if(missing(colors)) colors <- if(is.numeric(col)) cval else 1:n.color

  rg <- if (is.logical(range) && range)  apply(data,2, rrange)
        else if(is.matrix(range))        range
  if(!is.null(rg)) for (j in 1:nvv) { #-- set to NA if outside range :
    dd <- data[,j]; data[dd < rg[1,j] | dd > rg[2,j], j] <- NA
  }
  par(mfrow=c(nv,nv2),oma=c(1,0,2,2)); par(mgp=c(1,0.1,0))
  par(cex=0.7, ...)
  cext <- par("cex")*labcex
  ##
  ##----------------- plots ----------------------------
  for (j in 1:nv) { #-- plot row [j]
    v <- data[,j]
    for (j2 in lv2+1:nv2) { #-- plot column  [j2-lv2] = 1:nv2
      v2 <- data[,j2]
##-- different lines of margin for plots
      if(j==1 & j2!=nvv) par(mar=c(2, 2, 1, 0))  ## top
      if(j==1 & j2==nvv) par(mar=c(2, 2, 1, 1))  ## upper-right corner
      if(j!=1 & j2==nvv) par(mar=c(2, 2, 0, 1))  ## right
      if(j!=1 & j2!=nvv) par(mar=c(2, 2, 0, 0))  ## otherwise
      plot(v2,v, type="n", xlab="", ylab="")
      if(j==1)    mtext(vnames[j2], side=3, line=0, cex=cext)
      if(j2==nvv) mtext(vnames[ j], side=4, line=0, cex=cext)
      if(any(v!=v2,na.rm=TRUE))
        for (cc in 1:n.color) {
          if (any((ii <- col==cval[cc]), na.rm=TRUE)) {#-- plot in current color
            cl <- colors[cc]
            if(num.pch) {
              for (mm in levels(pch))
                if (any((kk <- ii & pch==mm), na.rm=TRUE))
                  points(v2[kk], v[kk], pch = mrk[as.numeric(mm)], col=cl)
            } else text(data[ii,j2],data[ii,j],pch,col=cl)
          }
        }
      else { uu <- par("usr")
             text(mean(uu[1:2]),mean(uu[3:4]), vnames[j], cex=cext) }
    }
  }
  on.exit()
  cat("p.pairs(..)  done\n")
}

## ================================================================
p.pllines <- function(x,y,group,lty=c(1,3,2,4),...)
{
  ## Purpose:   lines according to group
  ## -------------------------------------------------------------------------
  ## Arguments:
  ## -------------------------------------------------------------------------
  ## Author: Werner Stahel, Date: 21 Jun 93, 15:45
  plot(x,y,type="n",...)
  ngr <- max(group)
  for (gg in 1:ngr) {
    ii <- group==gg & !is.na(x) & !is.na(y)
    if(sum(ii)) lines(x[ii],y[ii],lty=lty[1+(gg-1)%%length(lty)])
  }
}
## ===========================================================================
p.plot.lm <-
function(rr, y, ask = TRUE, qq=TRUE, ta=TRUE, ta.lowess=TRUE, tamod=TRUE, tamod.lowess=TRUE,
         hat=TRUE, x=NULL, xadd=NULL, x.lowess=TRUE,
         main=tit(rr), ...)
{
  ## Purpose:  more plots for residual analysis
  ## -------------------------------------------------------------------------
  ## Arguments: rr: an lm object, preferably generated by f.lm
  ##            y:  y variable (optional)
  ##            the following arguments are T if the respective plots
  ##            and enhancements are wanted
  ##            qq: normal plot of standardized residuals
  ##            ta: Tukey-Anscombe (res vs. fit)
  ##            tamod: modified ta: abs(st.res) vs. fit
  ##            hat:   res vs. hat
  ##            x:     list of names of x-variables against which res are
  ##                   to be plotted
  ##            xadd:  additional x s
  ##            .. .lowess: add lowess to respective plots
  ## -------------------------------------------------------------------------
  ## Author: Werner Stahel, Date:  7 May 93, 13:46
    ##if(Browse) on.exit(browser())         #
  op <- par(ask = ask)
  form <- formula(rr)
  rr$fitted.values <- rr$fit
  f <- predict(rr)
  r <- rr$res
  if(missing(y)) {
    y <- f + r
    yname <- deparse(form[[2]])
  }
  else yname <- deparse(substitute(y))
  stres <- rr$stres
  rname <- paste("res(",yname,")")
  strname <- paste("st",rname,sep=".")
  if (is.null(stres)) {stres <- r ; strname <- rname}
  if (qq) qqnorm(stres,ylab=strname, main=main)
  fname <- paste("Fitted:", deparse(form[[3]]), collapse = " ")
  if(is.null(main)) main <- ""
  notna <- !is.na(r)
  if(ta) { plot(f, r, xlab = fname, ylab = rname, main=main, ...)
           abline(0, 0, lty = 2)
           if (ta.lowess) lines(lowess(f[notna],r[notna]),lty=2) }
  ra <- abs(stres)
  if (tamod) {
    plot(f, ra, xlab = fname, ylab ="abs(st.res)", main=main,
         ...)
    if (tamod.lowess) lines(lowess(f[notna],ra[notna]),lty=2) }
  h <- rr$h
  if(hat&!is.null(h)) {
    plot(h,r, xlab="hat diagonal",ylab=rname, main=main, ...)
    abline(0,0,lty=2) }
  if (length(x)>0) {
    if(is.logical(x)) x <- if(x) as.character(terms(form)) else NULL
    for (xx in c(x,xadd)) {
      plot (eval(parse(text=xx)),r, xlab=xx, ylab=rname, main=main, ...)
      abline(0,0,lty=2)
      if (x.lowess)
        lines(lowess(eval(parse(text=xx))[notna],r[notna]),lty=3) }}
  on.exit(par(op))
  "done"}
## ================================================================

p.scales <- function(unit = relsysize * 2.54 * min(pin), relsysize = 0.05)
{
  ## Fn.name:  p.scales
  ## Purpose:  Conversion between plot scales: usr, cm, symbol
  ## Author:   W. Stahel , Date: May/90; updated: M.Mae. 9/93
  ## ----------------------------------------------------------------
  ## Arguments:
  ##   unit: length of unit (or x and y units) of symbol coordinates in cm
  ##   relsysize:  same, as a proportion of the plotting area
  ## ----------------------------------------------------------------
  usr <- par("usr")
  pin <- par("pin")
  usr2cm <- (2.54 * pin)/(usr[c(2, 4)] - usr[c(1, 3)])
  sy2usr <- unit/usr2cm                 # result
  rs <- cbind(sy2usr, usr2cm)
  dimnames(rs) <- list(c("x", "y"), c("sy2usr", "usr2cm"))
  class(rs) <- c("gpar", "scale.factors")
  rs
}


p.triangle <- function(mat, label= "*", text.ecken = rep("",3), dreieck = TRUE)
{
  ## Purpose: 'Triangle plot' for plotting 3 proportions [a + b + c == 1]
  ## ----------------------------------------------------------------
  ## Arguments:
  ##      mat:   Matrix mit % A in der 1. Kolonne, % B in der 2.. (range 0:1)
  ##      label: Text der Laenge von mat[,1] zur Identifikation der Punkte
  ##      text.ecken: Ecken-Beschriftung
  ##      dreieck   : Falls T wird der Dreiecksrahmen gezeichnet
  ## EXAMPLE: p.triangle(rbind(c(.8,0),c(.1,.8),c(.1,.2), c(1,1)/3),label=1:4)
  ## ----------------------------------------------------------------
  par(pty = "s")
  pa <- mat[, 1]
  pc <- 1 - mat[, 1] - mat[, 2]
  if(any(pc<0 | pc>1)) stop("proportions must be between 0 and 1")
  if(dreieck) {
    ecken <- matrix(c(0, 1, 0.5, 0, 0, sqrt(3)/2), ncol = 2)
    plot(rbind(ecken, c(0, 0)), type = "l", xlim = c(0, 1), ylim =
         c(0, 1), axes = FALSE, xlab = "", ylab = "")
    text(ecken, text.ecken)
  }
  text((1 - pa - pc/2), ((pc * sqrt(3))/2), label, col = 3)
}


p.two.forget <- function(data, anova, label = FALSE)
{
  ## Zweck: forget-it-plot   Autor: Stahel  Datum: Dez 89
  fit <- data - anova$resid
  rw <- anova$row
  cl <- anova$col
  x <-  - outer(rw, cl, "-")
  rg <- range(c(fit, data))
  ht <- 0.05 * diff(rg)
  plot(range(x), rg - c(ht, 0), type = "n", xlab = "", ylab = "Y")
  mtext("forget-it-plot", 3, 1)
  segments(x, fit, x, data)
  points(x, data)
  dg <-  - min(0, floor(log10(max(abs(rg)))) - 3)
  gd <- anova$grand
  mnrw <- min(rw)
  mxrw <- max(rw)
  segments(cl - mnrw, gd + mnrw + cl, cl - mxrw, gd + mxrw + cl, lty = 2)

  mncl <- min(cl)
  mxcl <- max(cl)
  segments(mncl - rw, gd + mncl + rw, mxcl - rw, gd + mxcl + rw, lty = 2)

  if(label)
    text(c(cl - mnrw, mncl - rw), c(mnrw + cl, mncl + rw) + gd -
         ht, round(c(cl, rw), dg))
  "forget-it"
}

p.two.res <- function(anova, col = FALSE)
{
  ## Two.res  Zweck: Residuen einer twoway-Analyse auf spezielle Art zeichnen
  ##          Autor: w. Stahel  Datum: Dez. 89
  rs <- anova$resid
  ij <- list(rank(anova$row), rank(anova$col))
  ind <- if(col) c(2, 1) else 1:2
  dm <- dim(rs)[ind]
  ij <- list(ij[[ind[1]]], ij[[ind[2]]])
  x <- outer(ij[[1]], (0.6/dm[2]) * (ij[[2]] - 0.5) - 0.3, "+")
  if(ind[1]!=1)
    x <- t(x)
  tx <- c("row", "col")[ind]
  plot(x, rs, xlab = "", ylab = "Residuals", axes = FALSE)
  box()
  abline(h = 0, lty = 2)
  axis(2)
  axis(1, at = 1:dm[1], label = as.character(ij[[1]]))
  abline(v = seq(1.5, dm[1], 1), lty = 2)
  mtext(tx[1], 1, 2)
  mtext(paste(tx[2], ": ", paste(ij[[2]], collapse = ", ")), 1, 3.5)
}


p.tst.dev <- function(ltypes = 10, lwidths = 12, colors = 16, ptypes = 20)
{
  ## Purpose: Show Lines, Points and Colors for the current device
  ## -------------------------------------------------------------------------
  ## Arguments: ltypes, lwidths, colors :=  NUMBER of  lty, lwd, col
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 1990

  if(is.R()) {
    cat("using current par() settings ...\n")
  } else { ## S-plus
    Device.Default()
  }
  ##--- Define the region --------------------
  plot(c(0, 100), c(0, 100), type = "n", xlab = "", ylab = "Y label",
       main = paste(if(is.R()) "R" else "S+",
         " device - test :  .Device  == \"",  .Device, "\""),
       sub = " sub-title ")
  mtext(if(is.R())R.version.string else version, side=1, adj=1, cex=0.6)
  ##--- Line types: -------------------------
  for(i in 1:ltypes) {
    y <- ((i - 1/2) * 100)/ltypes
    lines(c(1, 20), c(y, y), lty = i)
    text(28, y, paste("lty=", i))
  }
  ##--- Line colors: -------------------------
  for(i in 1:colors) {
    y <- ((i - 1/2) * 100)/colors
    lines(34 + c(1, 20), c(y, y), col = i)
    text(34 + 28, y, paste("col=", i))
  }
  ##--- Line widths: -------------------------
  for(i in 1:lwidths) {
    y <- ((i - 1/2) * 100)/lwidths
    lines(2 * 34 + c(1, 20), c(y, y), lwd = i/2)
    text(2 * 34 + 28, y, paste("lwd=", i/2))
  }
}

p.profileTraces <-
    function(x, cex=1, subtitle="t-Profil-Plot und Profilspuren")
{
  ## Purpose: Zeichnet die Profilspuren und die t-Profil-Plots.
  ## Arguments: x = Resultat von der R/S-Funktion profile()
  ## Author: Andreas Ruckstuhl, Date: Nov 93
  ##         R port by Isabelle Flückiger and Marcel Wolbers
  ## -------------------------------------------------------------------------
  nx <- names(x)
  np <- length(x)
  opar <- par(oma = c(2, 2, 1.5, 0), mfrow = c(np, np),
              mar = c(2,4, 0, 0) + 0.2)
  on.exit(par(opar))
  for (i in 1:np) {
    for (j in 1:i) {
      if (i == j) { ## Diagonale : Profil t-Funktionen
        if (!is.null(this.comp <- x[[i]])) {
          xx <- this.comp$par[, nx[i]]
          tau <- this.comp[[1]]
          plot(spline(xx, tau), xlab = "", ylab = "",
               type = "l", las = 1, mgp = c(3, 0.8, 0),
               cex = 0.5 * cex)
          points(xx[tau == 0], 0, pch = 3)
          pusr <- par("usr")
          ## "at = " muss anders sein R & SPlus
          if(is.R()) { ## mtext(outer = TRUE, at= <NICHT "usr" Koord>):
            mtext(side = 1, line = 0.8, at = -1/(2*np)+i/np,
                  text = nx[j] , outer = TRUE, cex = cex)
            mtext(side = 2, line = 0.8, at = 1+1/(2*np)-i/np,
                  text = nx[i], outer = TRUE, cex = cex)
          }
          else {
            mtext(side = 1, line = 0.8, at = mean(pusr[1:2]),
                  text = nx[j] , outer = TRUE, cex = cex)
            mtext(side = 2, line = 0.8, at = mean(pusr[3:4]),
                  text = nx[i], outer = TRUE, cex = cex)
          }
        }
      }
      else { ## j < i : Likelihood Profilspuren
        if ((!is.null(x.comp <- x[[j]])) & (!is.null(y.comp <- x[[i]]))) {
          xx <- x.comp$par[, nx[j]]
          xy <- x.comp$par[, nx[i]]
          yx <- y.comp$par[, nx[j]]
          yy <- y.comp$par[, nx[i]]
          plot(xx, xy, xlab = "", ylab = "", las = 1,
               mgp = c(3, 0.8, 0), type = "n",
               xlim = range(c(xx, yx)),
               ylim = range(c(xy, yy)), cex = 0.5 * cex)
          lines(xx, xy, col = 2)
          lines(yx, yy, col = 3)
        }
      }
    }
    if (i < np) # frame()s:  S-plus braucht häufig eines mehr :
      for (k in 1:(np - i + if(is.R()) 0 else 1)) frame()
  }
  mtext(side = 3, line = 0.2, text = subtitle,
        outer = TRUE, cex = 1.2 * cex)
}

## Test Beispiel :

## --> /u/sfs/ueb/fortgeschrittene/loesungen/loes-rg.truthennen.R

p.hboxp <- function(x, y.lo, y.hi, boxcol = 3, medcol = 0,
                    medlwd = 5, whisklty = 2, staplelty = 1)
{
  ## Purpose: Add a HORIZONTAL boxplot to the current plot
  ## -------------------------------------------------------------------------
  ## Arguments: x: univariate data set
  ##            y.lo, y.hi:  min. and max. user-coordinates  OR y.lo=c(ylo,hyi)
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 12 Jan 96, 16:45
  ##       using code from the original 'hist.bxp' by Markus & Christian Keller
  ##
  ##-- Example: See code in  'hist.bxp' (.)
  ##
  if(missing(y.hi) && length(y.lo) == 2) { y.hi <- y.lo[2]; y.lo <- y.lo[1] }
  ## should test y.lo < y.hi, both to be numbers...

  ##--- 2nd set of Defaults  (by setting the args to NA) :
  if(is.na(medcol)) medcol <- par("col")
  if(is.na(medlwd)) medlwd <- par("lwd")
  if(is.na(whisklty))  whisklty  <- par("lty")
  if(is.na(staplelty)) staplelty <- par("lty") #

  b <- boxplot(x, plot = FALSE)
  st <- c(b$stats)## names(st) <- c("max","Q3","med","Q1","min")

  ##-------- drawing the boxplot --------------
  ## coordinates :

  m <- (y.hi + y.lo)/2
  llhh <- c(y.lo, y.lo, y.hi, y.hi)
  ## drawing the box
  polygon(c(st[4], st[2], st[2], st[4]), llhh,
          col = ifelse(boxcol == 0, par("col"), boxcol), lty = 1,
          density = ifelse(boxcol == 0, 0, -1)) #
  ## Median
  lines(rep(st[3], 2), c(y.lo, y.hi),
        col = ifelse(boxcol == 0 && missing(medcol), par("col"), medcol),
        lwd = medlwd, lty = 1) #
  ## Border of the box
  lines(c(st[4], st[2], st[2], st[4]), llhh,
        col = ifelse(boxcol == 0, par("col"), boxcol), lty = 1) #
  ## Whiskers
  lines(c(st[1:2], NA, st[4:5]), rep(m, 5), lty = whisklty) #
  ## Staples
  k <- .01 * diff(range(x))
  lines(st[1]+ c(-k, 0, 0, -k), llhh, lty = staplelty)
  lines(st[5]+ c( k, 0, 0,  k), llhh, lty = staplelty)#
  ## Outliers
  for(out in b$out)
    lines(rep(out, 2), c(y.lo, y.hi), lty = staplelty)
}



p.arrows <- function(x1, y1, x2, y2,
                     size=1, width = (sqrt(5)-1)/4/cin, fill = 2, ...)
{
  ## Purpose: Nicer arrows(): FILLED arrow heads
  ## -------------------------------------------------------------------------
  ## Arguments: size:  symbol size as a fraction of a character height
  ##            width: width of the Arrow Head
  ##            ...:   further arguments for the segment routine
  ## Author: Andreas Ruckstuhl, Date: 19 May 94;   Cosmetic by MM: June'98
  ## -------------------------------------------------------------------------
  cin <- size*par("cin")[2] ## vertical symbol size in inches
  uin <-  if(is.R()) 1/xyinch() else par("uin") ## inches per usr unit

  segments(x1, y1, x2, y2, ...)

  ## Create coordinate of a polygon for a ``unit arrow head'':
  x <- sqrt(seq(0, cin^2, length=floor(35*cin)+2))
  delta <- 0.005/2.54 # ? 2.54cm = 1 in
  x.arr <- c(-x, -rev(x))
  wx2 <- width* x^2
  y.arr <- c(- wx2 - delta, rev(wx2) + delta)
  ## Polar(x.., y..):
  deg.arr <- c(atan(y.arr, x.arr), NA)# - NA to 'break' long polygon
  r.arr <- c(sqrt(x.arr^2 + y.arr^2), NA)

  ## Draw Arrow Head at (x2,y2)
  theta <- atan((y2-y1)*uin[2], (x2-x1)*uin[1])
  lx <- length(x1)
  Rep <- rep(length(deg.arr), lx)
  x2 <- rep(x2, Rep)
  y2 <- rep(y2, Rep)
  theta <- rep(theta, Rep) + rep(deg.arr, lx)
  r.arr <- rep(r.arr, lx)
  polygon(x2+ r.arr*cos(theta)/uin[1],
          y2+ r.arr*sin(theta)/uin[2], col= fill)
}
