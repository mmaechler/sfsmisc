#### $Id: p.goodies.R,v 1.14 2004/01/12 15:41:13 maechler Exp $
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
### p.two.forget
### p.two.res

### p.profileTraces     Profil-Spuren fuer Nichtlineare Regression
### p.hboxp		Horizontale Boxplots
### p.arrows		Nicer arrows(): FILLED arrow heads
###
### ==========================================================================

p.datum <- function(outer = FALSE, cex = 0.75, ...)
    mtext(u.Datumvonheute(...), 4, cex = cex, adj = 0, outer = outer, las = 0)


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
  names(usr2cm) <- c("x", "y")
  cbind(sy2usr = unit/usr2cm,
        usr2cm = usr2cm)
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

p.profileTraces <-
    function(x, cex=1, subtitle="t-Profil-Plot und Profilspuren")
{
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

## mainly auxiliary of  hist.bxp() :
p.hboxp <- function(x, y.lo, y.hi, boxcol = 3, medcol = 0,
                    medlwd = 5, whisklty = 2, staplelty = 1)
{
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
