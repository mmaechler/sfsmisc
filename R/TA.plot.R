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
  ## Uses : n.plot(.)
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: Dec 92 / Nov.93;  for R: 1999/2000
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
