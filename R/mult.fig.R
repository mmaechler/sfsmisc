nrpl.2.mfrow <- function(nr.plots)
{
  ## Purpose: 'NumbeR' of PLots   'to' 'mfrow'
  ##          Give goods 'default mfrow' for given number of plots
  ## -------------------------------------------------------------------------
  ## Arguments: nr.plots : integer
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1990 (UW, Seattle); 1996

  if      (nr.plots <=  3)  c(nr.plots,1) #-- 1, 2, 3
  else if (nr.plots <=  6)  c((nr.plots+1)%/%2,2)  #-- nr.plots = 4,5,6
  else if (nr.plots <= 12)  c((nr.plots+2)%/%3,3)
  else c(nrow <- ceiling(sqrt(nr.plots)),
         ceiling( nr.plots / nrow))
}

###-- This version from /u/maechler/R/MM-Goodies/mult.fig.R :
mult.fig <-
function(nr.plots, mfrow, mfcol, marP = rep(0,4), mgp = c(1.5,.6,0),
         mar = marP + .1 + c(4,4,2,1),
         main = NULL,
         tit.wid = if (is.null(main)) 0 else 1 + 1.5*cex.main,
         quiet = .Device == "postscript",
         cex.main = par("cex.main"),
         col.main = par("col.main"),
         font.main = par("font.main"),
         ...)
{
  ## Purpose: 'MULTiple FIGures' incl. TITLE and other good defaults
  ## -------------------------------------------------------------------------
  ## Arguments: 
  ##             -- Either ONE of the first 3 arguments --
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1990 (UW, Seattle) -- 1995
  ## -------------------------------------------------------------------------
  ## >>> calls  nrpl.2.mfrow(.) !
  use.row <- missing(mfcol)
  if (use.row)
    if (missing(mfrow)) {
      if (missing(nr.plots))
        stop("must either specify 'nr.plots', 'mfrow' or 'mfcol' !")
      else  mfrow <- nrpl.2.mfrow (nr.plots)
    }
##-- This was for R versions <= 0.90 :
##-  Nrow <- (if(use.row) mfrow else mfcol)[1]
##-   if(Nrow > 2) { #-- Then R uses a much smaller character size --
##-     if(!is.null(main)) cex.main <- cex.main * 1.5
##-     tit.wid <- tit.wid * 1.5
##-   }
  oma <- c(0, 0, tit.wid, 0)
  old.par <<-
    if(use.row) par(mfrow = mfrow, oma= oma, mar = mar, mgp= mgp)
    else        par(mfcol = mfcol, oma= oma, mar = mar, mgp= mgp)
  if(!quiet) cat("Execute\n\t par(old.par) \n later to restore graphical par\n")
  ##---- now go ahead :
  ## nomore in R : frame()
  if (!is.null(main)) {# Do title *before* first plot!
      plot.new()
      mtext(main, side = 3, outer = TRUE,
            line = cex.main, # was tit.wid - 4,
            cex = cex.main,
            font = font.main, col = col.main, ...)
      par(new=TRUE)# reverse `plot.new()' above
  }
  invisible(list(new.par = par(c("mfrow","mfcol","oma","mar","mgp")),
                 old.par = old.par))
}
