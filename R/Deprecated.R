###-- Synchronize with ../man/Deprecated.Rd !!


p.pllines <- function(x,y,group,lty=c(1,3,2,4),...)
{
  ## Purpose:   lines according to group
  ## -------------------------------------------------------------------------
  ## Arguments:
  ## -------------------------------------------------------------------------
  ## Author: Werner Stahel, Date: 21 Jun 93, 15:45

  warning("p.pllines() is deprecated: in R, use",
          "plot(x,y, lty=group, type='l', ...)")

  plot(x,y,type="n",...)
  ngr <- max(group)
  for (gg in 1:ngr) {
    ii <- group==gg & !is.na(x) & !is.na(y)
    if(sum(ii)) lines(x[ii],y[ii],lty=lty[1+(gg-1)%%length(lty)])
  }
}


list2mat <- function(x, check = TRUE)
{
  ## Purpose:  list -> matrix
  ## -------------------------------------------------------------------------
  ## Arguments: x a list whose first 2 el.  MUST be equal length vectors
  ##		check: if T, check if lengths are ok.   F: "quick & dirty"
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 19 May 93, 09:46

    warning("list2mat(x) is deprecated -- use  sapply(x, c)  instead!")

  if(!is.list(x)) stop("Argument must be list !")
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
  collabs <- names(x)
  if(any(nuet <- "" == collabs))
      collabs[nuet] <- paste("L", which(nuet), sep = ".")
  matrix(unlist(x), n,p, dimnames = list(NULL, collabs))
}


###--  plotCI() : -----------------------------------------------------------

##- From: ben@zoo.ufl.edu
##- To: Mike Beddo <meb@dataventures.com>
##- cc: R Help <r-help@stat.math.ethz.ch>
##- Subject: Re: [R] How to plot error bars
##- Date: Wed, 8 Nov 2000 14:07:34 -0500 (EST)


##-   I'm going to take the liberty of reposting this function, which is based
##- on one that Bill Venables posted a while back.  I've tweaked with it a bit
##- to add functionality.  It will do horizontal bars or vertical bars, but
##- not (yet) both simultaneously (the hardest thing about that is deciding on
##- what format you want the data supplied in).
##-
##-   There's also a help file supplied below.
##-
##-   Should this (after appropriate tweaking/polishing/testing/revision) go
##- into the main R code base?  It seems like a pretty basic function to me
##- ...

plotCI <-
    function (x, y = NULL, uiw, liw = uiw, aui = NULL, ali = aui,
              err = "y", xlim = NULL, ylim = NULL, type = "p", log = "",
              sfrac = 0.01, gap = 0, add = FALSE,
              col = par("col"), lwd = par("lwd"), slty = par("lty"),
              scol = col, pt.bg = NA,
              xlab = NULL, ylab = NULL, main = "", axes = TRUE, ...)
{

    cat("plotCI() in the 'sfsmisc' package is deprecated (and will be removed)",
	"\n do use plotCI() from the 'gplots' package instead!\n")

    ## from Bill Venables, R-list
    if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (is.null(y)) {
        if (is.null(x))
            stop("both x and y NULL")
        y <- as.numeric(x)
        x <- seq(along = x)
    }
    if (missing(xlab)) xlab <- deparse(substitute(x))
    if (missing(ylab)) ylab <- deparse(substitute(y))
    if (missing(uiw)) { ## absolute limits
        ui <- aui
        li <- ali
    }
    else { ## relative limits
        if (err == "y") z <- y else z <- x
        ui <- z + uiw
        li <- z - liw
    }
    if (err == "y" & is.null(ylim))
        ylim <- range(c(y, ui, li), na.rm = TRUE)
    else
        if (err == "x" & is.null(xlim))
            xlim <- range(c(x, ui, li), na.rm = TRUE)
    if (!add)
        plot(x, y, log = log, ylim = ylim, xlim = xlim, col = col, lwd = lwd,
             xlab = xlab, ylab = ylab, main = main, type = "n", axes = axes, ...)
    if (gap == TRUE) gap <- 0.01 ## default gap size
    ul <- c(li, ui)
    if (err == "y") {
        ## draw vertical segments
        gap <- rep(gap,length(x))*diff(par("usr")[3:4])
        segments(x , li, x, pmax(y-gap,li), col = scol, lwd = lwd, lty = slty)
        segments(x , ui, x, pmin(y+gap,ui), col = scol, lwd = lwd, lty = slty)
        ## horizontal segments
        x2 <- c(x, x)
        smidge <- diff(par("usr")[1:2]) * sfrac
        segments(x2 - smidge, ul, x2 + smidge, ul, col = scol, lwd = lwd)
    }
    else if (err == "x") {
        ## draw horizontal segments
        gap <- rep(gap,length(x))*diff(par("usr")[1:2])
        segments(li, y, pmax(x-gap,li), y, col = scol, lwd = lwd, lty = slty)
        segments(ui, y, pmin(x+gap,ui), y, col = scol, lwd = lwd, lty = slty)
        ## vertical segments
        y2 <- c(y, y)
        smidge <- diff(par("usr")[3:4]) * sfrac
        segments(ul, y2 - smidge, ul, y2 + smidge, col = scol, lwd = lwd)
    }
    ## _now_ draw the points (in case we want to have "bg" set for points)
    points(x, y, col = col, lwd = lwd, bg = pt.bg, type = type, ...)
    invisible(list(x = x, y = y))
}

##  A modified "safe" (and more general) Huber estimator:
huberM <- function (x, k = 1.5, tol = 1e-06,
                    mu = median(x), s = mad(x, center=mu),
                    warn0scale = getOption("verbose"))
{
    ## Purpose: a "safe" version of MASS::huber()
    ## -----------------------------------------------
    ## --->  ?huberM
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 6 Jan 2003

 .Deprecated("huberM() in package 'robustbase'")

    x <- x[!is.na(x)]
    n <- length(x)
    it <- 0:0
    if(n == 0) # e.g `x' was all NA
	return(list(mu = NA, s = NA, it = it)) # instead of error
    if (s <= 0) {
        if(s < 0) stop("negative scale `s'")
        if(warn0scale && n > 1)
            warning("scale `s' is zero -- returning initial `mu'")
    }
    else
	repeat {
	    it <- it + 1:1
	    mu1 <- sum(pmin(pmax(mu - k * s, x), mu + k * s)) / n
	    if (abs(mu - mu1) < tol * s)
		break
	    mu <- mu1
	}
    list(mu = mu, s = s, it = it)
}

## keep a stub here [as from 2006-10-19; version 0.95-7] :
rnls <- function(...)
{
    ## Purpose:
    ##  Robust parameters estimation in the nonlinear model. The fitting is
    ##  done by iterated reweighted least squares (IWLS) as in rlm() of the
    ##  package MASS. In addition, see also 'nls'.

    stop("rnls() in package 'sfsmisc' is defunct.",
	"\n Do use nlrob() from the 'robustbase' package instead!\n")
}
