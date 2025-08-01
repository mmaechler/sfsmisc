#### Pretty Labels for "plotmath" axes -- Main function:  eaxis()

### --> these are from ~/R/MM/GRAPHICS/axis-prettylab.R

### Help files: ../man/pretty10exp.Rd  ../man/axTexpr.Rd   ../man/eaxis.Rd
###                    --------------         ----------          --------

pretty10exp <- function(x, drop.1 = FALSE, sub10 = FALSE,
                        digits = 7, digits.fuzz,
                        ## default `off` corrects for subnormal |x| {on x86_64 ..}:;
                        off = pmax(10^-digits, 2^-(l10x*log2(10)+1075)),
                        lab.type = c("plotmath", "latex"),
                        lab.sep = c("cdot", "times"))
{
    ## Purpose: produce "a 10^k"  label expressions instead of "a e<k>"
    ## ----------------------------------------------------------------------
    ## Arguments: x: numeric vector (e.g. axis tick locations)
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 7 May 2004, Jan 2006, ...

    if(!missing(digits.fuzz)) {
	message("'digits.fuzz' is deprecated; use 'digits' instead")
	digits <- digits.fuzz
    }
    lab.type <- match.arg(lab.type)
    lab.sep  <- match.arg(lab.sep)

    l10x <- log10(abs(x))
    eT <- floor(l10x + off) # x == 0 case is dealt with below
    ##    ^^^^^ round() would be better, e.g., when  x <- 10^-(323:300)  <--> "subnormal" rounding
    mT <- signif(x / 10^eT, digits) # m[antissa]
    ss <- vector("list", length(x))
    if(sub.10 <- !isFALSE(sub10)) {
	sub10 <- if(isTRUE(sub10))
                     c(0L,0L)
                 else if(identical(sub10,  "10")) c(-1L,1L)
                 else if(identical(sub10, "100")) c(-1L,2L)
                 else if(identical(sub10,"1000")) c(-2L,3L)
                 else
                     as.integer(sub10)
	noE <-
	    if(length(sub10) == 1) {                               ### FIXME
                ## *Not* really sensible -- was active till 2025-08... [sfsmisc version <= 1.1-21]
		## if(sub10 < 0)
		##     stop("'sub10' must not be negative if a single number")
		eT <= sub10
	    } else if(length(sub10) == 2) {
		stopifnot(sub10[1] <= sub10[2])
		sub10[1] <= eT & eT <= sub10[2]
	    } else stop("invalid 'sub10'")
	## for noE's, mt := value (instead of mantissa):
	mT[noE] <- mT[noE] * 10^eT[noE]
    }
    if (lab.type == "plotmath") {
	for(i in seq_along(x))
	    ss[[i]] <-
		if(x[i] == 0) quote(0)
		else if(sub.10 &&  noE[i]    ) substitute( A, list(A = mT[i]))
		else if(drop.1 && mT[i] ==  1) substitute( 10^E, list(E = eT[i]))
		else if(drop.1 && mT[i] == -1) substitute(-10^E, list(E = eT[i]))
		else substitute(A %*% 10^E, list(A = mT[i], E = eT[i]))
	do.call("expression", ss)
    } else { ## lab.type=="latex"
	## TO DO: allow format specifier??
	mTf <- formatC(mT, width=1)
	eTf <- formatC(eT, width=1)
	for(i in seq_along(x))
	    ss[[i]] <-
		if(x[i] == 0) ""
		else if(sub.10 &&  noE[i]    ) mTf[i]
		else if(drop.1 && mT[i] ==  1) sprintf("$10^{%s}$", eTf[i])
		else if(drop.1 && mT[i] == -1) sprintf("$-10^{%s}$",eTf[i])
		else sprintf("$%s \\%s 10^{%s}$", mTf[i], lab.sep,  eTf[i])
	unlist(ss)
    }
}

axTexpr <- function(side, at = axTicks(side, axp=axp, usr=usr, log=log),
                    axp = NULL, usr = NULL, log = NULL, drop.1 = FALSE)
{
    ## Purpose: Do "a 10^k" labeling instead of "a e<k>"
    ## -------------------------------------------------
    ## Arguments: as for axTicks()
    pretty10exp(at, drop.1)
}

### TODO:
###
### My axis(.)  function with at least two options ("engineering/not")
### Really wanted: allow   xaxt = "p" (pretty) or "P" (pretty, "Engineer")
### FIXME(2):  max.at is only needed because  axTicks() is sometimes too large
### FIXME(3): ??  axisTicks() instead of axTicks():
## set.seed(1);x <- runif(100,-0.18, 1.13)
## par(mar=.1+c(5,4,2,4)); plot(x,axes=FALSE)
##  eaxis(4) # ugly
##  eaxis(2, at=axisTicks(par("usr")[3:4],log=FALSE)) # much better
eaxis <- function(side, at = if(log) axTicks(side, axp=axp, log=log, nintLog=nintLog)
                             else    axTicks(side, axp=axp, log=log),
                  labels = NULL, log = NULL,
                  ## use expression (plotmath/latex) if 'log' or exponential format:
                  use.expr = log || format.info(as.numeric(at), digits=7)[3] > 0,
                  f.smalltcl = 3/5, at.small = NULL, small.mult = NULL, equidist.at.tol = 0.002,
                  small.args = list(),
                  draw.between.ticks = TRUE, between.max = 4,
                  outer.at = TRUE, drop.1 = TRUE, sub10 = FALSE, las = 1,
                  nintLog = max(12L, par("lab")[2L - is.x]), axp = NULL, n.axp = NULL,
                  max.at = Inf, lab.type="plotmath", lab.sep="cdot", ...)
{
    ## Purpose: "E"xtended, "E"ngineer-like (log-)axis
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 13 Oct 2007

    ## first part: same as graphics::axTicks() [also by MM] :
    is.x <- side%%2 == 1
    XY <- function(ch) paste0(if (is.x) "x" else "y", ch)
    if(is.null(log)) log <- par(XY("log"))
    if(is.null(axp)) {
	axp <- par(XY("axp"))
	if(!is.null(n.axp)) {
	    if(is.numeric(n.axp) && length(n.axp) == 1 && n.axp == as.integer(n.axp))
		axp[3] <- n.axp
	    else stop(gettextf(
		     "'n.axp' must be an integer to be used as '%s', see ?par and there 'xaxp'",
		     XY("axp")), domain=NA)
	}
    }
    if(is.finite(max.at <- round(max.at))) { ## "thin the 'at' values
	if(max.at < 1) stop("'max.at' must be >= 1")
	at <- quantile(at, (0:max.at)/max.at, names = FALSE,
		       type = 3) ## <-- ensure that order statistics are used
	if(!log && is.null(at.small) && { d <- diff(at)
            any(abs(diff(d)) > equidist.at.tol * mean(d)) }) # at is not equidistant :
	    at.small <- FALSE
    }
    if(is.null(labels))
	labels <- if(use.expr) {
                      pretty10exp(at, drop.1=drop.1, sub10=sub10,
                                  lab.type=lab.type, lab.sep=lab.sep)
                  } else if(lab.type == "latex")
                      paste("$", at, "$", sep="")
                  else TRUE
    else if(length(labels) == 1 && is.na(labels)) # no 'plotmath'
	labels <- TRUE
    axis(side, at = at, labels = labels, las=las, ...)
    if(log) {
	if(any(at <= 0)) stop("invalid 'log=TRUE' for at <= 0: not a true log scale plot?")
	l1 <- (lat <- log10(at)) %% 1 ##  the 10^k ones
	l.int <- l1 < 1e-5 | l1 > 1 - 1e-5
	if(draw.between.ticks && all(l.int)) { ## all lat are integer
	    ## check if have "thinned" but still want to draw ticks
	    if(any(diff(lat <- sort(round(lat, 5))) > 1)) {
		nl <- length(lat0 <- lat)
		## extend 'at' (new must contain the previous!)
		lat <- lat[1]:lat[nl]
		if(length(lat) > between.max*nl) { ## too many: thin them!
		    lat <- unique(round(seqXtend(lat0, between.max*nl,
						 "interpolate")))
		    if(is.null(at.small) && median(diff(lat)) > 1.5)
			## no small ticks, if large are mostly not 10^(k1..k2)
			at.small <- FALSE
		}
		at <- 10^lat
		axis(side, at = at, labels = FALSE, las=las, ...)
	    }
	}
    }
    if(is.null(at.small)) { ## create smart default, using small.mult
	at.small <-
	    if(log) {
		if(!all(l.int)) at <- at[l.int]
		if(is.null(small.mult)) small.mult <- 9
		if(length(at))
		    outer(2:small.mult, c(if(outer.at) at[1]/10, at))
	    } else {
                ## assumes that 'at' is equidistant
                d <- diff(at <- sort(at))
                if(any(abs(diff(d)) > equidist.at.tol * (dd <- mean(d))))
                    FALSE ## 'at' is not equidistant"
                else {
                    if(is.null(small.mult)) {
                        ## look at 'dd' , e.g. in {5, 50, 0.05, 0.02 ..}
                        d. <- dd / 10^floor(log10(dd))
                        small.mult <- {
                            if(d. %% 5 == 0) 5
                            else if(d. %% 4 == 0) 4
                            else if(d. %% 2 == 0) 2
                            else if(d. %% 3 == 0) 3
                            else if(d. %% 0.5 == 0) 5
                            else 2 }
                    }
                    outer(seq_len(small.mult-1)/small.mult * dd,
                          c(if(outer.at) at[1]-dd, at), "+")
                }
            }
        ##
        if(outer.at) { # make sure 'at.small' remain inside "usr"
            p.u <- sort(par("usr")[if(is.x) 1:2 else 3:4])
            if(log) p.u <- 10^p.u
            at.small <- at.small[p.u[1] <= at.small & at.small <= p.u[2]]
        }
    }
    if(is.numeric(at.small) && any(is.finite(at.small))) ## can use  NA or FALSE to suppress
	## axis(side, at = at.small, .....)
	do.call(axis, c(list(side, at = at.small, labels = FALSE,
			     tcl = f.smalltcl * par("tcl")),
			small.args))
}



## @author Alain Hauser <alain@huschhus.ch>
## @date 2014-02-12 originally

toLatex.numeric <- function(object,
                            digits = format.info(object)[2],
                            scientific = format.info(object)[3] > 0,
                            times = "\\cdot", ...)
{
    sround <- function(x, digits) sprintf("%0.*f", digits, x)
    if(scientific) {
        ## Strings in scientific format -- isn't regex a funny thing? ;-)
        # res <- as.character(pretty10exp(object, digits = digits + 1))
        # res <- sub("%\\*%", gsub("\\\\", "\\\\\\\\", times), res)
        # sub("10\\^(.*)$", "10^{\\1}", res)
        ## Original version without pretty10exp and regex:
        eT <- floor(log10(abs(object)) + 10^(-digits-1))
        sprintf("%s %s 10^{%d}",
               sround(object/10^eT, digits), times, eT)
    } else {
        ## Actual output strings
        sround(object, digits)
    }
}
