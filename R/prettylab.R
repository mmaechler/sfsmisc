### --> these are from ~/R/MM/GRAPHICS/axis-prettylab.R

### Help files: ../man/pretty10exp.Rd  ../man/axTexpr.Rd
###                    --------------         ----------

pretty10exp <- function(x, drop.1 = FALSE)
{
    ## Purpose: produce "a 10^k"  label expressions instead of "a e<k>"
    ## ----------------------------------------------------------------------
    ## Arguments: x: numeric vector (e.g. axis tick locations)
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 7 May 2004; 24 Jan 2006
    eT <- floor(log10(abs(x))) # x == 0 case is dealt with below
    mT <- x / 10^eT
    ss <- vector("list", length(x))
    for(i in seq(along = x))
        ss[[i]] <-
            if(x[i] == 0) quote(0)
            else if(drop.1 && mT[i] ==  1) substitute( 10^E, list(E = eT[i]))
            else if(drop.1 && mT[i] == -1) substitute(-10^E, list(E = eT[i]))
            else substitute(A %*% 10^E, list(A = mT[i], E = eT[i]))
    do.call("expression", ss)
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
### Myaxis(.)  function with at least two options ("engineering/not")
### Really wanted: allow   xaxt = "p" (pretty) or "P" (pretty, "Engineer")

eaxis <- function(side, at = axTicks(side, log=log), labels = NULL, log = NULL,
                  f.smalltcl = 3/5, at.small = NULL, small.mult = NULL,
                  outer.at = TRUE, drop.1 = TRUE)
{
    ## Purpose: "E"xtended, "E"ngineer-like (log-)axis
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 13 Oct 2007
    is.x <- side%%2 == 1
    if(is.null(log)) {
        XY <- function(ch) paste(if (is.x) "x" else "y", ch, sep = "")
        log <- par(XY("log"))
    }
    ## use expression (i.e. plotmath) if 'log' or exponential format:
    use.expr <- log || format.info(as.numeric(at), digits=7)[3] > 0
    if(is.null(labels))
	labels <- if(use.expr) pretty10exp(at, drop.1=drop.1) else TRUE
    else if(is.na(labels)) # no 'plotmath'
	labels <- TRUE
    axis(side, at = at, labels = labels)
    if(is.null(at.small)) { ## create smart default, using small.mult
        at.small <-
            if(log) {
                if(is.null(small.mult)) small.mult <- 9
                at. <- at[log10(at) %% 1 < 1e-3] ##  the 10^k ones:
                if(length(at.))
                    outer(2:small.mult, c(if(outer.at) at.[1]/10, at.))
            } else {
                ## assumes that 'at' is equidistant
                d <- diff(at <- sort(at))
                if(any(abs(diff(d)) > 1e-3 * (dd <- mean(d))))
                    stop("'at' is not equidistant")
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
                outer(1:(small.mult-1)/small.mult * dd,
                      c(if(outer.at) at[1]-dd, at), "+")
            }
        ##
        if(outer.at) { # make sure 'at.small' remain inside "usr"
            p.u <- sort(par("usr")[if(is.x) 1:2 else 3:4])
            if(log) p.u <- 10^p.u
            at.small <- at.small[p.u[1] <= at.small & at.small <= p.u[2]]
        }
    }
    axis(side, at = at.small, label = FALSE,
         tcl = f.smalltcl * par("tcl"))
}
