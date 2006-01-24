### --> these are from ~/R/MM/GRAPHICS/axis-prettylab.R

### Help files: ../man/pretty10exp.Rd  ../man/axTexpr.Rd
###                    --------------         ----------
### TODO:
###
### 2) Myaxis(.)  function with at least two options ("engineering/not")
### Really wanted: allow   xaxt = "p" (pretty) or "P" (pretty, "Engineer")

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
            else if(drop.1 && mT[i] == 1) substitute(10^E, list(E = eT[i]))
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

