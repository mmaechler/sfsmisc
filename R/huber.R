#### NOTA BENE:  New version of huberM() is only in  'robustbase' !!!
#### --------                   ----------------------------------------

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

## this is a compatible improvement of MASS' huber() :
## 1) returning median() if mad()=0
## 2)	"	NA when y has only NAs (or length 0)

if(FALSE)
huber <- function (y, k = 1.5, tol = 1e-06)
{
    y <- y[!is.na(y)]
    n <- length(y)
    if(n == 0) # e.g `y' was all na
	return(list(mu = NA, s = NA))# instead of error
    mu <- median(y)
    s <- mad(y)
    if (s == 0) { # FIXME?  make this warning optional
	if(n > 1) warning("scale MAD is zero for this sample")
    }
    else repeat {
	yy <- pmin(pmax(mu - k * s, y), mu + k * s)
	mu1 <- sum(yy)/n
	if (abs(mu - mu1) < tol * s)
	    break
	mu <- mu1
    }
    list(mu = mu, s = s)
}
