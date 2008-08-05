
str_data <- function(pkgs, ...)
{
    ## Purpose: str(.) of all datasets in a package
    ## ----------------------------------------------------------------------
    ## Arguments: pkgs : character vector of names of R packages
    ##		  ... : potential further arguments to be passed to str()
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 17 Jun 2005, 09:04
    stopifnot(is.character(pkgs))
    for(pkg in pkgs) {
	cat("\nAll data sets in R package '",pkg,"' :",
	    "\n--------------------------","  ", rep("=", nchar(pkg)),
	    "\n\n", sep='')
	dd <- data(package = pkg)
	items <- dd$results[,"Item"]
	## not those that are part of "another" (multi-object) one:
        if(length(i <- grep(".*\\(.*\\)$", items)) > 0)
            items <- items[- i]
	##
	## Gabor's wishes (2005-03-25):
        ##    1) allow filtering on class(),
	##    2) sorting according to size -- that needs 2 passes through...
	dat.env <- new.env()
	for(n in items) {
	    data(list = n, package = pkg, envir = dat.env)
	    nms <- ls(envir = dat.env, all.names=TRUE)
	    cat(n, ": ")
	    if(length(nms) == 1) { ## one data set == normal case
		if(nms != n) cat(nms, ": ")
		str(get(nms, envir = dat.env), ...)
	    }
	    else { ## more than one data set
		cat("\n")
		for(nn in nms) {
		    cat(" ", nn, ": ")
		    str(get(nn, envir = dat.env),
			indent.str = paste(" ", ''), ...)
		}
	    }
	    cat("--------------\n")
	    rm(list = nms, envir = dat.env)

	}
    }
}
