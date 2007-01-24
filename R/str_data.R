
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
	dat.env <- new.env()
	for(n in items) {
	    if(length(grep(".*\\(.*\\)$", n)) == 0) {
		cat(n, ": ")
		data(list = n, package = pkg, envir = dat.env)
		nms <- ls(envir = dat.env, all.names=TRUE)
		if(length(nms) == 1) { ## one data set == normal case
		    if(nms != n) cat(nms, ": ")
		    str(get(nms, envir = dat.env), ...)
		}
		else { ## more than one data set
		    cat("\n")
		    for(nn in nms) {
			cat(" ", nn, ": ")
			str(get(nn, envir = dat.env), ...)
		    }
		}
		cat("--------------\n")
		rm(list = nms, envir = dat.env)
	    }
	}
    }
}
