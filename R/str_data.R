
str.data <- function(pkg, ...)
{
    ## Purpose: str(.) of all datasets in a package
    ## ----------------------------------------------------------------------
    ## Arguments: pkg : name of package (as character string)
    ##		  ... : potential further arguments to be passed to str()
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 17 Jun 2005, 09:04
    dd <- data(package= pkg)
    items <- dd$res[,"Item"]
    dat.env <- new.env()
    for(n in items) {
	if(length(grep(".*\\(.*\\)$", n)) == 0) {
	    cat(n, ": ")
	    data(list = n, package = pkg, envir = dat.env)
	    nms <- ls(envir = dat.env, all.names=TRUE)
	    if(length(nms) == 1) { ## one data set == normal case
		if(nms != n) cat(nms, ": ")
		str(get(nms, envir = dat.env), ...)
	    } else { ## more than one data set
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

str.data("mlbench")

str.data("datasets", max=0, give.attr = FALSE)
