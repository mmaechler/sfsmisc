###-- Synchronize with ../man/Deprecated.Rd !!

###--- remove things from here to ../Old_Defunct/ex-Deprecated.R
###      ====                  == ==============================

pl.ds <- function(...) {
  warning("pl.ds() has been renamed to  plotDS().\n",
          "Please change your code to use the new name")
  plotDS(...)
}

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
