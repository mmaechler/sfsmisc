### Functions moved from  ./Deprecated.R
###                       ~~~~~~~~~~~~~~~
###--- remove things from here to ../Old_Defunct/ex-Deprecated.R
###      ====                  == ==============================

###___ add on top ___

## Deprecated in 2005; defunctified 2016-12-01 :
list2mat <- function(x, check = TRUE)
{
  ## Purpose:  list -> matrix
  ## -------------------------------------------------------------------------
  ## Arguments: x a list whose first 2 el.  MUST be equal length vectors
  ##		check: if T, check if lengths are ok.   F: "quick & dirty"
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 19 May 93, 09:46

    stop("list2mat(x) has been deprecated in 2005  and is defunct now.
  Use  sapply(x, c)  or vapply(..) instead!")
}


pl.ds <- function(...) {
    stop("pl.ds() has been renamed to  plotDS() and is defunct now.\n",
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

  stop("p.pllines() is defunct: in R, use",
          "plot(x,y, lty=group, type='l', ...)")

  plot(x,y,type="n",...)
  ngr <- max(group)
  for (gg in 1:ngr) {
    ii <- group==gg & !is.na(x) & !is.na(y)
    if(sum(ii)) lines(x[ii],y[ii],lty=lty[1+(gg-1)%%length(lty)])
  }
}
