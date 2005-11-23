###-- Synchronize with ../man/Deprecated.Rd !!

nna <- function(data)
{
    ## Purpose: "No NA" :  throw out NA s, also for MATRIX data
    ## Author:   WSt, Date: Dec 89; simplified,improved: M.Maechler, Nov.93, 94
    ## --------------------------------------------------------------------
    .Deprecated("na.omit")
    ## ------ NOTE:  na.omit(.) is VERY SIMILAR (has attr()..)

    Error <- "'nna(.)' is defined for vectors, matrices & data.frames only"
    if(is.atomic(data) || is.data.frame(data)) {
        ##-- should work for 'named vectors' [is.vector -> F !], data.frames,..
        if(is.null(dim(data))) data[!is.na(data)]
        else if(is.matrix(data))  data[!apply(is.na(data), 1, any), ]
        else stop(Error)
    } else stop(Error)
}

digits.v <- function(nvec, base = 2, num.bits = 1 + floor(log(max(nvec),base)))
{
    warning("'digits.v'() is deprecated -- please use  digitsBase() instead!")
    digitsBase(nvec, base = base, ndigits = num.bits)
}

digits <- function(n, base = 10)
{
    warning("'digits'() is deprecated -- please use  digitsBase() instead!")
    drop(digitsBase(n, base=base))
}

tapply.num <- function(y, indices, Func) {
    warning("'tapply.num'() has been renamed to 'tapplySimpl()' and is deprecated\n  -- please use the new name instead!")

    tapplySimpl(y, indices, Func)
}


subtit <- function(t) {
    .Deprecated("use mtext(*, side=3, line=0) directly")
    mtext(t, side = 3, line = 0)
}

### p.triangle          Dreiecks-Plot fuer 3-er Gehalte / Anteile
p.triangle <- function(mat, label= "*", text.ecken = rep("",3), dreieck = TRUE)
{
  ## Purpose: 'Triangle plot' for plotting 3 proportions [a + b + c == 1]
  ## ----------------------------------------------------------------
  ## Arguments:
  ##      mat:   Matrix mit % A in der 1. Kolonne, % B in der 2.. (range 0:1)
  ##      label: Text der Laenge von mat[,1] zur Identifikation der Punkte
  ##      text.ecken: Ecken-Beschriftung
  ##      dreieck   : Falls T wird der Dreiecksrahmen gezeichnet
  ## EXAMPLE: p.triangle(rbind(c(.8,0),c(.1,.8),c(.1,.2), c(1,1)/3),label=1:4)
  ## ----------------------------------------------------------------

 .Deprecated("ternaryplot() in package 'vcd'")

  par(pty = "s")
  pa <- mat[, 1]
  pc <- 1 - mat[, 1] - mat[, 2]
  if(any(pc<0 | pc>1)) stop("proportions must be between 0 and 1")
  if(dreieck) {
    ecken <- matrix(c(0, 1, 0.5, 0, 0, sqrt(3)/2), ncol = 2)
    plot(rbind(ecken, c(0, 0)), type = "l", xlim = c(0, 1), ylim =
         c(0, 1), axes = FALSE, xlab = "", ylab = "")
    text(ecken, text.ecken)
  }
  text((1 - pa - pc/2), ((pc * sqrt(3))/2), label, col = 3)
}

p.panelL <- function(x,y) {
    warning("'p.panelL'() is deprecated -- use an anonymous function() instead")
    text(x,y);lines(lowess(x,y, f = .4),col = 2)
}
p.panelS <- function(x,y,df = 4) {
    warning("'p.panelS'() is deprecated -- use an anonymous function() instead")
    text(x,y);lines(smooth.spline(x,y,df = df),col = 2)
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

