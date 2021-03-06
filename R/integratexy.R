## This is also sym.linked into
## Martin's WpDensity package /u/maechler/R/Pkgs/WpDensity/

integrate.xy <- function(x,fx, a,b, use.spline = TRUE, xtol = 2e-8)
{
  if(is.list(x)) {
    fx <- x$y; x <- x$x
    if(length(x) == 0)
        stop("list 'x' has no valid $x component")
  }
  if((n <- length(x)) != length(fx))
      stop("'fx' must have same length as 'x'")

  if(is.unsorted(x)) { i <- sort.list(x); x <- x[i]; fx <- fx[i] }
  if(any(i <- duplicated(x))) {
      n <- length(x <- x[!i])
      ## we might have to check that the same fx[] are duplicated
      ## otherwise either give an error or take the mean() of those...
      fx <- fx[!i]
  }
  if(any(diff(x) == 0))
     stop("bug in 'duplicated()' killed me: have still multiple x[]!")

  if(missing(a)) a <- x[1]
    else if(any(a < x[1])) stop("'a' must NOT be smaller than min(x)")
  if(missing(b)) b <- x[n]
    else if(any(b > x[n])) stop("'b' must NOT be larger  than max(x)")
  if(length(a) != 1 && length(b) != 1 && length(a) != length(b))
    stop("'a' and 'b' must have length 1 or same length !")
  else {
      k <- max(length(a),length(b))
      if(any(b < a))    stop("'b' must be elementwise >= 'a'")
  }

  if(use.spline) {
    xy <- spline(x,fx, n = max(1024, 3*n))
    ##-- Work around spline(.) BUG:  (ex.:  range(spline(1:20,1:20,n=95)))
    if(xy$x[length(xy$x)] < x[n]) {
        if(TRUE) cat("working around spline(.) BUG --- hmm, really?\n\n")
      xy$x <- c(xy$x,  x[n])
      xy$y <- c(xy$y, fx[n])
    }
    ## END if work around ----
    x <- xy$x; fx <- xy$y
    n <- length(x)
  }

  ab <- unique(c(a,b))
  BB <- abs(outer(x,ab,"-")) < (xtol * max(b - a))
  if(any(j <- 0 == colSums(BB))) { # the j-th element(s) of ab are not in x[]
    y <- approx(x,fx, xout = ab[j])$y
    x <- c(ab[j],x)
    i <- sort.list(x)
    x <- x[i];  fx <- c(y,fx)[i];  n <- length(x)
  }

  ##--- now we could use 'Simpson's formula IFF the x[i] are equispaced... --
  ##--- Since this may well be wrong, just use 'trapezoid formula':

  dig0 <- floor(-log10(xtol)) #
  f.match <- function(x,table,dig) match(signif(x,dig), signif(table,dig))
  ## was (S+) f.match <- function(x,table) match(as.single(x), as.single(table))

  d <- dig0; while(anyNA(ai <- f.match(a,x, d))) d <- d - 1/8 ; ai <- rep_len(ai, k)
  d <- dig0; while(anyNA(bi <- f.match(b,x, d))) d <- d - 1/8 ; bi <- rep_len(bi, k)
  dfx <- fx[-c(1,n)] * diff(x,lag = 2)
  r <- numeric(k)
  for (i in 1:k) {
    a <- ai[i];  b <- bi[i]
    r[i] <- (x[a+1] - x[a])*fx[a] + (x[b] - x[b-1])*fx[b] +
      sum(dfx[seq(a, length = max(0,b-a-1))])
  }
  r/2
}
