p.ts <- function(x, nrplots = max(1, min(8, n%/%400)), overlap = nk %/% 16,
                 main.tit=NULL, quiet = FALSE, ...)
{
  ## Purpose: plot.ts with multi-plots + Auto-Title -- currently all on 1 page
  ## -------------------------------------------------------------------------
  ## Arguments: x      : timeseries [ts,rts,its,cts] or numeric vector
  ##            nrplots: number of sub-plots    [DEFAULT: in {1..8}, ~= n/400]
  ##            overlap: how much should subsequent plots overlap [DEFAULT:...]
  ##
  ## Depends on   mult.fig()
  ##
  ## ---> help page  ?p.ts
  ##
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date:  1 Jul 1994; 18.Dec,1998.

  if(is.null(main.tit)) main.tit <- paste(deparse(substitute(x)))
  n <- length(x)
  if(nrplots == 1) plot.ts(x, ..., main = main.tit)
  else {
    if(nrplots <= 0) return(nrplots)
    if(n<=1) stop("'x' must have at least two points!")
    do.dates <- !is.null(class(x)) && class(x) == "cts"
    if(do.dates) x <- as.rts(x)# dates() as below fails [S+ 3.4]
    scal <- (end(x) - (t1 <- start(x)))/(n-1)
    nk <- n %/% nrplots
    yl <- range(pretty(range(x, na.rm = TRUE)))
    pp <- mult.fig(mfrow=c(nrplots,1), main=main.tit, quiet = TRUE)
    on.exit(par(pp $ old.par))
    
    for(i in 1:nrplots) {
      i0   <- max(0, (-overlap+(i-1)*nk)-1)
      in1 <- min(n, i*nk + overlap)-1
      st <- t1 + scal*i0 ##;  if(do.dates) st <- dates(st)
      en <- t1 + scal*in1##; if(do.dates) en <- dates(en)
      if(!quiet) cat(i," -- start= ", format(st), "; end  =", format(en),"\n")
      plot.ts(window(x, start= st, end  = en), ylim= yl, ...)
    }
  }
}
