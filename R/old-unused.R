### p.corr              ASCII-Plot einer Korrelations-Matrix  u.a. mehr
### p.00                "reset device" -- Versuch (!)
### p.m                 matrixplot: EINE Matrix = [x y1 y2 ...]
### p.xy                Scatterplot "easy"
### p.t                 Scatterplot "easy" -- case number instead of symbol
### p.plot.text

if(FALSE) { #============== very outdated ====================


## p.corr <- function(...) stop("use 'symnum' instead of 'p.corr'")


"p.00" <- function()
{
  ## Purpose: RESET current Device [after usage of par(..) ...]
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 23 Jun 93
  ## -------------------------------------------------------------------------
  par(mfrow=c(1,1))
  Device.Default(.Device)
  frame()
  ##-- this is NOT AT ALL sufficient !! --- still not really reset ---
  ## Try:
  ## motif();par.motif <- par(); mult.fig(5,main="Several"); tsplot(hstart)
  ## length(all.equal(par(), par.motif))
  ## p.00()
  ## all.equal(par(), par.motif) #-- still MANY not ok ...

### FIX THIS by 'fixing' Device.Default : It should SAVE the par(.) at
### beginning in frame 0; next time it is called, it first looks in frame
### 0 if it can use those  par.NAME and if yes  do  par(par.Name)

###--> Werner's solution: works for motif and postscript, using GLOBAL
###--> variables  DevDef.... -----> function  'u.dev.default'
}

p.m <- function(mat, ...) matplot(mat[, 1], as.matrix(mat[, -1]), ...)

p.xy <- function(x , y, ...)
{
  ## Purpose: Easy plot, also do sorting -- may use in 'pairs' as panel=p.xy
  ## Author: Martin Maechler, Date: Jan 1992; Sept 93
  ## ----------------------------------------------------------------
  ## Arguments: As with plot; e.g.  p.xy(sin(pi/10*1:100), main = "Sine")
  ## ----------------------------------------------------------------
  if(missing(y))
    invisible(plot(x, ..., type = "b", xlab="", ylab=""))
  else { i <- sort.list(x)
         invisible(plot(x[i], y[i], ..., type = "b", xlab="", ylab=""))
       }
}

p.t <- function(x,y,...) {
  ## Purpose: Easy plot, WITH numeric labels
  ## Author: Martin Maechler, Date: Sept 93
  ## ----------------------------------------------------------------
  ## Arguments: As with plot; e.g.  p.t(sin(pi/10*1:99), main = "Sine")
  ## ----------------------------------------------------------------
  if(missing(y)) { y <- x; x <- seq(along=y) }
  plot(x, y, ..., type='b', pch=' ', xlab='', ylab='')
  text(x,y, cex=.8*par("cex"))
}

p.plot.text <- function(x, y, labels = seq(along = x),
                        xlab = deparse(substitute(x)),
                        ylab = deparse(substitute(y)),
                        col = par("col"), textcex = 0.7, ...)
{
  ## Purpose: make scatterplot with text instead of 'O' at points (x,y).
  ## -------------------------------------------------------------------------
  ## Arguments: x,y : coordinates of points.
  ##         labels : vector of labels for each point
  ##            col : color parameter for each text string. This may be a
  ##                  vector of more than one element.
  ##        textcex : character expansion parameter for each text string.
  ##                  This may be a vector of more than one element.
  ##            ... : Graphical parameters may also be supplied as arguments
  ##                  to this function (see par).
  ## -------------------------------------------------------------------------
  ## Author: Prisca Durrer, Date: 17 Sep 92, 08:24
  n <- length(x)
  if(n != length(y))
    stop("x & y must have same length")
  warning("Martin Maechler thinks you should use n.plot() instead...")
  plot(x, y, type = "n", xlab = xlab, ylab = ylab, ...)
  text(x, y, labels = labels, cex = textcex, col = col)
}

give.xy.list <- function(x, y)
{
  stop("Use xy.coords() in R")
}




}
