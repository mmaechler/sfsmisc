\name{hatMat}
\alias{hatMat}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hatMat(x, trace=FALSE, pred.sm=function(x, y, ...) predict(smooth.spline(x, y, ...), x = x)$y, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{trace}{ ~~Describe \code{trace} here~~ }
  \item{pred.sm}{ ~~Describe \code{pred.sm} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
}
\details{
  ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of `comp1'}
  \item{comp2 }{Description of `comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	     or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, trace = FALSE,
                   pred.sm = function(x,y,...)
                   predict(smooth.spline(x,y, ...), x = x)$y,
                   ...)
{
    ## Purpose: Return Hat matrix of a smoother -- very general (but slow)
    ## -------------------------------------------------------------------------
    ## Arguments: x: the design points
    ##		trace: if TRUE, only the trace of the matrix is returned, i.e.,
    ##			sum(diag( <matrix> ))
    ##		pred.sm: a function(x,y,..), predict(smoother(x,y,...))
    ##		....   : smoothing parameters, for the pred.sm() function.
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  7 Mar 2001, 11:12
    n <- length(x)
    y <- pred.sm(x, numeric(n), ...)
    if(!is.numeric(y) || length(y) !=n)
        stop("`pred.sm' does not return a numeric length n vector")
    H <- if(trace) 0 else matrix(as.numeric(NA), n,n)
    for (i in 1:n) {
        y <- numeric(n) ; y[i] <- 1
        y <- pred.sm(x, y, ...)
        if(trace) H <- H + y[i] else H[,i] <- y
    }
    H
  }
}
\keyword{ ~kwd1 }% at least one, from doc/KEYWORDS
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
