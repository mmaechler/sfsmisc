rrange <- function(x, range = 1, coef = 1.5)
{
  ## Purpose: `Robust RANGE', ===>  ?rrange
  ## Author: Martin Maechler, 1990
  if(!missing(range)) {
    if(!missing(coef)) stop("Must use either `range' or `coef'")
    coef <- 1.5 * range
  }
  ## Work around R 1.2.3 buglet :
  do <- 123 == 100 * as.numeric(R.version$major) +
                10 * as.numeric(R.version$minor)

  ## S: (boxplot(..., plot = FALSE)$stats)[c(5, 1)]
  boxplot.stats(x, coef=coef, do.conf = FALSE,
                do.out = do)$stats[c(1,5)]
}
