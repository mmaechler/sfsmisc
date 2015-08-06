\name{missingCh}
\alias{missingCh}
\title{Has a Formal Argument been Set or is it Missing?}
\description{
  \code{missingCh} can be used to test whether a value was specified
  as an argument to a function.  Very much related to the standard \R
  function \code{\link{missing}}, here the argument is given by its
  name, a character string.

  As \code{missingCh()} calls \code{missing()}, do consider the
  caveats about the latter, see \code{\link{missing}}.
}
\usage{
missingCh(x, envir = parent.frame())
}
\arguments{
  \item{x}{a \code{\link{character}} string.}
  \item{envir}{a (function evaluation) \code{\link{environment}}, in which
    the variable named \code{x} is to be \dQuote{missing}.}
}
\value{
  a \code{\link{logical}} indicating if the argument named \code{x} is
  \code{\link{missing}} in the function \dQuote{above}, typically the
  caller of \code{missingCh}, but see the use of \code{envir} in the
  \code{vapply} example.
}
\author{Martin Maechler}
\seealso{
  \code{\link{missing}}
}
\examples{
tst1 <- function(a, b, dd, ...) ## does not work an with argument named 'c' !
    c(b = missingCh("b"), dd = missingCh("dd"))
tst1(2)#-> both 'b' and 'dd' are missing
tst1(,3,,3)
##     b    dd
## FALSE  TRUE  -- as 'b' is not missing but 'dd' is.

Tst <- function(a,b,cc,dd,EEE, ...)
    vapply(c("a","b","cc","dd","EEE"), missingCh, NA, envir=environment())
Tst()
## TRUE ... TRUE -- as all are missing()
Tst(1,,3)
##     a     b    cc    dd   EEE
## FALSE  TRUE FALSE  TRUE  TRUE
## .....       .....
## as 'a' and 'cc' where not missing()

## Formal testing:
stopifnot(tst1(), !tst1(,3,3), Tst(),
                        Tst(1,,3, b=2, E="bar") == c(0,0,1,0,0))
## maybe surprising that this ^^ becomes 'dd' and only 'cc' is missing
}
\keyword{programming}
