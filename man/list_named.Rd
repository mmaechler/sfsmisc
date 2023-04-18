\name{list_named}
\alias{list_}
\title{Automatically Named list()}
\description{
  A version of \code{\link{list}(...)}, but with \dQuote{automatically}
  \emph{named} list components.
}
\details{
  The names are extracted from \code{\link{sys.call}()}, and the function
  is written to be \emph{fast}  (rather than easy to ready for the uninitiated ;-)
}
\usage{
list_(...)
}
\arguments{
  \item{\dots}{components to make up the resulting \code{\link{list}}.
    Their variable names (or unevaluated expressions in the call) will
    become the \code{\link{names}(.)} of the result.}
}
\value{
  a \code{\link{list}} with the components in the arguments with
  \code{\link{names}} taken from their call to \code{list_(..)}.
}
\author{Martin Maechler}
\seealso{
  \code{\link{list}}, \code{\link{names}}
}
\examples{
a <- 1:4; lett <- letters[1:9]; CH <- "Suisse"
all.equal(list (a, lett),
          list_(a, lett)) # "names for current but not for target"
str(list(a, lett, CH)) # [[1]], [[2]], .. (no names)
str(list_(a, lett, CH))#   $a   $lett  ..

stopifnot(identical(            list (a, lett, CH),
                    unname(L <- list_(a, lett, CH))),
          is.list(L), names(L) == c("a", "lett", "CH"),
          identical(lett, L$lett) ## etc
          )

## The function is currently defined as
function (...) `names<-`(list(...), vapply(sys.call()[-1L], as.character, ""))
}
\keyword{utilities}
