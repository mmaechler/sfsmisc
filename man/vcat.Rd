\name{vcat}
\alias{vcat}
\alias{ccat}
\title{Paste Utilities -- Concatenate Strings}
\description{
  Con\bold{cat}enate vector elements or anything using
  \code{\link{paste}(*, collapse = .)}.
  These are simple short abbreviations I have been using in my own codes
  in many places.
}
\usage{
vcat(vec, sep = " ")
ccat(...)
}
\arguments{
  \item{vec, \dots}{any vector and other arguments to be pasted to together.}
  \item{sep}{the separator to use, see the \emph{Details} section.}
}
\details{The functions are really just defined as \preformatted{
 vcat :=  function(vec, sep = " ") paste(vec, collapse = sep)
 ccat :=  function(...)           paste0(..., collapse = "")
}}
\value{
  a character string (of length 1) with the concatenated arguments.
}
\author{Martin Maechler, early 1990's.}
\seealso{\code{\link{paste}}, \code{\link{as.character}},
    \code{\link{format}}. \code{\link{cat}()} is really for printing.
}
\examples{
ch <- "is"
ccat("This ", ch, " it: ", 100, "\%")
vv <- c(1,pi, 20.4)
vcat(vv)
vcat(vv, sep = ", ")
}
\keyword{print}
\keyword{utilities}
