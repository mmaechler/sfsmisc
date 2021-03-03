\name{repChar}
\alias{repChar}
\alias{bl.string}
\title{Make Simple String from Repeating a Character, e.g. Blank String}
\description{
  Simple constructors of a constant character string from one character,
  notably a \dQuote{blank} string of given string length.

  M.M. is now \sQuote{\emph{mentally deprecating}} \code{bl.string} in
  favor of using \code{repChar()} in all cases.

  With \R 3.3.0 (May 2016), the \emph{new} function
  \code{\link{strrep}()} was introduced; it is faster typically, and more
  flexible, e.g. accepting a \emph{vector} for the 2nd argument.
  \cr
  This (for now informally) deprecates all uses of \code{repChar()} and \code{bl.string()}.
}
\usage{
repChar(char, no)
bl.string(no)
}
\arguments{
  \item{char}{single character (or arbitrary string).}
  \item{no}{non-negative integer.}
}
\value{
  One string, i.e., \code{\link{character}(1)}), for \code{bl.string} a
  blank string, fulfilling \code{n == nchar(bl.string(n))}.
}
\author{Martin Maechler, early 1990's (for \code{bl.string}).}
\seealso{\code{\link{paste}}, \code{\link{character}}, \code{\link{nchar}}.}
\examples{
r <- sapply(0:8, function(n) ccat(repChar(" ",n), n))
cbind(r)

repChar("-", 4)
repChar("_", 6)
## it may make sense to a string of more than one character:
repChar("-=- ", 6)

## show the very simple function definitions:
repChar
bl.string
}
\keyword{character}
