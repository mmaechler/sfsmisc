\name{digitsBase}
\alias{digitsBase}
\title{Digit/Bit Representation of Integers in any Base}
\description{
  Compute the vector of \dQuote{digits} \eqn{A} of the \code{base} \eqn{b}
  representation of a number \eqn{N}, \eqn{N = \sum_{k=0}^M  A_{M-k} b ^ k}{%
    N = sum(k = 0:M ; A[M-k] * b^k)}.
}
\usage{
digitsBase(x, base = 2, ndigits = 1 + floor(log(max(x), base)))
}
\arguments{
  \item{x}{non-negative integer (vector) whose base \code{base}
    digits are wanted.}
  \item{base}{integer, at least 2 specifying the base for representation.}
  \item{ndigits}{number of bits/digits to use.}
}
\value{
  a \code{\link{matrix}} \code{m} where \code{m[,i]}
  corresponds to \code{x[i]}.

  \code{c( result )} then contains the blocks in proper order ..
}
\note{\code{digits} and \code{digits.v} are now deprecated and will be
  removed from the \pkg{sfsmisc} package.
}
\author{Martin Maechler, Dec 4, 1991 (for S-plus; then called \code{digits.v}).}
\examples{
digitsBase(0:12, 8) #-- octal representation
empty.dimnames(digitsBase(0:33, 2)) # binary

## This may be handy for just one number (and default decimal):
digits <- function(n, base = 10) drop(digitsBase(n, base = base))
digits(128, base = 8) # 2 0 0

hexdig <- c(0:9, LETTERS[1:6]) #-- hexadesimal coding :
i <- c(1:20, 100:120) # e.g.
M <- digitsBase(i, 16)
cM <- hexdig[1 + M]; dim(cM) <- dim(M)
sub("^0+","", apply(cM,2,paste, collapse=""))
}
\keyword{arith}
\keyword{utilities}
