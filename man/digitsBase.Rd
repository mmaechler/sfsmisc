\name{digitsBase}
\alias{digitsBase}
\alias{as.intBase}
%% TODO: use NAMESPACE to exclude:
\alias{as.integer.basedInt}
\alias{print.basedInt}
\title{Digit/Bit Representation of Integers in any Base}
\description{
  Integer number representations in other Bases.

  Formally, for every element \eqn{N =}\code{x[i]}, compute the (vector
  of) \dQuote{digits} \eqn{A} of the \code{base} \eqn{b}
  representation of the number \eqn{N}, \eqn{N = \sum_{k=0}^M  A_{M-k} b ^ k}{%
    N = sum(k = 0:M ; A[M-k] * b^k)}.\cr
  Revert such a representation to integers.
}
\usage{
digitsBase(x, base = 2, ndigits = 1 + floor(log(max(x), base)))
\method{as.integer}{basedInt}(x, \dots)
\method{print}{basedInt}(x, \dots)

as.intBase(x, base = 2)
}
\arguments{
  \item{x}{For \code{digitsBase()}: non-negative integer (vector) whose
    base \code{base} digits are wanted.

    For \code{as.intBase()}: \cr a list of numeric vectors, a character
    vector, or an integer matrix as returned by \code{digitsBase()},
    representing digits in base \code{base}.
  }
  \item{base}{integer, at least 2 specifying the base for representation.}
  \item{ndigits}{number of bits/digits to use.}
  \item{\dots}{potential further arguments passed to methods, notably
    \code{\link{print}}.}
}
\value{
  For \code{digitsBase()}, an object, say \code{m}, of class
  \code{"basedInt"} which is basically a (\code{ndigits} x \code{n})
  \code{\link{matrix}} where \code{m[,i]} corresponds to \code{x[i]},
  \code{n <- length(x)} and \code{attr(m,"base")} is the input
  \code{base}.

  \code{as.intBase()} and the \code{\link{as.integer}} method for
  \code{basedInt} objects return an \code{\link{integer}} vector.
}
\note{\code{digits} and \code{digits.v} are now deprecated and will be
  removed from the \pkg{sfsmisc} package.
}
\author{Martin Maechler, Dec 4, 1991 (for S-plus; then called \code{digits.v}).}
\examples{
digitsBase(0:12, 8) #-- octal representation
empty.dimnames(digitsBase(0:33, 2)) # binary

## This may be handy for just one number (and default decimal):
digits <- function(n, base = 10) as.vector(digitsBase(n, base = base))
digits(128, base = 8) # 2 0 0

## one way of pretty printing (base <= 10!)
b2ch <- function(db)
        noquote(gsub("^0+(.{1,})$"," \\\\1", % \\\\  |--> \\ in example R core
                apply(db, 2, paste, collapse = "")))
b2ch(digitsBase(0:33, 2))  #->  0 1 10 11 100 101 ... 100001
b2ch(digitsBase(0:33, 4))  #->  0 1 2 3 10 11 12 13 20 ... 200 201

## Hexadecimal:
i <- c(1:20, 100:106)
M <- digitsBase(i, 16)
hexdig <- c(0:9, LETTERS[1:6])
cM <- hexdig[1 + M]; dim(cM) <- dim(M)
b2ch(cM) #->  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F 10 11 ... 6A

## Inverse of digitsBase() : as.integer method for the "basedInt" class
as.integer(M)
## or also as.intBase() working from strings:
(cb <- apply(digitsBase(0:33, 4), 2, paste, collapse = ""))
##-> "000" "001" ..... "200" "201"
all(0:33 == as.intBase(cb, base = 4))
}
\keyword{arith}
\keyword{utilities}
