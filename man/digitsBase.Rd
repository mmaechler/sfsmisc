\name{digits}
\alias{digits}
\alias{digits.v}
\title{Base Representation of Integers}
\description{
  Compute the vector of ``digits'' \eqn{A} of the \code{base} \eqn{b}
  representation of a number \eqn{N}, \eqn{N = \sum_{k=0}^M  A_{M-k} b ^ k}{%
    N = sum(k = 0:M ; A[M-k] * b^k)}.
}
\usage{
digits.v(nvec, base = 2, num.bits = 1 + floor(log(max(nvec), base)))
digits  (n, base = 10)
}
\arguments{
  \item{n,nvec}{non-negative integer (vector) whose base \code{base}
    digits are wanted.}
  \item{base}{integer, at least 2 specifying the base for representation.}
  \item{num.bits}{number of bits/digits to use.}
}
\value{

  \code{digits.v}: a \code{\link{matrix}} \code{m} where \code{m[,i]}
  corresponds to \code{nvec[i]}.

  c( result ) then contains the blocks in proper order ..

}
\author{Martin Maechler, Dec 4, 1991}
\examples{
digits(128, base = 8) # 2 0 0

digits.v(0:12, 8) #-- octal representation
empty.dimnames(digits.v(0:33, 2)) # binary

hexdig <- c(0:9, LETTERS[1:6]) #-- hexadesimal coding :
i <- c(1:20, 100:120) # e.g.
M <- digits.v(i, 16)
cM <- hexdig[1 + M]; dim(cM) <- dim(M)
sub("^0+","", apply(cM,2,paste, collapse=""))
}
\keyword{arith}
