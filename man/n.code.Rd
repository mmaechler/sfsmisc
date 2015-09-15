\name{n.code}
\alias{n.code}
\alias{code2n}
\title{Convert "Round" Integers to Short Strings and Back}
\description{
  \code{n.code} convert \dQuote{round integers} to short character strings.
  This is useful to build up variable names in simulations, e.g.

  \code{code2n} is the \emph{inverse} function of \code{n.code()}.
}
\usage{
n.code(n,    ndig = 1, dec.codes = c("", "d", "c", "k"))
code2n(ncod, ndig = 1, dec.codes = c("", "d", "c", "k"))
}
\arguments{
  \item{n}{integer vector.}
  \item{ncod}{character vector, typically resulting from
    \code{n.code}.}
  \item{ndig}{integer giving number of digits before the coding character.}
  \item{dec.codes}{character code for 1, 10, 100, 1000 (etc).}
}
% \details{
%   ~~ If necessary, more details than the __description__  above ~~
% }
\value{
  \code{n.code(n)} returns a \code{\link{character}} vector of the same
  length as \code{n}.

  \code{code2n(ncod)} returns a \code{\link{integer}} vector of the same
  length as \code{ncod}.

  Usually, \code{code2n(n.code(n)) == n}.
}
\author{Martin Maechler}
%\seealso{ ..}
\examples{
n10 <-  c(10,20,90, 100,500, 2000,10000)
(c10 <- n.code(n10))#-> "1d" "2d" "9d" "1c" ..
stopifnot(code2n(c10) == n10)
}
\keyword{utilities}

