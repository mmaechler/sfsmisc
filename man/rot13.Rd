\name{rotn}
\alias{rotn}
\title{Generalized Rot13 Character Translation (Rotation)}
\description{
  Compute generalized \sQuote{rot13} character translations or
  \dQuote{rotations}

  In the distant past, considered as poor man's encryption, such
  rotations are way too poor nowadays and provided mainly for didactical
  reasons.
}
\usage{
rotn(ch, n = 13)
}
\arguments{
  \item{ch}{a \code{\link{character}} vector; often a string (of length 1).}
  \item{n}{an integer in \eqn{\{1\dots26\}}{{1...26}}; the default is
    particularly useful.}
}
\value{
  a character as \code{ch}, but with each character (which
  belongs to \code{\link{letters}} or \code{\link{LETTERS}}
  \dQuote{rotated} by \code{n} (positions in the alphabet).
}
\author{Martin Maechler}
\details{
  Note that the default \code{n = 13} makes \code{rotn} into
  a function that is its own inverse.

  Written after having searched for it and found
  \code{seqinr::rot13()} which was generalized and rendered more
  transparently to my eyes.
}
\seealso{
  \code{\link{rot2}}, a completely different rotation (namely in the
  plane aka \eqn{R^2}).
}
\examples{
rotn(c("ABC", "a","b","c"), 1)
rotn(c("ABC", "a","b","c"), 2)
rotn(c("ABC", "a","b","c"), 26) # rotation by 26 does not change much

(ch <- paste("Hello", c("World!", "you too")))
rotn(ch)
rotn( rotn(ch ) ) # rotn(*, 13) is its own inverse
}
\keyword{manip}