\name{lseq}
\alias{lseq}
\title{Generate Sequences, Equidistant on Log Scale}
\description{
  Generate sequences which are equidistant on a log-scale.
}
\usage{
lseq(from, to, length)
}
\arguments{
  \item{from}{starting value of sequence.}
  \item{to}{end value of the sequence.}
  \item{length}{desired length of the sequence.}
}
\value{
  a \code{\link{numeric}} vector of length \code{length}.
}
\seealso{\code{\link{seq}}.%% ~/R/D/r-devel/R/src/library/base/man/seq.Rd
}
\examples{
(x <- lseq(1, 990, length= 21))
plot(x, x^4,    type = "b", col = 2, log = "xy")
if(with(R.version, major >= 2 && minor >= 1))
plot(x, exp(x), type = "b", col = 2, log = "xy")
}
\keyword{manip}
