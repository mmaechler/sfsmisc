\name{p.scales}
\alias{p.scales}
\title{Conversion between plotting scales: usr, cm, symbol}
\description{
  Give scale conversion factors of three coordinate systems in use for
  traditional R graphics: use, cm, symbol.
}
\usage{
p.scales(unit = relsysize * 2.54 * min(pin), relsysize = 0.05)
}
\arguments{
  \item{unit}{length of unit (or x and y units) of symbol coordinates in cm.}
  \item{relsysize}{same, as a proportion of the plotting area.}
}
\value{
  A numeric 2x2 matrix, with rows named \code{x} and \code{y}, and
  columns, named \code{"sy2usr"} and \code{"usr2cm"} which give the
  scale conversion factors from \sQuote{symbol} (as given) to
  \sQuote{usr} coordinates and from these to \sQuote{cm}, respectively.
}
\author{Werner Stahel, 1990;  simplification: M.Maechler, 1993, 2004}
\seealso{\code{\link{par}("usr")}, of also \code{("pin")} on which this
    is based.}
\examples{
p.scales()
}
\keyword{dplot}
