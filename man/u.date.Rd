\name{u.date}
\alias{u.date}
\title{Return Date[-Time] String in 'European' Format}
\description{
  Return one string of the form "day/month/year", plus "hour:minutes",
  optionally.
}
\usage{
u.date(short=FALSE)
}
\arguments{\item{short}{logical; if \code{TRUE}, no time is given.}
}
\value{String with current date (and time).
}
\author{Martin Maechler, ca. 1992}
\seealso{\code{\link{u.Datumvonheute}}.}
\examples{
u.date()
u.date(short = TRUE)
}
\keyword{utilities}

