\name{p.datum}
\alias{p.datum}
\title{Plot `Datum' (deutsch!) unten rechts}
\description{
  Plot the date (and time, if required) in German, at the lower right hand
  margin of your plot.date
}
\usage{
p.datum(outer = FALSE, cex = 0.75, ...)
}
\arguments{
  \item{outer}{logical; passed to \code{\link{mtext}}.}
  \item{cex}{non-negative; passed to \code{\link{mtext}}.}
  \item{\dots}{any arguments to \code{\link{u.Datumvonheute}}.}
}
\seealso{\code{\link{u.date}}, \code{\link{date}}.}
\examples{
plot(1)
p.datum()
}
\keyword{hplot}%-- one or more ...
