\name{p.datum}
\alias{p.datum}
\title{Plot `Datum' (deutsch!) unten rechts}
\description{
  Plot the date (& time, if required) in German, at the lower right hand
  margin of your plot.date
}
\usage{
p.datum(outer=FALSE, ...)
}
\arguments{
  \item{outer}{logical; passed to \code{\link{mtext}}.}
  \item{\dots}{Any arguments to \code{\link{u.Datumvonheute}}.}
}
\seealso{\code{\link{u.date}}, \code{\link{date}}.}
\examples{
plot(1)
p.datum()
}
\keyword{hplot}%-- one or more ...
