\name{u.sys}
\alias{u.sys}
\title{`Portable' System function (R / S-plus)}
\description{
  Convenient call to the underlying operating system.  The main purpose
  has been to provide a function with identical UI both in S-PLUS and \R.
  MM thinks you shouldn't use this anymore, usually.
}
\usage{
u.sys(\dots, intern = TRUE)
}
\arguments{
  \item{\dots}{any number of strings -- which will be
    \code{\link{paste}()}d together and passed to \code{system}.}
  \item{intern}{logical -- note that the default is \emph{reversed} from
    the one in \code{\link{system}()}.}
}
\author{Martin Maechler}
\seealso{\code{\link{system}}, really!}
\examples{
u.sys # shows how simply the function is defined :
\dontrun{
  function (..., intern = TRUE)
  system(paste(..., sep = ""), intern = intern)
}
}
\keyword{utilities}
\keyword{environment}% System, not R
