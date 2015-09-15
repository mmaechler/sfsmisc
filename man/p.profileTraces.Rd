\name{p.profileTraces}
\encoding{latin1}
\alias{p.profileTraces}
\title{Plot a profile.nls Object With Profile Traces}
\description{
  Displays a series of plots of the profile t function and the likelihood
  profile traces for the parameters in a nonlinear regression model that
  has been fitted with \code{\link{nls}} and profiled with
  \code{\link{profile.nls}}.
}
\usage{
p.profileTraces(x, cex = 1,
                subtitle = paste("t-Profiles and traces of ",
                       deparse(attr(x,"summary")$formula)))
}
\arguments{
  \item{x}{an object of class \code{"profile.nls"}, typically resulting from
    \code{profile(\link[stats]{nls}(.))}, see
    \code{\link[stats]{profile.nls}}.}
  \item{cex}{character expansion, see \code{\link{par}(cex =)}.}
  \item{subtitle}{a subtitle to set for the plot.  The default now
    includes the \code{\link{nls}()} formula used.}
}
%- \details{
% .........
%- }
\author{Andreas Ruckstuhl, \R port by Isabelle Flückiger and Marcel Wolbers}
\note{the \pkg{stats}-internal \code{stats:::plot.profile.nls} plot
  method just does \dQuote{the diagonals}.
}
\seealso{\code{\link{profile}}, and \code{\link{nls}} (which has
  unexported \code{profile} and \code{stats:::plot.profile.nls} methods).
}
\examples{
require(stats)
data(Puromycin)
Treat <- Puromycin[Puromycin$state == "treated", ]
fm <- nls(rate ~ T1*conc/(T2+conc), data=Treat,
          start = list(T1=207,T2=0.06))
(pr <- profile(fm)) # quite a few things..
op <- par(mfcol=1:2)
plot(pr) # -> 2 'standard' plots
par(op)
## ours:
p.profileTraces(pr)
}
\keyword{hplot}
\keyword{nonlinear}
