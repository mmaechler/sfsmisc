\name{KSd}
\alias{KSd}
\title{Approximate Critical Values for Kolmogorov-Smirnov's D}
\description{
  Computes the critical value for Kolmogorov-Smirnov's \eqn{D_n}, for
  sample sizes \eqn{n \ge 10}{n >= 10} and confidence level 95\%.
}
\details{
  Based on tables values given in the reference below.
  For \eqn{n\le 80}{n <= 80} uses interpolations from exact values, elsewhere
  uses asymptotic approximation.
}
\usage{
KSd(n)
}
\arguments{
  \item{n}{the sample size, \code{n >= 10}.}
}
\value{
  The critical value for D (two-sided) for significance level 0.05 (or
  confidence level 95\%).
}
\references{
  Peter J. Bickel and Kjell A. Doksum (1977),
  \emph{Mathematical Statistics: Basic Ideas and Selected Topics}.
  Holden Day.
  Section 9.6 and table IX.
}
\author{Kjetil Halvorsen and Martin Maechler}

\seealso{Is used from \code{\link{ecdf.ksCI}}.}

\examples{
KSd(90)
KSd(1:9)# now works

op <- par(mfrow=c(2,1))
  plot(KSd, 10, 150)# nice
  abline(v = c(75,85), col = "gray")
  plot(KSd, 79, 81, n = 1001)# *very* tiny discontinuity at 80
par(op)
}
\keyword{distribution}
