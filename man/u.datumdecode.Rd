\name{u.datumdecode}
\alias{u.datumdecode}
\title{Convert ``Numeric'' Dates}
\description{
  Daten der Form 8710230920 aufspalten in Jahr, Monat, Tag, Std, Min
}
\usage{
u.datumdecode(d, YMDHMnames = c("Jahr", "Monat", "Tag", "Std", "Min"))
}
\arguments{
  \item{d}{numeric dates in the form YYMMDDHHMM.}
  \item{YMDHMnames}{(column) names to be used for the result.}
}
% \details{
%   ~~ If necessary, more details than the __description__  above ~~
% }
\value{
  a numeric matrix (or vector) with 5 columns containing the year,
  month, etc.
}
\author{?? (someone at SfS ETH)}
\note{
  MM: This is a wrong concept, and also suffers from the ``millenium bug''
  (by using only 2 digits for the year).
}
\seealso{\R's \emph{proper} date-time coding: \code{\link{DateTimeClasses}};
  \code{\link{u.date}} etc.
}
\examples{
u.datumdecode(8710230920)
##  Jahr Monat   Tag   Std   Min
##    87    10    23     9    20

u.datumdecode(c(8710230900, 9710230920, 0210230920))
##      Jahr Monat Tag Std Min
## [1,]   87    10  23   9  00
## [2,]   97    10  23   9  20
## [3,]    2    10  23   9  20
}
\keyword{utilities}
