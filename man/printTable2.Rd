\name{printTable2}
% source in ../R/printTable.R
\alias{printTable2}
\alias{margin2table}
\alias{print.margin2table}
%\title{Compute and Print Marginals etc for 2-way Contingency Tables}
\title{Berechne und Drucke Randtotale etc f�r 2-weg Kontingenz Tafeln}
\description{
  2-weg Kontingenztafel mit allem zusammen

  \code{margin2table()} computes marginals adds them to the table and
  returns a \code{margin2table} object the print method for which adds
  text decorations (using \code{"-"} and \code{"|"}).
}
\usage{
printTable2(table2, digits = 3)
margin2table(x)
\method{print}{margin2table}(x, digits = 3, \dots)
}
\arguments{
  \item{table2}{Matrix mit Anzahlen, die Kontingenztafel.}
  \item{x}{a matrix, or the result of \code{margin2table}.}
  \item{digits}{Anzahl Dezimalstellen, auf die die H�ufigkeiten gerundet
    werden sollen.}
  \item{\dots}{further potential arguments, unused currently.}
}
% \details{
%  .........
% }
\value{
  \code{margin2table} returns a matrix with \emph{added marginals},
  i.e., an extra row and column, and is of class \code{margin2table}
  which has a nice print method.

  \code{printTable2} is just producing output.
}
\author{Martin Maechler, Feb.1993; then Dec 2003}
\seealso{\code{\link{table}}, \code{\link{ftable}}.}

\examples{
data(HairEyeColor)
printTable2(HairEyeColor[,, "Male"])
printTable2(HairEyeColor[,, "Female"])
}
\keyword{utilities}
