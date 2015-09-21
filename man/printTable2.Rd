\name{printTable2}
\encoding{latin1}
% source in ../R/printTable.R
\alias{printTable2}
\alias{margin2table}
\alias{print.margin2table}
\title{Add and Print Marginals for 2-way Contingency Tables}
%\title{Berechne und Drucke Randtotale etc für 2-weg Kontingenz Tafeln}
\description{
  \code{printTable2()} prints a 2-way contingency table \dQuote{with all
    bells and whistles} (currently using German labeling).

  \code{margin2table()} computes marginals, adds them to the table and
  returns a \code{margin2table} object the print method for which adds
  text decorations (using \code{"-"} and \code{"|"}).
}
\usage{
printTable2(table2, digits = 3)
margin2table(x, totName = "sum", name.if.empty=FALSE)
\method{print}{margin2table}(x, digits = 3, quote = FALSE, right = TRUE, \dots)
}
\arguments{
  \item{table2}{a matrix with non-negative integer entries, i.e. the
  contingency table.}%Matrix mit Anzahlen, die Kontingenztafel.
  \item{x}{a matrix; for \code{print()}, the result of \code{margin2table}.}
  \item{digits}{Anzahl Dezimalstellen, auf die die Häufigkeiten gerundet
    werden sollen.}
  \item{quote, right}{logicals passed to \code{\link{print.default}()},
    but with different default values.}
  \item{totName}{string to use as row- and column- name if \code{x} has
    corresponding \code{\link{dimnames}}.}
  \item{name.if.empty}{logical indicating if the margin \dQuote{totals}
    should be named in any case.}
  \item{\dots}{further potential arguments, unused currently.}
}
% \details{
%  .........
% }
\value{
  \code{margin2table} returns a matrix with \emph{added marginals},
  i.e., an extra row and column, and is of class \code{"margin2table"}
  (and \code{"\link{table}"} still) which has a nice print method.

  \code{printTable2} is just producing output.
}
\author{Martin Maechler, Feb.1993; then Dec 2003}
\seealso{\code{\link{table}}, \code{\link{ftable}}.}

\examples{
margin2table(diag(4),,TRUE)
m <- diag(3); colnames(m) <- letters[1:3]
margin2table(m)
margin2table(m / sum(m))

data(HairEyeColor)
margin2table(HairEyeColor[,, "Male"])
printTable2(HairEyeColor[,, "Male"])
printTable2(HairEyeColor[,, "Female"])
}
\keyword{utilities}

