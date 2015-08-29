\name{read.org.table}
\title{Read.table for an Emacs Org Table}
\alias{read.org.table}
\description{
  Read an emacs \dQuote{Org} table (in \code{file} or \code{text}) by
  \code{\link{read.table}()}.
}
\usage{
read.org.table(file, header = TRUE, skip = 0, fileEncoding = "", text, \dots)
}
\arguments{
  \item{file}{a file name, a \code{\link{file}} or other connection.}
  \item{header}{logical indicating if the org table has header line (in
    the usual \code{"|"}-separated org table format).}
  \item{skip}{integer number of initial lines to skip.}
  \item{fileEncoding}{see \code{\link{read.table}}}
  \item{text}{instead of \code{file}, a \code{\link{character}} or
    string (of a few lines, typically).}
  \item{\dots}{further arguments passed to \code{\link{read.table}}.}
}
\value{
  a \code{\link{data.frame}}
}
\references{
  Org-Mode Manual on Tables, \url{http://orgmode.org/manual/Tables.html}

  Org tutorial for tables, \url{http://orgmode.org/manual/Tables.html}
}
\seealso{
  CRAN package \CRANpkg{ascii} can \emph{write} org tables.
  \code{\link{read.table}}
}
\examples{
t1 <-
"
| a | var2 |   C |
|---+------+-----|
| 2 | may  | 3.4 |
| 7 | feb  | 4.7 |
"
d <- read.org.table(text = t1)
d
stopifnot(dim(d) == c(2, 3),
          identical(names(d), c("a", "var2", "C")),
          d[,"a"] == c(2,7))
}
\keyword{file}
\keyword{utilities}
