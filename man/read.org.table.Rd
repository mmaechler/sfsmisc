\name{read.org.table}
\title{Read.table for an Emacs Org Table}
\alias{read.org.table}
\description{
  Read an emacs \dQuote{Org} table (in \code{file} or \code{text}) by
  \code{\link{read.table}()}. % FIXME: Should be easy to made to work for some of the
  %% Markdown formats for tables.
}
\usage{
read.org.table(file, header = TRUE, skip = 0,
               encoding = "native", fileEncoding = "", text, \dots)
}
\arguments{
  \item{file}{a file name, a \code{\link{file}} or other connection.}
  \item{header}{logical indicating if the org table has header line (in
    the usual \code{"|"}-separated org table format).}
  \item{skip}{integer number of initial lines to skip.}
  \item{encoding}{to be used in the main
    \code{\link{readLines}(file, encoding=encoding)} call.}
  \item{fileEncoding}{if \code{file} is a file name, i.e., a
    \code{\link{character}} string, and \code{fileEncoding} is not the
    empty string, \code{file(file, "rt", encoding = fileEncoding)} will
    be used.}
  \item{text}{instead of \code{file}, a \code{\link{character}} or
    string (of a few lines, typically).}
  \item{\dots}{further arguments passed to \code{\link{read.table}}.
    You should \emph{not} use \code{encoding} (but possibly
    \code{fileEncoding}!) here, as we do not call
    \code{\link{read.table}} on \code{file} (but on a
    \code{\link{textConnection}}).}
}
\value{
  a \code{\link{data.frame}}
}
\note{TODO: It should be easy to extend \code{read.org.table()}  to also
  work for some of the proposed Markdown formats for tables.
  Please write to \code{\link{maintainer}("sfsmisc")} or open a
  github issue if you are interested.
}
\references{
  Org-Mode \emph{Manual} on tables, \url{http://orgmode.org/manual/Tables.html}

  Org \emph{tutorial} for tables, \url{http://orgmode.org/worg/org-tutorials/tables.html}
}
\seealso{
  CRAN package \CRANpkg{ascii} % \CRANpkg{ascii} % <- needs R >= 3.2.0
  can \emph{write} org tables.
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
