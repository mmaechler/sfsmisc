\name{u.Datumvonheute}
\alias{u.Datumvonheute}
\alias{C.Monatsname}
\alias{C.Wochentag}
\alias{C.Wochentagkurz}
\alias{C.weekday}
\title{Datum und Uhrzeit (auf deutsch)}
\description{
  Return current date and time as a string, possibly including day of
  the week in \emph{German}.
}
\usage{
u.Datumvonheute(W.tag=2, Zeit=FALSE)

C.Monatsname
C.Wochentag
C.Wochentagkurz
C.weekday
}
\arguments{
  \item{W.tag}{logical or integer specifying you want weekday (`Wochentag').
    \code{0} or \code{FALSE} gives no,  \code{1} or \code{TRUE} gives a
    short and \code{2} the long version of the day of the week.}
  \item{Zeit}{logical or integer specifying if time ("Zeit") is desired.
        \code{0} or \code{FALSE} gives no,  \code{1} or \code{TRUE} gives a
    hours only and \code{2} hours and minutes.}
}
\value{
  A string with the current date/time, in the form specified by the arguments.

  The \code{C.*} are \code{\link{character}} vector ``constants'',
  the German ones actually used by \code{u.Datumvonheute}.
}
\author{Caterina Savi, Martin Maechler}
\seealso{\code{\link{u.date}} for a similar English version, and
  \code{\link{p.datum}} which plots.
  For English month names, etc \code{\link{month.name}}.
}
\examples{
u.Datumvonheute()
u.Datumvonheute(W.tag=1, Zeit=TRUE)
u.Datumvonheute(W.tag= FALSE, Zeit=2)
}
\keyword{utilities}
