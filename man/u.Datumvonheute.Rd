\name{u.Datumvonheute}
\alias{u.Datumvonheute}
\title{Datum und Uhrzeit (deutsch!)}
\description{
  Return current date and time as a string, possibly including day of
  the week.
}
\usage{
u.Datumvonheute(W.tag=2, Zeit=F)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{W.tag}{logical or integer specifying you want weekday
    ("Wochentag").
    \code{0} or \code{FALSE} gives no,  \code{1} or \code{TRUE} gives a
    short and \code{2} the long version of the day of the week.}
  \item{Zeit}{logical or integer specifying if time ("Zeit") is desired.
        \code{0} or \code{FALSE} gives no,  \code{1} or \code{TRUE} gives a
    hours only and \code{2} hours and minutes.}
}
\value{
  A string with the current date/time, in the form specified by the arguments.
}
\author{Caterina Savi, Martin Maechler}
\seealso{\code{\link{u.date}} for a similar English version;
  \code{\link{p.datum}} which plots.
}
\examples{
u.Datumvonheute()
u.Datumvonheute(W.tag=1, Zeit=T)
u.Datumvonheute(W.tag= FALSE, Zeit=2)
}
\keyword{utilities}%-- one or more ...
