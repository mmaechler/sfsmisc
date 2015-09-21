\name{AsciiToInt}
\encoding{latin1}
\alias{AsciiToInt}
\alias{ichar}
\alias{chars8bit}
\alias{strcodes}
\title{Character to and from Integer Codes Conversion}
\description{
  \code{AsciiToInt} returns \code{\link{integer}} codes in \code{0:255}
  for each (one byte) character in \code{strings}.  \code{ichar} is an
  alias for it, for old S compatibility.

  \code{strcodes} implements in \R the basic engine for translating
  characters to corresponding integer codes.

  \code{chars8bit()} is the \emph{inverse} function of
  \code{AsciiToint}, producing \dQuote{one byte} characters from integer
  codes.  Note that it (and hence \code{strcodes()} depends on the
  locale, see \code{\link{Sys.getlocale}()}.
}
\usage{
AsciiToInt(strings)
     ichar(strings)
chars8bit(i = 1:255)
strcodes(x, table = chars8bit(1:255))
}
\arguments{
  \item{strings, x}{\code{\link{character}} vector.}
  \item{i}{numeric (integer) vector of values in \code{1:255}.}
  \item{table}{a vector of (unique) character strings, typically of one
  character each.}
}
\details{
  Only codes in \code{1:127} make up the ASCII encoding which should be
  identical for all \R versions, whereas the \emph{\sQuote{upper}} half
  is often determined from the ISO-8859-1 (aka \dQuote{ISO-Latin 1)}
  encoding, but may well differ, depending on the locale setting, see
  also \code{\link{Sys.setlocale}}.

  Note that \code{0} is no longer allowed since, \R does not allow
  \code{\\0} aka \code{nul} characters in a string anymore.
}
\value{
  \code{AsciiToInt} (and hence \code{ichar}) and \code{chars8bit} return a
  vector of the same length as their argument.

  \code{strcodes(x, tab)} returns a \code{\link{list}} of the same
  \code{\link{length}} and \code{\link{names}} as \code{x} with list
  components of integer vectors with codes in \code{1:255}.
}
\author{Martin Maechler, partly in 1991 for S-plus}
\examples{
chars8bit(65:70)#-> "A" "B" .. "F"
stopifnot(identical(LETTERS,   chars8bit(65:90)),
          identical(AsciiToInt(LETTERS), 65:90))

% In R 2.1.0, the "ü" could not even be parsed in UTF-8; now gives NA
## may only work in ISO-latin1 locale (not in UTF-8):
try( strcodes(c(a= "ABC", ch="1234", place = "Zürich")) )
## in "latin-1" gives  {otherwise should give NA instead of 252}:
\dontrun{
$a
[1] 65 66 67

$ch
[1] 49 50 51 52

$place
[1]  90 252 114 105  99 104
}
 myloc <- Sys.getlocale()

if(.Platform $ OS.type == "unix") { # ''should work'' here
  try( Sys.setlocale(locale = "de_CH") )# "try": just in case
  print(strcodes(c(a= "ABC", ch="1234", place = "Zürich"))) # no NA hopefully
  print(AsciiToInt(chars8bit()))# -> 1:255  {if setting latin1 succeeded above}

  print(chars8bit(97:140))
  try( Sys.setlocale(locale = "de_CH.utf-8") )# "try": just in case
  print(chars8bit(97:140)) ## typically looks different than above
}

## Resetting to original locale .. works "mostly":
lapply(strsplit(strsplit(myloc, ";")[[1]], "="),
       function(cc) try(Sys.setlocale(cc[1], cc[2]))) -> .scratch

Sys.getlocale() == myloc # TRUE if we have succeeded to reset it

}
\keyword{manip}
%\keyword{utilities}
