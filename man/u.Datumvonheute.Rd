\name{u.Datumvonheute}
\alias{u.Datumvonheute}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~}
\description{
 ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
u.Datumvonheute(W.tag=2, Zeit=F)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{W.tag}{ ~~Describe \code{W.tag} here~~ }
 \item{Zeit}{ ~~Describe \code{Zeit} here~~ }
}
\details{
 ~~ If necessary, more details than the __description__  above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of `comp1'}
  \item{comp2 }{Description of `comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	     or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (W.tag = 2, Zeit = F) 
{
    dat <- as.numeric(system("date '+%w %d %m %y %H %M' | tr ' '  '\n'", 
        TRUE))
    DMY <- paste(dat[2], ". ", C.Monatsname[dat[3]], " ", dat[4], 
        sep = "")
    r <- if (W.tag) {
        W <- ifelse(dat[1] == 0, 7, dat[1])
        if (W.tag == 2) 
            Wtag <- C.Wochentag[W]
        else Wtag <- C.Wochentagkurz[W]
        paste(Wtag, DMY, sep = ", ")
    }
    else DMY
    if (Zeit) {
        paste(r, if (Zeit == 2) 
            paste(dat[5:6], collapse = ":")
        else dat[5], sep = "; ")
    }
    else r
  }
}
\keyword{ ~keyword }%-- one or more ...
