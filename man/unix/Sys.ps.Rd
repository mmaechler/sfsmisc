\name{Sys.ps}
\alias{Sys.ps}
\alias{Sys.sizes}
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Sys.ps(process=Sys.PID(), fields=c("pid", "%mem", "%cpu", "cputime", "time", "sz", "rsz", "tsiz", , usefile=length(fields) > 10, verbose=getOption("verbose"))

Sys.sizes(process=Sys.PID())
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{process}{ ~~Describe \code{process} here~~ }
  \item{fields}{ ~~Describe \code{fields} here~~ }
  \item{usefile}{ ~~Describe \code{usefile} here~~ }
  \item{verbose}{ ~~Describe \code{verbose} here~~ }
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
function (process = Sys.PID(), fields = c("pid", "%mem", "%cpu", 
    "cputime", "time", "sz", "rsz", "tsiz", "rssize", "vsize", 
    "user", "comm"), usefile = length(fields) > 10, verbose = getOption("verbose")) 
{
    ps.opt <- {
        if (is.numeric(process) && process == round(process)) 
            paste("-p", process)
        else if (process == "ALL") 
            "-e"
        else if (is.character(process) && length(process) == 
            1) 
            paste("-C", process)
        else stop(paste("invalid `process':", format(process)))
    }
    i.field <- pmatch(fields, .Sys.ps.fields)
    if (any(ina <- is.na(i.field))) {
        warning(paste("Dropping invalid field names", fields[ina]))
        i.field <- i.field[!ina]
    }
    fields <- .Sys.ps.fields[i.field]
    imult <- !is.na(match(fields, .Sys.ps.multifields))
    if (any(imult) && length(fields) > 1) {
        warning(paste("Not using `multi fields' ", paste(fields[imult], 
            collapse = ",")))
        fields <- fields[!imult]
        imult <- FALSE
    }
    if (usefile) 
        ofile <- tempfile("R.Sys.ps")
    cmd <- paste("ps --width 1000", ps.opt, "-o", paste(fields, 
        collapse = ","), if (usefile) 
        paste(" >", ofile))
    if (verbose) 
        cat("Now calling\n\t", cmd, "\n")
    lines <- system(cmd, intern = !usefile)
    if (usefile) {
        if (lines) 
            warning(paste("system() returned non-0 :", lines))
        lines <- scan(ofile, what = "", sep = "\n", quiet = TRUE)
    }
    if (length(lines) <= 1) 
        stop(paste("call returned less than two lines:", lines, 
            sep = "\n"))
    r <- sub("^ ", "", gsub("[ \t]+", " ", lines))
    if (length(fields) == 1) {
        if (length(r) == 2) 
            return(structure(r[2], names = fields))
        else warning(paste("Funny result with one `fields': length(r)=", 
            length(r)))
    }
    ll <- strsplit(r, " ")
    d.len <- diff(lenl <- sapply(ll, length))
    if (lenl[1] == length(fields)) 
        ll[[1]] <- fields
    else warning(paste("Number returned headers =", lenl[1], 
        " != ", "#{fields} =", length(fields)))
    if (d.len) {
        warning(paste("Lengths differ:", paste(lenl, collapse = ",")))
    }
    r <- c(ll[[2]], rep(NA, max(0, -d.len)))
    names(r) <- if (d.len > 0) 
        c(ll[[1]], rep(".x.", d.len))
    else ll[[1]][1:lenl[2]]
    r
  }
}
\keyword{ ~keyword }%-- one or more ...
