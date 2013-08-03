\name{sourceAttach}
\alias{sourceAttach}
\title{Source and Attach an R source file}
\description{
  Source (via \code{\link{sys.source}()}) and attach
  (\code{\link{attach}}) an \R source file.
}
\usage{
sourceAttach(file, pos=2,
             name = paste(abbreviate(gsub(fsep,"", dirname(file)),
                                     12, method="both.sides"),
                          basename(file), sep=fsep),
             keep.source = getOption("keep.source.pkgs"),
             warn.conflicts = TRUE)
}
\arguments{
  \item{file}{file name}
  \item{pos}{passed to \code{\link{attach}()}}
  \item{name}{character, with a smart default, passed to \code{attach()}.}
  \item{keep.source}{logical, see \code{\link{sys.source}()}.}
  \item{warn.conflicts}{logical, see \code{\link{attach}}.}
}
\value{
  the return value of \code{\link{attach}()}.
}
\author{Martin Maechler, 29 Jul 2011}
\seealso{
 \code{\link{attach}}, \code{\link{sys.source}}, \code{\link{source}}
}
\examples{
 sourceAttach(system.file("test-tools-1.R", package="Matrix", mustWork=TRUE))
 search() # shows the new "data base" at position 2
 ## look what it contains:
 ls.str(pos = 2)
}
\keyword{file}
\keyword{utilities}
