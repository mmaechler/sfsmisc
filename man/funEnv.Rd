\name{funEnv}
\alias{funEnv}
\title{List-like Environment of Functions (and More)}
\description{
  Construct a \dQuote{list}, really an \code{\link{environment}}
  typically of functions and optionally other \R objects, where the
  \code{\link{function}}s and \code{\link{formula}}s all
  all share the same environment.
  Consequently, the functions may call each other.

  On technical level, this is just a simple wrapper around
  \code{\link{list2env}()}.
}
\usage{
funEnv(..., envir = NULL, parent = parent.frame(),
       hash = (...length() > 100), size = max(29L, ...length()))
}
\arguments{
  \item{...}{an arbitrary \emph{named} \dQuote{list} of \R objects,
    typically including several \code{\link{function}}s.}
  \item{envir}{an \code{\link{environment}} or \code{NULL}.}
  \item{parent}{(for the case \code{envir = NULL}): a parent frame aka
    enclosing environment, see \code{\link{new.env}} and \code{\link{list2env}}.}
  \item{hash, size}{(for the case \code{envir = NULL}): \code{hash} a
    logical indicating if the created environment should use hashing,
    and (\code{size}) the hash size, see \code{\link{list2env}}.}
}
\value{
  an \code{\link{environment}}, say \code{E}, containing the objects
  from \code{...} (plus those in \code{envir}), and all function
  objects' \code{\link{environment}()} is E.
}
%% \references{
%% }
\author{Martin Maechler}

\seealso{
  \code{\link{list2env}}, \code{\link{environment}}
}
\examples{
ee <- funEnv(f = function(x) g(2*(x+1)),
             g = function(y) hh(y+1),
            hh = function(u) u^2,
          info = "Some Information (not a function)")
ls(ee) # here the same as  names(ee)
## Check that it works: i.e., that "f sees g" and "g sees hh":
stopifnot(all.equal(ee$f(pi), (2*pi+3)^2))
ee$f(0:4) # [1]  9  25  49  81 121
}
\keyword{programming}
\keyword{data}
