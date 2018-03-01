\name{wrapFormula}
\alias{wrapFormula}
\title{Enhance Formula by Wrapping each Term, e.g., by "s(.)"}
\description{
  The main motivation for this function has been the easy construction
  of a \dQuote{full GAM formula} from something as simple as
  \code{Y ~ .}. \cr
  The potential use is slightly more general.
}
\usage{
wrapFormula(f, data, wrapString = "s(*)")
}
\arguments{
  \item{f}{the initial \code{\link{formula}}; typically something like
    \code{Y ~ .}.}
  \item{data}{\code{\link{data.frame}} to which the formula applies;
    see, \code{\link{formula}} or also \code{\link[mgcv]{gam}} or
    \code{\link{lm}}.}
  \item{wrapString}{\code{\link{character}} string, containing
    \code{"*"}, specifying the wrapping expression to use.}
}
\value{
  a \code{\link{formula}} very similar to \code{f}; just replacing each
  \emph{additive} term by its wrapped version.
}
\note{
  There are limits for this to work correctly; notably the right hand
  side of the formula \code{f} should not be nested or otherwise
  complicated, rather typically just \code{ . } as in the examples.
}
\author{Martin Maechler, May 2007.}
\seealso{\code{\link{formula}};
  \code{\link[mgcv]{gam}} from package \CRANpkg{mgcv} (or also from
  package \CRANpkg{gam}).
}
\examples{
myF <- wrapFormula(Fertility ~ . , data = swiss)
myF # Fertility ~ s(Agriculture) + s(....) + ...

if(require("mgcv")) {
   m1 <- gam(myF, data = swiss)
   print( summary(m1) )
   plot(m1, pages = 1) ; title(format(m1$call), line= 2.5)
}

## other wrappers:
wrapFormula(Fertility ~ . , data = swiss, wrap = "lo(*)")
wrapFormula(Fertility ~ . , data = swiss, wrap = "poly(*, 4)")
}
\keyword{models}
