\name{seqXtend}
\alias{seqXtend}
\title{Sequence Covering the Range of X, including X}
\description{
  Produce a sequence of unique values (sorted increasingly),
  \emph{containing} the initial set of values \code{x}.
  This can be useful for setting prediction e.g. ranges in nonparametric
  regression.
}
\usage{
seqXtend(x, length., method = c("simple", "aim", "interpolate"),
        from = NULL, to = NULL)
}
\arguments{
  \item{x}{numeric vector.}
  \item{length.}{integer specifying \emph{approximately} the desired
    \code{\link{length}()} of the result.}
  \item{method}{string specifying the method to be used.  The default,
    \code{"simple"} uses \code{\link{seq}(*, length.out = length.)} where
      \code{"aim"} aims a bit better towards the desired final length,
      and \code{"interpolate"} interpolates evenly \emph{inside}
      each interval \eqn{[x_i, x_{i+1}]}{(x[i], x[i+1])} in a way to
      make all the new intervalls of approximately the same length.}
  \item{from, to}{numbers to be passed to (the default method for)
    \code{\link{seq}()}, defaulting to the minimal and maximal \code{x}
    value, respectively.}
}
\note{
  \code{method = "interpolate"} typically gives the best results. Calling
  \code{\link{roundfixS}}, it also need more computational resources
  than the other methods.
}
\value{
  numeric vector of increasing values, of approximate length
  \code{length.}
  (unless \code{length. < length(unique(x))} in which case, the result
  is simply \code{sort(\link{unique}(x))}),
  containing the original values of \code{x}.

  From, \code{r <- seqXtend(x, *)}, the original values are at
  indices \code{ix <- match(x,r)}, i.e., \code{identical(x, r[ix])}.
}
\author{Martin Maechler}
\seealso{\code{\link{seq}}; \code{\link{plotDS}} can make particularly
  good use of \code{seqXtend()}
}
\examples{
a <- c(1,2,10,12)
seqXtend(a, 12)# --> simply 1:12
seqXtend(a, 12, "interp")# ditto
seqXtend(a, 12, "aim")# really worse
stopifnot(all.equal(seqXtend(a, 12, "interp"), 1:12))

## for a "general" x, however, "aim" aims better than default
x <- c(1.2, 2.4, 4.6, 9.9)
length(print(seqXtend(x, 12)))        # 14
length(print(seqXtend(x, 12, "aim"))) # 12
length(print(seqXtend(x, 12, "int"))) # 12

## "interpolate" is really nice:
xt <- seqXtend(x, 100, "interp")
plot(xt, main="seqXtend(*, 100, \"interpol\")")
points(match(x,xt), x, col = 2, pch = 20)
# .... you don't even see that it's not equidistant
# whereas the cheap method shows ...
xt2 <- seqXtend(x, 100)
plot(xt2, col="blue")
points(match(x,xt2), x, col = 2, pch = 20)

## with "Date" objects
Drng <- as.Date(c("2007-11-10", "2012-07-12"))
(px <- pretty(Drng, n = 16)) # say, for the main labels
## say, a finer grid, for ticks -- should be almost equidistant
n3 <- 3*length(px)
summary(as.numeric(diff(seqXtend(px, n3))))        # wildly varying
summary(as.numeric(diff(seqXtend(px, n3, "aim")))) #   (ditto)
summary(as.numeric(diff(seqXtend(px, n3, "int")))) # around 30
}
\keyword{manip}
\keyword{utilities}
