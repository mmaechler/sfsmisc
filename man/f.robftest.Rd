\name{f.robftest}
\alias{f.robftest}
\title{Robust F-Test: Wald test for multiple coefficients of rlm() Object.}
\description{
  Compute a robust F-Test, i.e., a Wald test for multiple coefficients
  of an \code{\link[MASS]{rlm}} object.
}
\usage{
f.robftest(object, var = -1)
}
\arguments{
  \item{object}{result of \code{\link[MASS]{rlm}()}.}
  \item{var}{variables.  Either their names or their indices;  the
    default, \code{-1} means all \emph{but} the intercept.}
}
\details{
  This builds heavily on \code{\link[MASS]{summary.rlm}()}, the
  \code{\link{summary}} method for \code{\link[MASS]{rlm}} results.
}
\value{
  An object of class \code{"htest"}, hence with the standard print
  methods for hypothesis tests.  This is basically a list with components
  \item{statistic}{the F statistic, according to ...}% FIXME
  \item{df}{numerator and denominator degrees of freedom.}
  \item{data.name}{(extracted from input \code{object}.)}
  \item{alternative}{\code{"two.sided"}, always.}
  \item{p.value}{the P-value, using an F-test on \code{statistic} and
    \code{df[1:2]}.}
}
\references{
  FIXME --- Need some here !
}
\author{Werner Stahel, Jul.2000; updates by Martin Maechler.}
\seealso{\code{\link[MASS]{rlm}}, \code{\link{summary.aov}}, etc.}
\examples{
if(require("MASS")) {
  ## same data as  example(rlm)
  data(stackloss)
  summary(rsl <- rlm(stack.loss ~ ., stackloss))
  f.robftest(rsl)
 } else  " forget it "

}
\keyword{robust}
\keyword{htest}

