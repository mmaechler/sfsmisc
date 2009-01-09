\name{u.log}
\alias{u.log}
\title{(Anti)Symmetric Log High-Transform}
\description{
  Compute \eqn{log()} only for high values and keep low ones --
  antisymmetrically such that \code{u.log(x)} is (once) continuously
  differentiable, it computes
%Fails \deqn{f(x) = \left\{\begin{array}{ll}
%Fails    x	 & for |x| \leq c \\
%Fails    sign(x) c (1 + \log(|x|/c)) & for |x| > c \end{array}}{%
%Fails    f(x) = x  for |x| <= c  and  sign(x)*c*(1 + log(|x|/c)) for |x| >= c.}
  \eqn{f(x) = x} for \eqn{|x| \le c}{|x| <= c}  and
  \eqn{sign(x)  c\cdot(1 + log(|x|/c))}{sign(x)*c*(1 + log(|x|/c))}
  for \eqn{|x| \ge c}{|x| >= c}.
}
\usage{
u.log(x, c = 1)
}
\arguments{
  \item{x}{numeric vector to be transformed.}
  \item{c}{scalar, > 0}
}
\value{
  numeric vector of same length as \code{x}.
}
\author{Martin Maechler, 24 Jan 1995}
%\seealso{ ~~objects to SEE ALSO as \code{\link{~~fun~~}}, ~~~ }
\examples{
curve(u.log, -3, 10); abline(h=0, v=0, col = "gray20", lty = 3)
curve(1 + log(x), .01,  add = TRUE, col= "brown") # simple log
curve(u.log(x,    2),   add = TRUE, col=2)
curve(u.log(x, c= 0.4), add = TRUE, col=4)
}
\keyword{arith}
