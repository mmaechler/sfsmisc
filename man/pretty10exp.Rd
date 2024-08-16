\name{pretty10exp}
\alias{pretty10exp}
\title{Nice  10 ** k  Label Expressions}
\description{
  Produce nice \eqn{a \times 10^k}{a * 10^k} expressions to be used
  instead of the scientific notation  \code{"a E<k>"}.
}
\usage{
pretty10exp(x, drop.1 = FALSE, sub10 = FALSE, digits = 7, digits.fuzz,
            off = pmax(10^-digits, 2^-(l10x*log2(10)+1075)),
            lab.type = c("plotmath","latex"),
            lab.sep = c("cdot", "times"))
}
\arguments{
  \item{x}{numeric vector (e.g. axis tick locations)}
  \item{drop.1}{logical indicating if \eqn{1 \times}{1 *} should be
    dropped from the resulting expressions.}
  \item{sub10}{logical, \code{"10"}, a non-negative integer number or
    an integer vector of length two, say \eqn{(k_1,k_2)}{(k1,k2)}, indicating if some
    \eqn{10^j} expressions for \eqn{j \in J}{j in J} should be formatted
    traditionally, notably e.g., \eqn{10^0 \equiv 1}{10^0 == 1}.
    \cr
    When a (non-negative) number, say \eqn{k}, \eqn{J = \{j; j \le k\}}{%
      J = {j; j \le k}} are all simplified, when a length--2 vector,
    \eqn{J = \{j; k_1 \le j \le k_2\}}{J = {j; k1 \le j \le k2}} are.

    Special cases: \code{sub10 = TRUE} means to use
    \eqn{1} instead of \eqn{10^0} and \code{sub10 = "10"} uses both
    \eqn{1} for \eqn{10^0} and \eqn{10} for \eqn{10^1}; these are short
    forms of \code{sub10 = c(0,0)} and \code{sub10 = c(0,1)} respectively.
  }
  \item{digits}{number of digits for mantissa (\eqn{a}) construction;
    the number of \emph{significant} digits, see \code{\link{signif}}.}
  \item{digits.fuzz}{the old deprecated name for \code{digits}.}
  \item{off}{a numeric offset in \code{eT <- floor(l10x + off)} where
    \code{l10x <- log10(abs(x))} and \code{eT} are the exponents \eqn{k}
    for label factors \eqn{10^k}.  Previously hardcoded to
    \code{10^-digits}, the new default provides better results for
    subnormal \code{abs(x)} values.}
  \item{lab.type}{a string indicating how the result should look like.
    By default, (\code{\link{plotmath}}-compatible)
    \code{\link{expression}}s are returned. Alternatively,
    \code{lab.type = "plotmath"} returns LaTeX formatted strings for
    labels.  (The latter is useful, e.g., when using the \CRANpkg{tikzDevice}
    package to generate LaTeX-processed figures.)}
  \item{lab.sep}{character separator between mantissa and exponent for
    LaTeX labels; it will be prepended with a backslash,
    i.e., \sQuote{"cdot"} will use \sQuote{"\\cdot"}}
}
\value{
  For the default \code{lab.type = "plotmath"},
  an expression of the same length as \code{x}, typically with elements
  of the form \code{a \%*\% 10 ^ k}.
  Exceptions are \code{0} which is kept simple, if \code{drop.1} is
  true and \eqn{a = 1}, \code{10 ^ k} is used, and if \code{sub10}
  is not false, \code{a \%*\% 10 ^ 0} as \code{a}, and \code{a \%*\% 10 ^ k} as
  as the corresponding formatted number \code{a * 10^k} independently of
  \code{drop.1}.

  Otherwise, a \code{\link{character}} vector of the same length as
  \code{x}.  For \code{lab.type = "latex"}, currently the only
  alternative to the default, these strings are LaTeX (math mode)
  compatible strings.
}
\note{
  If \code{sub10} is set, it will typically be a small number such as 0,
  1, or 2.  Setting \code{sub10 = TRUE} will be interpreted as
  \code{sub10 =1} where resulting exponents \eqn{k} will either be
  negative or \eqn{k \ge 2}{k >= 2}.
}
\author{Martin Maechler;  Ben Bolker contributed \code{lab.type = "latex"}
  and \code{lab.sep}.}
\seealso{\code{\link{axTexpr}} and \code{\link{eaxis}()} which build on
  \code{pretty10exp()}, notably the \code{eaxis()} example plots.

  The new \code{\link{toLatex.numeric}} method which gives very similar
  results with option \code{scientific = TRUE}.
  \cr Further, \code{\link{axis}}, \code{\link{axTicks}}.
}
\examples{
pretty10exp(-1:3 * 1000)
pretty10exp(-1:3 * 1000, drop.1 = TRUE)
pretty10exp(c(1,2,5,10,20,50,100,200) * 1e3)
pretty10exp(c(1,2,5,10,20,50,100,200) * 1e3, drop.1 = TRUE)

set.seed(17); lx <- rlnorm(10, m=8, s=6)
pretty10exp(lx, digits = 3)
pretty10exp(lx, digits = 3, sub10 = 2)

pretty10exp(lx, digits = 3, lab.type="latex")
pretty10exp(lx, digits = 3, lab.type="latex", lab.sep="times", sub10=2)

## use regular formatted numbers from 0.03 to 300 :
pretty10exp(3*10^(-3:4), sub10 = c(-2,2))
pretty10exp(3*10^(-3:4), sub10 = c(-2,2), lab.type = "l")

\dontshow{
stopifnot(identical(pretty10exp(numeric(0)), expression()))
}
ax <- 10^(-6:0) - 2e-16
pretty10exp(ax, drop.1=TRUE) # nice for plotting
pretty10exp(ax, drop.1=TRUE, sub10=TRUE)
pretty10exp(ax, drop.1=TRUE, sub10=c(-2,2))

## in sfsmisc version <= 1.0-16, no 'digits',
## i.e., implicitly had  digits := #{double precision digits} ==
(dig. <- .Machine$double.digits * log10(2)) # 15.95
pretty10exp(ax, drop.1=TRUE, digits= dig.)  # ''ugly''

## Subnormal numbers
x <- sort(c(outer(10^-(323:305), 1:9))); x <- c(x[1]/2, x)
tail(x, 12) # nice
head(x, 6)  # "ugly" (they are multiple's of 2^-1074):
head(x, 6) / 2^-1074  # nice

head(p0 <- pretty10exp(x, off = 10^-7), 30) # previous behavior {before 'off' existed}
str(head(pTen <- lapply(p0, `[[`, 3L)))
str(exTen <- sapply(pTen, `[[`, 3L)) # -324 -324 ..
head(f0 <- sapply(p0, `[[`, 2L), 17)

head(p1 <- pretty10exp(x))# new default
str(head(pTen1 <- lapply(p1, `[[`, 3L)))
str(exTen1 <- sapply(pTen1, `[[`, 3L)) # -324 -324 ..
head(f1 <- sapply(p1, `[[`, 2L), 17)   #

head(cbind(x, f0, f1, exTen, exTen1), 80)
(nEQ <- which(sapply(1:length(p0), function(i) p0[[i]] != p1[[i]])))
cbind(x, f0, f1, exTen, exTen1)[nEQ,]
stopifnot(is.finite(f1), 0.5 <= f1, f1 <= 9)
}
\keyword{dplot}
