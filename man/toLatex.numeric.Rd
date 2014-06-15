\name{toLatex.numeric}
\alias{toLatex.numeric}
\title{LaTeX or Sweave friendly Formatting of Numbers}
\description{
  Formats real numbers, possibly in scientific notation, with a given
  number of digits after the decimal point.  Output can be used in LaTeX
  math mode, e.g., for printing numbers in a table, where each number
  has to be printed with the same number of digits after the decimal
  point, even if the last digits are zeros.
}
\usage{
\method{toLatex}{numeric}(object, digits = format.info(object)[2],
        scientific = format.info(object)[3] > 0, times = "\\\\cdot", \dots)
}
\arguments{
  \item{object}{a numeric vector.}
  \item{digits}{number of digits \emph{after the decimal point} (for the
    mantissa if \code{scientific}).  The default behaves the same as \R's
    \code{\link{format}()}.}
  \item{scientific}{logical indicating if scientific notation \code{a *
      10^k} should be used.  The default behaves the same as \R's
    \code{\link{format}()}.}
  \item{times}{character string indicating the LaTeX symbol to be used for
    the ``times'' sign.}
  \item{\dots}{unused; for compatibility with \code{\link{toLatex}}.}
}
\note{We use \code{digits} for \code{\link{round}}, i.e., round after
  the decimal point on purpose, rather than \code{\link{signif}()}icant
  digit rounding as used by \code{\link{print}()} or
  \code{\link{format}()}.
}
\value{
  a \code{\link{character}} vector of the same length as \code{object},
  containing the formatted numbers.
}
\author{
  Alain Hauser% and Martin -- E-mail ? for harvesters
}
\seealso{
  \code{\link{pretty10exp}} which gives \code{\link{expression}}s
  similar to our \code{scientific=TRUE}.
  \code{\link{toLatex}} with other methods.
}
\examples{
xx <- pi * 10^(-9:9)

format(xx)
formatC(xx)

toLatex(xx) #-> scientific = TRUE is chosen
toLatex(xx, scientific=FALSE)

sapply(xx, toLatex)
sapply(xx, toLatex, digits = 2)
}
\keyword{misc}
