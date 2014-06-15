\name{is.whole}
\alias{is.whole}
\title{Test Whether a Vector or Array Consists of Whole Numbers}
\description{
  This function tests whether a \code{numeric} or \code{complex} vector
  or array consists of whole numbers.  The function \code{\link{is.integer}}
  is not appropriate for this since it tests whether the vector is of class
  \code{integer} (see examples).
}
\usage{
is.whole(x, tolerance = sqrt(.Machine$double.eps))
}
\arguments{
  \item{x}{\code{integer}, \code{numeric}, or \code{complex} vector or
    array to be tested}
  \item{tolerance}{maximal distance to the next whole number}
}
% \details{
%%  ~~ If necessary, more details than the description above ~~
% }
\value{
  The return value has the same dimension as the argument \code{x}: if \code{x}
  is a vector, the function returns a \code{logical} vector of the same length;
  if \code{x} is a matrix or array, the function returns a \code{logical} matrix
  or array of the same dimensions.  Each entry in the result indicates whether
  the corresponding entry in \code{x} is whole.
}
\author{Alain Hauser <alain@huschhus.ch>}
\seealso{\code{\link{is.integer}}}
\examples{
## Create a random array, matrix, vector
set.seed(307)
a <- array(runif(24), dim = c(2, 3, 4))
a[4:8] <- 4:8
m <- matrix(runif(12), 3, 4)
m[2:4] <- 2:4
v <- complex(real      = seq(0.5, 1.5, by = 0.1),
             imaginary = seq(2.5, 3.5, by = 0.1))

## Find whole entries
is.whole(a)
is.whole(m)
is.whole(v)

## Numbers of class integer are always whole
is.whole(dim(a))
is.whole(length(v))
}
\keyword{arith}
