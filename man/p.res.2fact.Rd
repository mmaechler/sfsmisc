\name{p.res.2fact}
\alias{p.res.2fact}
%%-- FIXME -- copy/paste from ---> ./p.res.2x.Rd
\title{Residual plot vs. 2 Factors Using Boxplots}
\description{

 USES function 'u.boxplot.x' iff values are restricted !
 Arguments: x,y     two vector arguments giving the levels of factors
            z       gives values to be included in boxplots
            restricted      absolute value which truncates the size.
                     The corresponding places are marked by stars.
            notch: T/F   should the boxplots be notched
    xlab, ylab: x- and y- labels for plot. default : from args. x and y

}
\usage{
p.res.2fact(x, y, z, restricted, notch = FALSE,
            xlab = NULL, ylab = NULL, main = NULL)
}
\arguments{
  \item{x}{ ~~Describe \code{x} here~~ }
  \item{y}{ ~~Describe \code{y} here~~ }
  \item{z}{ ~~Describe \code{z} here~~ }
  \item{restricted}{ ~~Describe \code{restricted} here~~ }
  \item{notch}{ ~~Describe \code{notch} here~~ }
  \item{xlab}{ ~~Describe \code{xlab} here~~ }
  \item{ylab}{ ~~Describe \code{ylab} here~~ }
  \item{main}{ ~~Describe \code{main} here~~ }
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
%\references{ ~put references to the literature/web site here ~ }
\author{Lorenz Gygax \email{logyg@wild.unizh.ch} and Martin Maechler, Jan.95;
  starting from \code{\link{p.res.2x}()}.
}
\seealso{\code{\link{p.res.2x}}, \code{\link{boxplot}},
  \code{\link{plot.lm}}, \code{\link{TA.plot}}.
}
\examples{
I <- 8; J <- 3; K <- 20
xx <- factor(rep(rep(1:I, rep(K,I)),J))
yy <- factor(rep(1:J, rep(I*K,J)))
zz <- rt(I*J*K, df=5) #-- Student t with 5 d.f.
p.res.2fact(xx,yy,zz, restr= 4, main= "i.i.d. t <- 5 random  |.| <= 4")
mtext("p.res.2fact(xx,yy,zz, restr= 4, ..)",
      line=2, adj=1, outer=TRUE, cex=1)
rm(xx,yy,zz,I,J,K)
}
\keyword{hplot}
\keyword{regression}
