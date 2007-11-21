\name{potatoes}
\alias{potatoes}
\docType{data}
\title{Fisher's Potato Crop Data}
\description{
 Fisher's potato crop data set is of historical interest as an early
 example of a multi-factor block design.
}
\usage{data(potatoes)}
\format{
  A data frame with 64 observations on the following 5 variables.
  \describe{
    \item{pos}{a factor with levels \code{1:4}.}
    \item{treat}{a factor with 16 levels \code{A} to \code{H} and
      \code{J} to \code{Q}, i.e., \code{LETTERS[1:17][-9]}.}
    \item{nitrogen}{a factor specifying the amount of nitrogen
      sulfate (\eqn{NH_4}), with the four levels \code{0,1,2,4}.}
    \item{potash}{a factor specifying the amount of potassium (K,
      \sQuote{kalium}) sulfate, with the four levels \code{0,1,2,4}.}
    \item{yield}{a numeric vector giving the yield of potatoes in ...}% << FIXME
  }
}
% \details{
%   FIXME %%  more details than the __description__ above ~~
% }  %% ----
\source{
  Bennett, J. H. (1972)
  \emph{Collected Papers of R. A. Fischer} vol.~II, 1925-31;
  The University of Adelaide.
  %% One of the blocks, in the book
  %% Stahel "Statist. Datenanalyse" Beisp.Kartoffelertrag (2nd ed. 251 a, 253 h)
}
\references{
  T.Eden and R. A. Fisher (1929)
  Studies in Crop Variation. VI. Experiments on the Response of the
  Potato to Potash and Nitrogen.
  \emph{J. Agricultural Science} \bold{19}, 201--213.
  Accessible from Bennett (1972), see above.
}
\examples{
data(potatoes)
## See the experimental design:
with(potatoes, {
     cat("4 blocks of experiments;",
         "each does every (nitrogen,potash) combination (aka 'treat'ment) once.",
         '', sep="\n")
     print(ftable(table(nitrogen, potash, treat)))
     print(ftable(tt <- table(pos,potash,nitrogen)))
     tt[cbind(pos,potash,nitrogen)] <- as.character(treat)
     cat("The 4 blocks  pos = 1, 2, 3, 4:\n")
     ftable(tt)
     })
## First plot:
with(potatoes, interaction.plot(potash,nitrogen, response=yield))

## ANOVAs:
summary(aov(yield ~ nitrogen * potash + Error(pos), data = potatoes))
    # "==>" can use simply
summary(aov(yield ~ nitrogen + potash + pos, data = potatoes))
    # and
summary(aov(yield ~ nitrogen + potash, data = potatoes))
}
\keyword{datasets}
