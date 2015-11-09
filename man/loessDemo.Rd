\name{loessDemo}
\title{Graphical Interactive Demo of loess()}
\alias{loessDemo}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
loessDemo(x, y, span = 1/2, degree = 1, nearest = FALSE,
          xlim = numeric(0), ylim = numeric(0), strictlim = TRUE, verbose = TRUE,
          nout = 501, family = c("gaussian", "symmetric"),
          inch.sym = 0.25, pch = 4, shade = TRUE, w.symbols = TRUE,
          sym.col = "blue", w.col = "light blue", line.col = "steelblue")
}
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{span}{
%%     ~~Describe \code{span} here~~
}
  \item{degree}{
%%     ~~Describe \code{degree} here~~
}
  \item{nearest}{
%%     ~~Describe \code{nearest} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{strictlim}{
%%     ~~Describe \code{strictlim} here~~
}
  \item{verbose}{
%%     ~~Describe \code{verbose} here~~
}
  \item{nout}{
%%     ~~Describe \code{nout} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{inch.sym}{
%%     ~~Describe \code{inch.sym} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{shade}{
%%     ~~Describe \code{shade} here~~
}
  \item{w.symbols}{
%%     ~~Describe \code{w.symbols} here~~
}
  \item{sym.col, w.col, line.col}{colors for the symbols, weights and
    lines, respectively.}
}
%% \details{
%% %%  ~~ If necessary, more details than the description above ~~
%% }
%% \value{
%% }
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
  As function \code{loess.demo()},  written and posted to S-news, on 27
  Sep 2001, by Greg Snow, Brigham Young University, % gls@byu.edu
  it was modified by Henrik Aa. Nielsen, IMM, DTU, % han@imm.dtu.dk
  and subsequently spiffed up for \R by Martin Maechler.
}
\seealso{
  \code{\link{loess}}.
}
\examples{
}
\keyword{loess}
\keyword{dynamic}
\keyword{hplot}
