\name{eaxis}
\alias{eaxis}
\title{Extended / Engineering Axis for Graphics}
\description{
  An extended \code{\link[graphics]{axis}()} function which labels more
  prettily, in particular for log-scale axes.

  It  makes use of \link{plotmath} or (LaTeX) \code{\link{expression}}s of
  the form \eqn{k \times 10^k}{k * 10^k} for labeling a
  log-scaled axis and when otherwise exponential formatting would be
  used (see \code{\link{pretty10exp}}).
}
\usage{
eaxis(side, at = if(log) axTicks(side, axp=axp, log=log, nintLog=nintLog)
                 else    axTicks(side, axp=axp, log=log),
      labels = NULL, log = NULL,
      f.smalltcl = 3/5, at.small = NULL, small.mult = NULL,
      small.args = list(),
      draw.between.ticks = TRUE, between.max = 4,
      outer.at = TRUE, drop.1 = TRUE, sub10 = FALSE, las = 1,
      nintLog = max(10, par("lab")[2 - is.x]),
      axp = NULL, n.axp = NULL, max.at = Inf,
      lab.type = "plotmath", lab.sep = "cdot",
      \dots)
}
\arguments{
  \item{side}{integer in 1:4, specifying side of \code{\link{axis}}.}
  \item{at}{numeric vector of (\dQuote{normalsized}) tick locations; by
    default \code{\link[graphics]{axTicks}(side, ..)}, i.e., the same as
    \code{\link{axis}()} would use.}
  \item{labels}{\code{NULL} (default), \code{\link{logical}},
    \code{character} or \code{expression}, as in \code{\link{axis}()};
    in addition, if \code{NA}, \code{labels = TRUE} is passed to
    \code{\link{axis}()}, i.e. \code{\link{pretty10exp}} is \emph{not}
    used.  Use \code{FALSE} to suppress any labeling.}
  \item{log}{logical or \code{NULL} specifying if log-scale should be
    used; the default depends on the current plot's axis.}
  \item{f.smalltcl}{factor specifying the lengths of the small ticks in
    proportion to the normalsized, labeled ticks.}
  \item{at.small}{locations of \emph{small} ticks; the default,
    \code{NULL}, uses \code{small.mult} and constructs \dQuote{smart}
    locations.}
  \item{small.mult}{positive integer (or \code{NULL}), used when
    \code{at.small} is NULL to indicate which multiples of
    \code{at} (typically \code{\link{axTicks}()}) should be used as
    \dQuote{small ticks}.  The default \code{NULL} will use \code{9} in
    the log case and a number in 2:5 otherwise.}% depending on scale details
  \item{small.args}{optional \code{\link{list}} of further arguments to
    the \code{\link{axis}()} call which draws the small ticks.}
  \item{draw.between.ticks}{(only if \code{log} is true): logical indicating
    that possible (non-small) ticks between the labeled (via \code{at})
    ones should be drawn as well (and possibly also used for \code{at.small}
    construction), see also \code{between.max}.}
  \item{between.max}{(only if \code{log} and \code{draw.between.ticks}
    are true): integer indicating ticks should be drawn (approximately)
    between the labeled ones.}
  \item{outer.at}{logical specifying that \code{at.small} should also be
    constructed outside the \code{at} range, but still inside the
    corresponding \code{\link{par}("usr")}.}
  \item{drop.1}{logical specifying if \eqn{1 \times}{1 *} should be dropped
    from labels, passed to \code{\link{pretty10exp}()}.}
  \item{sub10}{logical, integer (of length 1 or 2) or \code{"10"}, indicating if
    some \eqn{10^k} should be simplified to \dQuote{traditional}
    formats, see \code{\link{pretty10exp}}.}
  \item{nintLog}{only used in \R > 2.13.x, when \code{log} is true:
    approximate (lower bound on) number of intervals for log scaling.}
  \item{axp}{to be passed to \code{\link{axTicks}()} if \code{at} is not
    specified.}
  \item{n.axp}{to be set to \code{axp[3]} when \code{axp} and \code{at}
    are not specified, in order to tweak the \emph{number} of (non-small)
    tick marks produced from \code{\link{axTicks}(..)}, notably when
    \code{log} is true, set \code{n.axp} to 1, 2, or 3:
    \describe{
        \item{1:}{will produce tick marks at \eqn{10^j} for integer \eqn{j},}
        \item{2:}{gives marks  \eqn{k 10^j} with \eqn{k \in \{1, 5\}}{k in {1,5}},}
        \item{3:}{gives marks  \eqn{k 10^j} with \eqn{k \in \{1, 2, 5\}}{k in {1,2,5}}}
      } see \code{'xaxp'} on the \code{\link{par}} help page.}
  \item{max.at}{maximal number of \code{at} values to be used
    effectively.  If you don't specify \code{at} yourself carefully, it
    is recommended to set this to something like \code{25}, but this is
    not the default, for back compatibility reasons.}
  \item{las, \dots}{arguments passed to (the first) \code{\link{axis}}
    call.  Note that the default \code{las = 1} differs from
    \code{axis}'s default \code{las = 0}.}
  \item{lab.type}{string, passed to \code{\link{pretty10exp}} to choose
    between default \code{\link{plotmath}} or LaTeX label format.}
  \item{lab.sep}{separator between mantissa and exponent for LaTeX labels,
    see \code{\link{pretty10exp}}.}
}
\author{Martin Maechler}
\seealso{\code{\link[graphics]{axis}},
  \code{\link[graphics]{axTicks}},
  \code{\link{axTexpr}},
  \code{\link{pretty10exp}}.
}
\examples{
x <- lseq(1e-10, 0.1, length = 201)
plot(x, pt(x, df=3), type = "l", xaxt = "n", log = "x")
eaxis(1)
## without small ticks:
eaxis(3, at.small=FALSE, col="blue")

## If you like the ticks, but prefer traditional (non-"plotmath") labels:
plot(x,  gamma(x),   type = "l", log = "x")
eaxis(1, labels=NA)

x <- lseq(.001, 0.1, length = 1000)
plot(x, sin(1/x)*x, type = "l", xaxt = "n", log = "x")
eaxis(1)
eaxis(3, n.axp = 1)# -> xaxp[3] = 1:  only  10^j (main) ticks

## non- log-scale : draw small ticks, but no "10^k" if not needed:
x <- seq(-100, 100, length = 1000)
plot(x, sin(x)/x, type = "l", xaxt = "n")
eaxis(1)           # default -> {1, 2, 5} * 10^j  ticks
eaxis(3, n.axp = 2)# -> xaxp[3] := 2 -- approximately two (main) ticks

x <- seq(-1, 1, length = 1000)
plot(x, sin(x)/x, type = "l", xaxt = "n")
eaxis(1, small.args = list(col="blue"))

x <- x/1000
plot(x, 1-sin(x)/x, type = "l", xaxt = "n", yaxt = "n")
eaxis(1)
eaxis(2)
## more labels than default:
op <- par(lab=c(10,5,7))
plot(x, sin(x)/x, type = "l", xaxt = "n")
eaxis(1) # maybe (depending on your canvas), there are too many,
## in that case, maybe use
plot(x, sin(x)/x, type = "l", xaxt = "n")
eaxis(1, axTicks(1)[c(TRUE,FALSE)]) # drop every 2nd label
eaxis(3, labels=FALSE)

## ore use 'max.at' which thins as well:
plot(x, sin(x)/x, type = "l", xaxt = "n")
eaxis(1, max.at=6)
par(op)

### Answering R-help "How do I show real values on a log10 histogram", 26 Mar 2013
## the data:
       set.seed(1); summary(x <- rlnorm(100, m = 2, sdl = 3))
## the plot (w/o  x-axis) :
       r <- hist(log10(x), xaxt = "n", xlab = "x [log scale]")
## the nice axis:
       axt <- axTicks(1)
       eaxis(1, at = axt, labels = pretty10exp(10^axt, drop.1=TRUE))
## Additionally demo'ing   'sub10' options:
       plot(r, xaxt="n")
       eaxis(1, at = axt, labels = pretty10exp(10^axt, drop.1=TRUE, sub10 = 2))
## or
       plot(r, xaxt="n")
       eaxis(1, at = axt, labels = pretty10exp(10^axt, drop.1=TRUE, sub10 = "10"))
## or
       plot(r, xaxt="n")
       eaxis(1, at = axt, labels = pretty10exp(10^axt, drop.1=TRUE, sub10 = c(-2, 2)))
}
\keyword{aplot}
