\name{histBxp}
\encoding{latin1}
\alias{histBxp}
\title{Plot a Histogram and a Boxplot}
\description{
  Creates a histogram and a horizontal boxplot on the current graphics
  device.
}
\usage{
histBxp(x, nclass, breaks, probability=FALSE, include.lowest=TRUE,
         xlab = deparse(substitute(x)),
         \dots,
         width=0.2, boxcol=3, medcol=2, medlwd=5, whisklty=2, staplelty=1)
}
\arguments{
  \item{x}{numeric vector of data for histogram. Missing values
    (\code{NA}s) are allowed.}
  \item{nclass}{
    recommendation for the number of classes (i.e., bars) the histogram should
    have. The default is a number proportional to the logarithm of the length
    of \code{x}.
  }
  \item{breaks}{
    vector of the break points for the bars of the histogram. The count in the
    i-th bar is \code{sum(breaks[i] < x <= breaks[i+1])}
    except that if \code{include.lowest} is \code{TRUE} (the default),
    the first bar also includes points equal to \code{breaks[1]}.  If
    omitted, evenly-spaced break points are determined from
    \code{nclass} and the extremes of the data.
  }
  \item{probability}{
    logical flag: if \code{TRUE}, the histogram will be scaled as a probability
    density; the sum of the bar heights times bar widths will equal \code{1}. If
    \code{FALSE}, the heights of the bars will be counts.
  }
  \item{include.lowest}{
    If \code{TRUE} (the default), the lowest bar will include data
    points equal to the lowest break, otherwise it will act like the
    other bars (see the description of the \code{breaks} argument).
  }
  \item{xlab}{character or expression for x axis labeling.}
  \item{\dots}{additional arguments to \code{\link{barplot}}.  The
    \code{\link{hist}} function uses the function \code{barplot} to do
    the actual plotting; consequently, arguments to the \code{barplot}
    function that control shading, etc., can also be given to
    \code{hist}. See the \code{barplot} documentation for arguments
    \code{angle}, \code{density}, \code{col}, and  \code{inside}. Do not
    use the \code{space} or \code{histo} arguments.
  }
  \item{width}{
    width of the box relative to the height of the histogram. DEFAULT is
    \code{0.2}.}
  \item{boxcol}{color of filled box. The default is \code{3}.}
  \item{medcol}{
    the color of the median line. The special value, \code{NA},
    indicates the current plotting color (\code{par("col")}). The
    default is \code{2}. If \code{boxcol=0} and \code{medcol} is not
    explicitly specified this is set to the current plotting color
    (\code{par("col")}).
  }
  \item{medlwd}{
    median line width.  The special value \code{NA}, is used to indicate
    the current line width (\code{par("lwd")}).  The default is \code{5}.
  }
  \item{whisklty}{
    whisker line type. The special value \code{NA} indicates the
    current line type (\code{par("lty")}). The default is \code{2}
    (dotted line).}
  \item{staplelty}{
    staple (whisker end cap) line type. The special value \code{NA}
    indicates the current line type (\code{par("lty")}). The default is
    \code{1} (solid line).

    Graphical parameters (see \code{\link{par}}) may also
    be supplied as arguments to this function.
    In addition, the high-level graphics arguments described under
    \code{par} and the arguments to \code{title} may be supplied to this
    function.}
}
\details{
  If \code{include.lowest} is \code{FALSE} the bottom breakpoint must be
  strictly less than the minimum of the data, otherwise (the default) it
  must be less than or equal to the minimum of the data.  The top
  breakpoint must be greater than or equal to the maximum of the data.

  This function has been called \code{hist.bxp()} for 17 years; in 2012,
  the increasingly strong CRAN policies required a new name (which could not
  be confused with an S3 method name).
}
\author{S-Plus: Markus Keller, Christian Keller; port to \R: Martin Mächler.
}
\seealso{\code{\link{hist}}, \code{\link{barplot}},
  \code{\link{boxplot}}, \code{\link{rug}} and
  \code{\link[Hmisc]{scat1d}} in the \pkg{Hmisc} package.
}
\examples{
 lab <- "50 samples from a t distribution with 5 d.f."
 mult.fig(2*3, main = "Hist() + Rug()   and    histBxp(*)")
 for(i in 1:3) {
   my.sample <-  rt(50, 5)
   hist(my.sample, main=lab); rug(my.sample)# for 50 obs., this is ok, too..
   histBxp(my.sample, main=lab)
 }
}
\keyword{hplot}
