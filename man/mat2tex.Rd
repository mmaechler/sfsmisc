%%-- Original from Vincent Carey, see (commented) E-mail at end !
\name{mat2tex}
\alias{mat2tex}
\title{Produce LaTeX commands to print a matrix}
\usage{
mat2tex(x, file= "mat.tex", nam.center = "l", col.center = "c",
        append = TRUE, digits = 3, title)
}
\arguments{
  \item{x}{a matrix}
  \item{file}{names the file to which LaTeX commands should be written}
  \item{nam.center}{character specifying row names should be center;
    default \code{"l"}.}
  \item{col.center}{character (vector) specifying how the columns should
    be centered; must have values from \code{c("l","c","r")}; defaults
    to \code{"c"}.}
  \item{append}{logical; if \code{FALSE}, will destroy the file
    \code{file} before writing commands to it; otherwise (by default),
    simply adds commands at the end of file \code{file}.}
  \item{digits}{integer; setting of \code{\link{options}(digits=..)} for
    purpose of number representation.}
  \item{title}{a string, possibly using LaTeX commands, which will span
    the columns of the LaTeX matrix}
}
\description{
  \dQuote{Translate} an \R matrix (like object) into a LaTeX table,
  using \code{\\begin{tabular} ...}.
}
\value{
  No value is returned.  This function, when used correctly,
  only writes LaTeX commands to a file.
}
\author{For S: Vincent Carey \email{vjcarey@sphunix.sph.jhu.edu}, from a
  post on Feb.19, 1991 to S-news.  Port to \R (and a bit more) by
  Martin Maechler \email{maechler@stat.math.ethz.ch}.
}
\seealso{
  \code{\link[Hmisc]{latex}} in package \pkg{Hmisc} is more flexible
  (but may surprise by its auto-printing ..).
}
\examples{
mex <- matrix(c(pi,pi/2,pi/4,exp(1),exp(2),exp(3)),nrow=2, byrow=TRUE,
               dimnames = list(c("$\\\\pi$","$e$"),c("a","b","c")))
mat2tex( mex, title="$\\\\pi, e$, etc." )% double-esc -> 4 x backslash

## The last command produces the file "mat.tex" containing

##>  \\begin{tabular} {| l|| c| c| c|}
##>  \\multicolumn{ 4 }{c}{ $\\pi, e$, etc. } \\\\ \\hline
##>     \\  & a & b & c \\\\ \\hline \\hline
##>  $\pi$ & 3.14 & 1.57 & 0.785 \\\\ \\hline
##>  $e$ & 2.72 & 7.39 & 20.1 \\\\ \\hline
##>  \\end{tabular}

## Now you have to properly embed the contents of this file
## in a LaTeX document -- for example, you will need a
## preamble, the \\begin{document} statement, etc.

## Note that the backslash needs protection in dimnames
## or title actions.

mat2tex(mex, stdout(), col.center = c("r","r","c"))
}
\keyword{interface} % to latex
\keyword{utilities}
%%
%% From: vjcarey@sphunix.sph.jhu.edu (Vincent J. Carey)
%% Subject: mat2tex, a report generation aid (LONG)
%% To: s-news@stat.wisc.edu
%% Date: Tue, 19 Feb 91 19:25:05 EST
%%
%% Has there been any work on "report generation" from S
%% beyond the tbl() function?  I have been manually
%% transcribing S matrix elements to LaTeX tables
%% and find this to be a tedious and error-prone process.
%% Certainly there are ways of cutting and pasting window
%% contents to reduce the troubles associated with digit
%% keying, but this is far from foolproof, and is also
%% pretty dull.
%%
%% Therefore I offer this function, mat2tex(), which
%% produces LaTeX commands to format the entries of
%% an S matrix.  The function will, by default, place
%% the commands in the file "mat.tex".
%%
%% The documentation file included is fairly explicit. The
%% function does not produce a complete LaTeX program, but
%% a fragment "containing" a matrix.  Column and row labels
%% (which may include references to special LaTeX symbols,
%% provided backslash protection is maintained) are propagated
%% to the LaTeX fragment.  There is a provision for a matrix "title".
%%
%% Unadulterated output of a simple example (see the doc page
%% for the input):
%%
%% \begin{tabular} {| l|| c| c| c|}
%% \multicolumn{ 4 }{c}{ $\pi, e$, etc. } \\ \hline
%%    \  & a & b & c \\ \hline \hline
%% $\pi$ & 3.14 & 1.57 & 0.785 \\ \hline
%% $e$ & 2.72 & 7.39 & 20.1 \\ \hline
%% \end{tabular}
%%
%% Further work needed: digit/decimal-point alignment,
%% consistent boxing of the title, more arguments to allow
%% sensible customization -- e.g., the user may not desire
%% \hline between rows.  Comments, criticism and enhancements
%% are welcome.
%%
%% -------------------------------------------------------------------------
%%
%% Vincent J. Carey
%% Department of Biostatistics
%% Johns Hopkins School of Public Health
%%
%% vjcarey@sphunix.sph.jhu.edu
