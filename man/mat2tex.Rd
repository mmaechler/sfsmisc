\name{mat2tex}
\alias{mat2tex}
\title{
Write LaTeX commands to print a matrix
}
\usage{
mat2tex(x, file="mat.tex", append=T, digits=3, title)
}
\arguments{
\item{x}{
a matrix
}
\item{file}{
names the file to which LaTeX commands should be written
}
\item{append}{
if F, will destroy the file file before writing commands
to it; if T, simply adds commands at the end of file file
}
\item{digits}{
setting of digits option for purpose of number representation
}
\item{title}{
a string, possibly using LaTeX commands, which will span
the columns of the LaTeX matrix
}}
\value{
No value is returned to S.  This function, when used correctly, 
only writes LaTeX commands to a file.
}
\examples{
test <- matrix(c(pi,pi/2,pi/4,exp(1),exp(2),exp(3)),nrow=2,
			       byrow=T)
dimnames(test) _ list(c("$\\pi$","$e$"),c("a","b","c"))
mat2tex( test, title="$\\pi, e$, etc." )


The last command produces the file "mat.tex" containing
(the following excerpt is rewritten to be processed by the
S HELP nroff program)


 \\begin{tabular} {| l|| c| c| c|}  
 \\multicolumn{ 4 }{c}{ $\\pi, e$, etc. } \\\\ \\hline 
    \\  & a & b & c \\\\ \\hline \\hline 
 $\pi$ & 3.14 & 1.57 & 0.785 \\\\ \\hline 
 $e$ & 2.72 & 7.39 & 20.1 \\\\ \\hline 
 \\end{tabular}  


Now you have to properly embed the contents of this file
in a LaTeX document -- for example, you will need a
preamble, the \\begin{document} statement, etc.


Note that the backslash needs protection in dimnames
or title actions.


vjcarey@sphunix.sph.jhu.edu
}
% Converted by Sd2Rd version 1.10.
