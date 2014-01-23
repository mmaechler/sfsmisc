\name{Sys.cpuinfo}
\alias{Sys.procinfo}
\alias{Sys.cpuinfo}
\alias{Sys.meminfo}
\alias{Sys.memGB}
\alias{Sys.MIPS}
\title{Provide Information about the Linux Hardware (CPU, Memory, etc)}
\description{
  Return information about the Linux hardware, notably
  the CPU (the central processor unit) and memory of the
  computer \R is running on.
  This is currently \bold{only available for Linux}.

  These functions exist on other unix-alike platforms, but produce an
  error when called.
}
\usage{
Sys.procinfo(procfile)
Sys.cpuinfo()
Sys.meminfo()
Sys.memGB(kind = "MemTotal")
Sys.MIPS()
}
\arguments{
  \item{procfile}{name of file the lines of which give the CPU info ``as
    on Linux''}
  \item{kind}{a \code{\link{character}} string specifying which
    \emph{kind} of memory is desired.}
}
\value{
  The \code{Sys.*info()} functions return a \code{"simple.list"},
  here basically a named character vector,
  (where the names have been filtered through \code{\link{make.names}(*,
  unique=TRUE)} which is of importance for multi-processor or multi-core
  CPUs, such that vector can easily be indexed.

  \code{Sys.memGB()} returns available memory in giga bytes [GB];\cr
  \code{Sys.MIPS()} returns a number giving an approximation of
  the \bold{M}illion \bold{I}instructions \bold{P}er \bold{S}econd that
  the CPU processes (using \dQuote{bogomips}).  This is a performance
  measure of the basic \emph{non-numeric} processing capabilities.
  For single-core Linux systems, often about twice the basic clock rate
  in ``MHz'' (as available by \code{Sys.cpuinfo()["cpu.MHz"]}); now,
  with multicore systems, the result is often around (but smaller than)
  \code{2 * #\{cores\} * clock.rate}.
}
\author{Martin Maechler}
\note{These currently do rely on the Linux \file{/proc/} file system, and may not
  easily be portable to non-Linux environments.

  On multi-processor machines, \code{Sys.cpuinfo()} contains each field
  for each processor (i.e., \code{names(Sys.cpuinfo())} has
  \code{\link{duplicated}} entries).

  Conceivably, the bogoMIPS source code is open and available and could
  be built into \R.
}
\seealso{\code{\link{Sys.ps}}, etc.}

\examples{
(n.cores <- parallel::detectCores())
if(substr(R.version[["os"]], 1,5) == "linux") { ##-- only on Linux
  Sys.cpuinfo() # which is often ugly; this looks much better:
  length(Sys.cpu2 <- local({I <- Sys.cpuinfo(); I[ !grepl("^flags", names(I)) ] }))
  ## may still be too much, notably if n.cores > 2:
  (Sys3 <- Sys.cpu2[!grepl("[.][0-9]+$", names(Sys.cpu2))])

  Sys.MIPS() ## just the 'bogomips' from above:
  Sys.MIPS() / as.numeric(Sys.cpuinfo()["cpu.MHz"]) ## ~~ 2 * #{cores} ((no longer))

  ## Available Memory -- can be crucial:
  Sys.memGB() #- default "MemTotal"
  if(Sys.memGB("MemFree") > 16)
     message("Be happy! You have more than 16 Gigabytes of free memory")
}
}
\keyword{utilities}
