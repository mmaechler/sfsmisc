\name{str_data}
\alias{str_data}
\title{Overview on All Datasets in an R Package}
\description{
  Provide an overview over all datasets available by
  \code{\link{data}()} in a (list of) given \R packages.
}
\usage{
str_data(pkgs, filterFUN, \dots)
}
\arguments{
  \item{pkgs}{character vector of names of \R packages.}
  \item{filterFUN}{optionally a \code{\link{logical}}
    \code{\link{function}} for filtering the \R objects.}
  \item{\dots}{potentical further arguments to be passed to
    \code{\link{str}}; \code{str(utils:::str.default)} gives useful list.}
}
\value{
  invisibly (see \code{\link{invisible}}) a \code{\link{list}} with
  named components matching the \code{pkgs} argument.  Each of these
  components is a named list with one entry per \code{data(.)} argument
  name.  Each entry is a \code{\link{character}} vector of the names
  of all objects, typically only one.

  The side effect is, as with \code{\link{str}()}, to print
  everything (via \code{\link{cat}}) to the console.
}
\author{Martin Maechler}
\seealso{\code{\link[utils]{str}}, \code{\link[utils]{data}}.
}
\examples{
str_data("cluster")

str_data("datasets", max=0, give.attr = FALSE)

## Filtering (and return value)
dfl <- str_data("datasets", filterFUN=is.data.frame)
df.d <- dfl$datasets
## work around bug in R < 2.14.1 (?) {for *some* setups only}:
str(df.d <- df.d[sapply(df.d, is.character)])
## dim() of all those data frames:
t(sapply(unlist(df.d), function(.) dim(get(.))))

### Data sets in all attached packages but "datasets" (and stubs):
s <- search()
(Apkgs <- sub("^package:", '', s[grep("^package:", s)]))
str_data(Apkgs[!Apkgs \%in\% c("datasets", "stats", "base")])
}
\keyword{datasets}
\keyword{utilities}
\keyword{documentation}

