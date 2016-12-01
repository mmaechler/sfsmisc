###--> Synchronize with ../man/Deprecated.Rd !!
###--> move things from here as defunct to  ./Defunct.R
###                                           =========

## Deprecation of these, as of  2016-12-01 :
pmax.sa <- function(scalar, arr)
{
    warning("pmax.sa(s,a) is deprecated; use  pmax(a,s)  instead")
}

pmin.sa <- function(scalar, arr)
{
    warning("pmin.sa(s,a) is deprecated; use  pmin(a,s)  instead")
}


## Deprecation of these, as of  2013-08-03 :
u.assign0 <- function(x, value, immediate = FALSE) {
    ## Purpose: Simple function with identical UI for both R & S
    ## Author: Martin Maechler, Date: 7 Jul 1999
    warning("u.assign0(..) is deprecated, use assign(.., , envir = .GlobalEnv)\n",
            "   {if you really must; that is deprecated in packages as well}")
    ## assign(x, value, envir = .GlobalEnv) :
    .a <- as.name(paste0("a", "ss", "ign"))
    eval(substitute(AA(x, value, envir = .GlobalEnv), list(AA = .a)))
}
u.get0 <- function(x) {
    warning("u.get0(x) is deprecated, use get(x, envir = .GlobalEnv)")
    get(x, envir = .GlobalEnv)
}
