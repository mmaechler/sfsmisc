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
