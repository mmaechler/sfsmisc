###-- Synchronize with ../man/Deprecated.Rd !!

nna <- function(data)
{
    ## Purpose: "No NA" :  throw out NA s, also for MATRIX data
    ## Author:   WSt, Date: Dec 89; simplified,improved: M.Maechler, Nov.93, 94
    ## --------------------------------------------------------------------
    .Deprecated("na.omit")
    ## ------ NOTE:  na.omit(.) is VERY SIMILAR (has attr()..)

    Error <- "'nna(.)' is defined for vectors, matrices & data.frames only"
    if(is.atomic(data) || is.data.frame(data)) {
        ##-- should work for 'named vectors' [is.vector -> F !], data.frames,..
        if(is.null(dim(data))) data[!is.na(data)]
        else if(is.matrix(data))  data[!apply(is.na(data), 1, any), ]
        else stop(Error)
    } else stop(Error)
}

digits.v <- function(nvec, base = 2, num.bits = 1 + floor(log(max(nvec),base)))
{
    warning("'digits.v'() is deprecated -- please use  baseDigits() instead!")
    baseDigits(nvec, base = base, ndigits = num.bits)
}

digits <- function(n, base = 10)
{
    warning("'digits'() is deprecated -- please use  baseDigits() instead!")
    drop(baseDigits(n, base=base))
}

subtit <- function(t) mtext(t, side = 3, line = 0)
