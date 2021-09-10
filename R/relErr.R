## The relative error typically returned by all.equal.numeric(),
## "kept as simple as possible"
relErr <- function(target, current) { ## make this work, also for 'Matrix' ==> no mean() ..
    n <- length(current)
    if(length(target) < n) # (as we don't use mean())
        target <- rep(target, length.out = n)
    sum(abs(target - current)) / sum(abs(target))
}

## Componentwise aka "Vectorized" relative error:
## Must not be NA/NaN unless one of the components is  ==> deal with {0, Inf, NA}
relErrV <- function(target, current, eps0 = .Machine$double.xmin) {
    n <- length(target <- as.vector(target))
    ## assert( <length current> is multiple of <length target>) :
    lc <- length(current)
    if(!n) {
	if(!lc) return(numeric()) # everything length 0
	else stop("length(target) == 0 differing from length(current)")
    } else if(!lc)
	stop("length(current) == 0 differing from length(target)")
    ## else n, lc  > 0
    if(lc %% n)
	stop("length(current) must be a multiple of length(target)")
    R <- if(lc != n) # explicitly recycle
	     target[rep(seq_len(n), length.out=lc)]
	 else
	     target # (possibly "mpfr")
    R[] <- 0
    ## use *absolute* error when target is zero {and deal with NAs}:
    t0 <- abs(target) < eps0 & !(na.t <- is.na(target))
    R[t0] <- current[t0]
    ## absolute error also when it is infinite, as (-Inf, Inf) would give NaN:
    dInf <- is.infinite(E <- current - target)
    R[dInf] <- E[dInf]
    useRE <- !dInf & !t0 & (na.t | is.na(current) | (current != target))
    R[useRE] <- (current/target)[useRE] - 1
    R
}
