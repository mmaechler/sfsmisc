
## This is also in the SfS package /u/sfs/R/SfS/R/misc-goodies.R :
D1D2 <- function(x, y, xout = x, fudge.fact = 10, deriv = 1:2, spl.spar=NULL)
{
    ## Purpose: Numerical first derivatives of  f() for   y_i = f(x_i) + e_i.
    ## Find  f'(xout) & f''(xout) -- using smoothing splines with GCV'
    ## Author: Martin Maechler, Date:  23 Sep 1992, 9:40
    ## -------------------------------------------------------------------------
    ## Arguments: x = { x_i } MUST be sorted increasingly // y = { y_i }
    ## -------------------------------------------------------------------------
    if(is.unsorted(x)) {
        i <- order(x)
        x <- x[i]
        y <- y[i]
    }
    sp <-
        if(is.null(spl.spar)) {
            sp <- smooth.spline(x,y)
            smooth.spline(x,y,
                          spar = if(is.R() && {v <- R.version;
                                               v$major <= 1 && v$minor < 3})
                          sp$ spar + 1/3 * log(fudge.fact, 2) / 8
                          ## because lambda = r * 256 ^ (3 * spar - 1)
                          else # S compatible
                          fudge.fact * sp$ spar
                          )
        } else smooth.spline(x,y, spar = spl.spar)
    c(list(x = x,
           D1 = if(any(deriv==1)) predict(sp, xout, deriv = 1) $ y,
           D2 = if(any(deriv==2)) predict(sp, xout, deriv = 2) $ y),
      sp[c("spar", "df")])
}


