#### Bivariate (bi)linear interpolation  --- on a grid ---
#### -----------------------------------------------------
#### The more general situation is handled by the algorithms in the
#### 'akima' package

## approx(x, y = NULL, xout, method = "linear", n = 50,
##        yleft, yright, rule = 1, f = 0, ties = mean)
approx2 <- function(x, y, z, ##-- similar to 'image':
                    ## x[i] * y[j]  defines grid --- z is  |x| * |y| - matrix
                    xout, yout ## of same length
                    ## rule = ?, f = ?, ties = ?
                    )
{
    ## ----------------------------------------------------------------------
    ## Author: Martin Maechler, Date: 14 May 2011, 20:11
    stopifnot(is.matrix(z), is.numeric(z),
              dim(z) == c(length(x), length(y)),
              (n.o <- length(xout)) == length(yout), n.o >= 1)
    i.xo <- findInterval(xout, x)#, rightmost.closed = FALSE, all.inside = FALSE
    i.yo <- findInterval(yout, y)#, rightmost.closed = FALSE, all.inside = FALSE
    if(n.o == 1) { ## only one point
        dx <- x[i.xo+1] - (x0 <- x[i.xo])
        dy <- y[i.yo+1] - (y0 <- y[i.yo])
        f00 <- z[i.xo,i.yo]
        f00 +
            ((f10 <- z[i.xo+1,i.yo  ])- f00)*(tx <- (xout - x0)/dx) +
            ((f01 <- z[i.xo,  i.yo+1])- f00)*(ty <- (yout - y0)/dy) +
            (z[i.xo+1, i.yo+1] + f00 - (f10+f01))* tx*ty
    } else {
        stop("not yet implemented \"parallel\" interpolation at more than one location")
    }
}
