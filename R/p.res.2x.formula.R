if(FALSE) ##: This is not yet ready for prime time
p.res.2x.formula <-  ## Change the name ;  no 'lm'
    ## take graphics:::mosaicplot.formula() as example
function(formula = ~., data, restricted = NULL, size = 1,
         slwd = 1, scol = 2,
         xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, ...)
{
    ## Purpose:  plot residuals vs. two x's
    ## Author:   ARu , Date:  11/Jun/91
    ## Aenderungen: MMae, 30/Jan/92, Dez.94 / WSt
    ## --------------------------------------------------------------------------
    ## Arguments:
    ##   formula   formula defining the variables zu be used, either
    ##             z ~ x + y
    ##             ~ x + y   in this case, data must inherit from  lm ,
    ##             and the residuals of  data  will be used as  z .
    ##   data      a data.frame or an  lm  or  aov  object.
    ##             In the latter case, g.rex2x will look for the data
    ##             that was used to fit the model.
    ##  restricted      absolute value which truncates the size.
    ##             The corresponding symbols are marked by stars.
    ##  size       the symbols are scaled so that 'size' is the size of
    ##             the largest symbol in cm.
    ##  slwd, scol line width and color to be used for the symbols
    ##  ...        additional arguments for the S-function 'plot'
    ## EXAMPLE :
    ## g.res2x(zz~.,data=data.frame(xx=rep(1:10,7),yy=rep(1:7, rep(10,7)),
    ##    zz=rnorm(70)), restr = 2, main = "i.i.d.  N(0,1) random residuals")
    ## --------------------------------------------------------------------------
    formula <- as.formula(formula)
    if (inherits(data,"lm")) {
        t.d <- get(as.character(data$call[3]))
        if (length(formula) < 3) {
            formula <- update.formula(formula,residuals~.)
            t.d <- f.merge1(t.d,resid(data),namefrom = "residuals")
        }
    } else  t.d <- data
    if (!is.data.frame(t.d)) {
        if(is.matrix(data)) data <- as.data.frame(data) else
        stop("data is not a data frame or 'lm' object")
    }
    t.d <- na.omit(model.frame(formula, t.d))
    z <- t.d[,1]
    x <- as.numeric(t.d[,2])
    y <- as.numeric(t.d[,3])
    if(is.null(xlab)) xlab <- names(t.d)[2]
    if(is.null(ylab)) ylab <- names(t.d)[3]

    ##
    ##--- restrict z values: ---
    az <- abs(z)
    has.restr <-
        if(is.null(restricted)) FALSE else any(restr <- az > restricted)
    if(has.restr) {
        z[z >   restricted] <- restricted
        z[z < - restricted] <- - restricted
    }
    ## if (is.null(xlim)) xlim <- range(x)
    ## if (is.null(ylim)) ylim <- range(y)
    rx <- range(x)
    ry <- range(y)
    ##--- fix plot region: ---
    pcm <- par("pin") * 2.54            #damit in cm
    ##--- damit im Plot das Symbol wirklich die Groesse size hat:
    size <- size/(2 * sqrt(2))
    fx <- (size * diff(rx))/(pcm[1] - 2 * size)/2
    fy <- (size * diff(ry))/(pcm[2] - 2 * size)/2
    ##--
    plot(x, y, xlim = rx + c(-1,1)* fx, ylim = ry + c(-1,1)* fy, pch = ".",
         xlab = xlab, ylab = ylab, ...)

    ##--- draw symbols: ---
    z <- z/max(abs(z), na.rm = TRUE)
    usr <- par("usr")
    sxz <-     diff(usr[1:2])/pcm[1] * size * z
    syz <- abs(diff(usr[3:4])/pcm[2] * size * z)
    segments(x - sxz, y - syz,  x + sxz, y + syz, lwd = slwd, col = scol)

    ##--- mark restricted observations: ---
    if(any(restr)) {
        points((x - sxz)[restr], (y - syz)[restr], pch = 8, mkh = 1/40)
        points((x + sxz)[restr], (y + syz)[restr], pch = 8, mkh = 1/40)
    }

}

