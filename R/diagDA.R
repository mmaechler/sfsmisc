diagDA <- function(ls, cll, ts, pool= TRUE)
{
    ## Purpose: Diagonal (Linear or Quadratic) Discriminant Analysis
    ## ----------------------------------------------------------------------
    ## Arguments: --> ?diagDA  (i.e. ../man/diagDA.Rd )
    ## ----------------------------------------------------------------------
    ## Authors: Sandrine Dudoit, sandrine@stat.berkeley.edu
    ##	        Jane Fridlyand, janef@stat.berkeley.edu
    ## as function  stat.diag.da() in package "sma"
    ##
    ## Modification (API and speed): Martin Maechler, Date: 19 Nov 2003, 15:34

### ---------------------- Fit Model ------------------------------
    ls <- data.matrix(ls)
    n <- nrow(ls)
    p <- ncol(ls)

    m0 <- as.integer(min(cll, na.rm=TRUE) - 1)
    cll <- as.integer(cll) - m0 ## cll now in 1:K
    inaC <- is.na(cll)
    clL <- cll[!inaC]
    K <- max(clL)
    if(K != length(unique(clL)))
        stop(sQuote("cll")," did not contain *consecutive* integers")

    nk <- integer(K)
    m <- v <- matrix(0,p,K)

    colVars <- function(x, means = colMeans(x, na.rm = na.rm), na.rm=FALSE) {
        x <- sweep(x, 2, means)
        colSums(x*x, na.rm = na.rm) / (nrow(x) - 1)
    }
    sum.na <- function(x) sum(x, na.rm=TRUE)

    ## Class means and variances
    for(k in 1:K) {
        which <- (cll == k)
        nk[k] <- sum.na(which)
        lsk <- ls[which, , drop = FALSE]
        m[,k] <- colMeans(lsk, na.rm = TRUE)
        v[,k] <- colVars (lsk, na.rm = TRUE, means = m[,k])
    }

### ---------------------- Predict from Model -----------------------------

    ts <- data.matrix(ts)
    if(p != ncol(ts))
        stop("test set matrix must have same columns as learning one")
    ## any NA's in test set currently must give NA predictions
    ts <- na.exclude(ts)
    nt <- nrow(ts)
    disc <- matrix(0, nt,K)

    if(pool) { ## LDA
        ## Pooled estimates of variances
        vp <- rowSums(rep(nk - 1, each=p) * v) / (n - K)
        ## == apply(v, 1, function(z) sum.na((nk-1)*z))/(n-K)

        ## FIXME: check for 0 variance now !!
        ivp <- rep(1/vp, each = nt) # to use in loop

        for(k in 1:K) {
            y <- ts - rep(m[,k], each=nt)
            disc[,k] <- rowSums(y*y * ivp)
            ## == apply(ts, 1, function(z) sum.na((z-m[,k])^2/vp))
        }
    }
    else { ## QDA
###>>>>>>>>>>>>>>>>>>>>> FIXME! -- improve the same as "LDA" above <<<<<<<<<<<<
        for(k in 1:K)
            disc[,k] <- apply(ts,1,
                              function(z)
                              sum((z-m[,k])^2/v[,k]) + sum.na(log(v[,k])))
    }

    ## predictions

    pred <- m0 + apply(disc, 1, which.min)
    if(inherits(ts, "omit")) # had missings in `ts'
        pred <- napredict.exclude(omit = attr(ts,"na.action"), pred)
    pred
}
