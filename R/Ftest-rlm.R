##- From: Werner Stahel <stahel@stat.math.ethz.ch>
##- To: holzer@stat.math.ethz.ch, maechler@stat.math.ethz.ch
##- Subject: robuster F-test
##- Date: Fri, 14 Jul 2000 17:01:55 +0200 (CEST)


f.robftest <- function(object, var = -1)
{
  ## Purpose:   robust F-test: Wald test for several coefficients of
  ##            an rlm object
  ## -------------------------------------------------------------------------
  ## Arguments:
  ##   object   result of rlm(...)
  ##   var      variables. Either their names or their indices
  ## -------------------------------------------------------------------------
  ## Author: Werner Stahel, Date: 14 Jul 2000;  MM, 2000-07-14

  if ( !inherits(object,"rlm"))
    stop("f.robftest only works for rlm objects")

  require("MASS")# for summary.rlm()

  ## determine and check coefficients to be tested
  t.cf <- object$coef
  t.vind <-
    if(is.character(var)) match(var,names(t.cf)) else seq(length(t.cf))[var]
  t.wrong <- is.na(t.vind) | t.vind > length(t.cf) | t.vind < 1
  if (any(t.wrong)) 
    stop(paste("variable ",var[t.wrong]," not found"))
  t.coef <- t.cf[t.vind]
  if (0 == (t.nv <- length(t.coef)))
    stop("no variables to be tested")
  ## covariance matrix of estimated coefficients
  t.r <- summary.rlm(object, method="XtWX")
  ## Nota BENE:  vcov() calls vcov.lm() which uses  $sigma instead of $stddev !
  t.cov <- t.r$cov.unscaled[t.vind,t.vind] * t.r$stddev ^ 2
  ## results

  ##t.f <- c(t.coef %*% solve(t.cov) %*% t.coef)/t.nv
  ## quite a bit more efficient (for larger p):  x' A^-1 x :
  t.f <- sum(t.coef * solve(t.cov, t.coef))/t.nv
  t.df <- c(t.nv, t.r$df[2])

  ## MM: FIXME:  Return an object of class "htest" ! ---> print.* method, etc
  ## ---------   library(ctest)
  ##             --------------
  list(statistic = c(F = t.f), df = t.df,
       p.value = pf(t.f, t.df[1], t.df[2], lower.tail = FALSE))
}

