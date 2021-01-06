library(sfsmisc)

options(digits=9)

set.seed(12)
m <- matrix(round(rnorm(25),2), 5, 5); m <- 1+ m + t(m); diag(m) <- diag(m) + 4
(mp  <- posdefify(m))
(mp. <- posdefify(m, method = "allEV"))

stopifnot(eigen(mp,  only.val=TRUE)$values > 0,
          eigen(mp., only.val=TRUE)$values > 0,
          all.equal(diag(m), diag(mp), tol= 1e-15),
          all.equal(diag(m), diag(mp.),tol= 1e-15),
          T)

## nearcor()
pr <- matrix(c(1,     0.477, 0.644, 0.478, 0.651, 0.826,
               0.477, 1,     0.516, 0.233, 0.682, 0.75,
               0.644, 0.516, 1,     0.599, 0.581, 0.742,
               0.478, 0.233, 0.599, 1,     0.741, 0.8,
               0.651, 0.682, 0.581, 0.741, 1,     0.798,
               0.826, 0.75,  0.742, 0.8,   0.798, 1),
             nrow = 6, ncol = 6)

nc.  <- nearcor(pr, conv.tol = 1e-7)  # default, 11 iter.
ncr  <- nearcor(pr, conv.tol = 1e-15) # 27-28 iterations (because of conv.tol)!
ncr0 <- nearcor(pr, conv.tol = 1e-15, posd.tol = 0)# -> no posdefify step
parts <- setdiff(names(nc.), "iterations")
str(ncr [parts])
str(ncr0[parts])# looks identical (with few digits accuracy)
nr <- ncr$cor

stopifnot(exprs = {
    nc.$iterations == 11
    all.equal(nr[lower.tri(nr)],
	      c(0.48796803265083, 0.64265188295401, 0.49063868812228, 0.64409905497094,
		0.80871120142824, 0.51411473401472, 0.25066882763262, 0.67235131534931,
		0.72583206922437, 0.59682778611131, 0.58219178154582, 0.7449631866236,
		0.72988206459063, 0.77215024062758, 0.81319175546212), tol = 1e-12)
    ncr$iterations %in% 27:28 # platform dependently ("no long double")
    all.equal(ncr, ncr0, tolerance = 6e-6)# seen 6.6676e-7
})
