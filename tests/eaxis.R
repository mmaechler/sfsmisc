library(sfsmisc)
axt <- -3:4
stopifnot(all.equal(pretty10exp(10^axt, drop.1=TRUE, sub10 = c(-2, 2)),
                    expression(10^-3, 0.01, 0.1, 1, 10, 100, 10^3, 10^4)))

stopifnot(all.equal(pretty10exp(10^axt, drop.1=TRUE, sub10 = c(-2, 2), lab.type="latex"),
      c("$10^{-3}$", "0.01", "0.1", "1", "10", "100", "$10^{3}$", "$10^{4}$")))
