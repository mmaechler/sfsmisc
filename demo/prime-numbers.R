####---- Prime numbers, factorization, etc. --- "illustatration of programming"

###---- EXAMPLES

###--- load the current function definitions:
source("/u/maechler/S/MISC/prime-numbers-fn.S")

factorize(n <- c(7,27,37,5*c(1:5, 8, 10)))
factorize(47)
factorize(7207619)## quick !

unix.time(fac.1ex <- factorize(1000 + 1:99)) #-- 0.95 sec (sophie Sparc 5)
#-- 0.4 / .65 sec (florence Ultra 1/170)
unix.time(fac.2ex <- factorize(10000 + 1:999))
## R 0.49 : 5.4 sec (florence Ultra 1/170)
## ------   6.1 sec (sophie   Ultra 1/140)

## This REALLY takes time -- (for loop over 10000; 10000 times res[[i]] <-..
unix.time(factorize.10000 <- factorize(1:10000))
## sophie: Sparc 5 (..) :lots of swapping after while, >= 20 minutes CPU;
##			then using less and less CPU, ..more swapping ==> KILL
## florence (hypersparc): [1] 1038.90   5.09 1349.  0   0  ( 17 min. CPU)
## lynne (Ultra-1):       [1]  658.77   0.90  677.  0   0

if(!is.null(vv <- version$language) && vv == "R") save()


object.size(factorize.1e4) #-->[1] 3027743

###--- test
test.factorize(fac.1ex[1:10]) #-- T T T
which(!test.factorize(fac.1ex))
which(!test.factorize(factorize(8000 + 1:1000)))


prime.sieve(prime.sieve())
unix.time(P1e4 <- prime.sieve(prime.sieve(prime.sieve()), max=10000))
##-> 1.45 (on sophie: fast Sparc 5 ..)
length(P1e4) #--> 1229

unix.time(P1e4.2 <- prime.sieve( max=10000))
##-> 1.46 (sophie)   maybe a little longer

unix.time(P1e5 <- prime.sieve(P1e4, max=1e5))
##-> 105.7  (on sophie: fast Sparc 5)

P1000 <- prime.sieve(max=1000)
###------ start plot device is necessary !!
plot(P1000,  seq(P1000), type='b', main="Prime number theorem")
lines(P1000, P1000/log(P1000), col=2, lty=2, lwd=1.5)

plot(P1e4,  seq(P1e4), type='l', main="Prime number theorem")
lines(P1e4, P1e4/log(P1e4), col=2, lty=2, lwd=1.5)

u.dev.default()
{ ps.do("prime-number.ps")
  mult.fig(2, main="Prime number theorem")
  plot(P1e5,seq(P1e5), type='l', main="pi(n) &  n/log(n) ",
       xlab='n',ylab='pi(n)', log='xy', sub = 'log - log - scale')
  lines(P1e5, P1e5/log(P1e5), col=2, lty=2, lwd=1.5)
  mtext(paste("/u/maechler/S/MISC/prime-numbers.S",u.Datumvonheute(Zeit=T),
	      sep='\n'), side = 3, cex=.75, adj=1, line=3, outer=T)

  plot(P1e5, seq(P1e5) / (P1e5/log(P1e5)), type='b', pch='.',
       main= "Prime number theorem : pi(n) / {n/log(n)}", ylim =c(1,1.3),
       xlab = 'n', ylab='pi(n) / (n/log(n)', log='x')
  abline(h=1, col=3, lty=2)
  ps.end() }
