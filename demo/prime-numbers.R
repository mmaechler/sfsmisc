####---- Prime numbers, factorization, etc. --- "illustatration of programming"

###---- Function definitions --------

if(!exists("is.sorted", mode='function'))
   is.sorted <- function(x) (length(x) <= 1) || all(diff(x) >= 0)

prime.sieve <- function(p2et = c(2,3,5), maxP = pM^2)
{
  ## Purpose: Produce ALL prime numbers from 2, 3.., using 2,3,5,7,...
  ## -------------------------------------------------------------------------
  ## Arguments: p2et: primes c(2,3,5,..., pM);
  ##            maxP : want primes up to maxP
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 26 Jan 96, 15:08
  if(any(p2et[1:2] != 2:3) || !is.sorted(p2et) || !is.numeric(p2et))
        stop("argument 'p2et' must contain SORTED primes 2,3,..")
  k <- length(p2et)
  pM <- p2et[k]
  if(maxP <= pM+1) p2et #- no need to compute more
  else if(maxP > pM^2)  prime.sieve(prime.sieve(p2et), maxP=maxP)
  else { #-- pM < maxP <= pM^2
    r <- seq(from = pM+2, to = maxP, by = 2)
    for(j in 1:k)
      if(0== length(r <- r[r%% p2et[j] != 0])) break
    c(p2et,r)
  }
}

factorize <- function(n)
{
  ## Purpose:  Prime factorization of integer(s) 'n'
  ## -------------------------------------------------------------------------
  ## Arguments: n vector of integers to factorize (into prime numbers)
  ##    --> needs 'prime.sieve'
  ## >> Better would be: Define class 'primefactors' and "multiply" method
  ##                     then use this function recursively only "small" factors
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 26--30 Jan 96
  N <- length(n <- as.integer(n))
  M <- trunc(sqrt(max(n))) #-- check up to this prime number
  ##-- for M > 100 to 200: should DIVIDE first and then go on ..
  ##-- Here, I am just (too) lazy:
  k <- length(pr <- prime.sieve(maxP = M))
  nDp <- outer(pr, n, FUN = function(p,n) n %% p == 0) ## which are divisible?
  # dim(nDp) = (k,N) ;
  ## Divide those that are divisible :
  ## quot <- matrix(n,k,N,byrow=T)[nDp] %/% matrix(pr,k,N)[nDp]
  ## quot <- rep(n,rep(k,N))[nDp] %/% rep(pr,N)[nDp]
  res <- vector("list",length = N)
  names(res) <- n
  for(i in 1:N) { ## factorize  n[i]
    nn <- n[i]
    if(any(Dp <- nDp[,i])) { #- Dp: which primes are factors
      nP <- length(pfac <- pr[Dp]) # all the small prime factors
      if(exists("DEBUG")&& DEBUG) cat(nn," ")
    } else { # nn is a prime
      res[[i]] <- cbind(p=nn, m=1)
      prt.DEBUG("direct prime", nn)
      next # i
    }
    m.pr <- rep(1,nP)# multiplicities
    Ppf <- prod(pfac)
    while(1 < (nn <- nn %/% Ppf)) { #-- have multiple or only bigger factors
      Dp <- nn %% pfac == 0
      if(any(Dp)) { # have more smaller factors
	m.pr[Dp] <- m.pr[Dp] + 1
	Ppf <- prod(pfac[Dp])
      } else { #-- the remainder is a bigger prime
	pfac <- c(pfac,nn)
	m.pr <- c(m.pr, 1)
	break # out of while(.)
      }
    }
    res[[i]] <- cbind(p=pfac,m=m.pr)

  } # end for(i ..)

  res
}

test.factorize <- function(res)
{
  ## Purpose:  Test prime factorization
  ## -------------------------------------------------------------------------
  ## Arguments: result of  factorize
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 29 Jan 96, 10:29
  n <- as.integer(names(res))
  n == sapply(res, function(pf) prod(pf[,"p"] ^ pf[,"m"]))
}


factorize(n <- c(7,27,37,5*c(1:5, 8, 10)))
factorize(47)
factorize(7207619)## quick !

unix.time(fac.1ex <- factorize(1000 + 1:99)) #-- 0.95 sec  (sophie)

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
