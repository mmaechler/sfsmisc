####---- Prime numbers, factorization, etc. --- "illustatration of programming"

###---- Function definitions -------- for examples, see "./prime-numbers.S"

if(!exists("is.sorted", mode='function'))
   is.sorted <- function(x) (length(x) <= 1) || all(diff(x) >= 0)

prime.sieve <- function(p2et = c(2,3,5), maxP = pM^2)
{
  ## Purpose: Produce ALL prime numbers from 2, 3.., using 2,3,5,7,...
  ## -------------------------------------------------------------------------
  ## Arguments: p2et: primes c(2,3,5,..., pM);
  ##		maxP : want primes up to maxP
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 26 Jan 96, 15:08
  if(any(p2et[1:2] != 2:3) || !is.sorted(p2et) || !is.numeric(p2et))
	stop("argument 'p2et' must contain SORTED primes 2,3,..")
  k <- length(p2et)
  pM <- p2et[k]
  if(maxP <= pM+1) p2et #- no need to compute more
  else if(maxP > pM^2)	prime.sieve(prime.sieve(p2et), maxP=maxP)
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
  ##	--> needs 'prime.sieve'
  ## >> Better would be: Define class 'primefactors' and "multiply" method
  ##			 then use this function recursively only "small" factors
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: 26--30 Jan 96
  N <- length(n <- as.integer(n))
  M <- trunc(sqrt(max(n))) #-- check up to this prime number
  ##-- for M > 100 to 200: should DIVIDE first and then go on ..
  ##-- Here, I am just (too) lazy:
  k <- length(pr <- prime.sieve(maxP = M))
  nDp <- outer(pr, n, FUN = function(p,n) n %% p == 0) ## which are divisible?
  ## dim(nDp) = (k,N) ;
  ## Divide those that are divisible :
  ## quot <- matrix(n,k,N,byrow=T)[nDp] %/% matrix(pr,k,N)[nDp]
  ## quot <- rep(n,rep(k,N))[nDp] %/% rep(pr,N)[nDp]
  res <- vector("list",length = N)
  names(res) <- n
  for(i in 1:N) { ## factorize	n[i]
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

##- From: Bill Venables <wvenable@attunga.stats.adelaide.edu.au>
##- Date: Thu, 10 Sep 1998 21:02:20 +0930
##- To: mona kanaan <M.N.Kanaan@open.ac.uk>
##- Cc: s-news@wubios.wustl.edu
##- Subject: Re: [S] factors (divisors ) of an integer
##-
##- > Dear all,
##- >	  I wonder whether there is an already built in Splus function to find
##- > the divisors of a given integer, if so could you please point it out to
##- > me.
##- >	  Or if someone has already written such a function, could you
##- > please pass it over, if possible.
##- >
##- >
##- > The function I am looking for works sth like this
##- >
##- >  N <- 6
##- >  DN <- DIV(N)
##- >  DN
##- >  1 2 3 6
##- >
##- > Thanks a lot,
##- > Mona
##-
##- This turns out to be a pretty little programming exercise.
##- Here's a vectorized version, even, although it only returns the
##- *prime* divisors, not all the devisors.  That a supplmentary
##- exercise...

factorizeBV <- function(n) {
    if(!is.numeric(n))
	stop("cannot factorize non-numeric arguments")
    if(length(n) > 1) {
	l <- list()
	for(i in seq(along = n))
	    l[[i]] <- Recall(n[i])
	return(l)
    }
    if(n != round(n) || n < 2)
	return(n)
    tab <- 2:n
    fac <- numeric(0)
    while(n > 1) {
	while(n %% tab[1] == 0) {
	    fac <- c(fac, tab[1])
	    n <- n/tab[1]
	}
	tab <- tab[tab <= n]
	omit <- tab[1] * c(1, tab[tab <= n/tab[1]])
	tab <- tab[ - match(omit, tab, nomatch = 0)]
    }
    fac
}
factorizeBV(6)
##[1] 2 3
factorizeBV(4:8)
##- [[1]]:
##- [1] 2 2
##-
##- [[2]]:
##- [1] 5
##-
##- [[3]]:
##- [1] 2 3
##-
##- [[4]]:
##- [1] 7
##-
##- [[5]]:
##- [1] 2 2 2


##- From: mona kanaan <M.N.Kanaan@open.ac.uk>
##- Date: Fri, 11 Sep 1998 08:52:59 +0100 (BST)
##- To: "'S-News'" <s-news@wubios.wustl.edu>
##- Subject: [S] Summary: Factors (divisors) of an inreger

##- Thanks a lot, for everybody who replied to my query.
##- Here is a summary of what was passed on.
##- The first two codes due to Bill Venables and Bill Dunlap give the Prime
##- divisors of an integer(this is what i was actually looking for), the last
##- code gives all divisors but is not efficient for "large"
##- integers (this is what i was trying to avoid).
##-
##- Thanks again
##- Mona

## .... Bill Venables solution	[see above !] ........


##- -----------------------------------------------------------------
##- -----------------------------------------------------------------
##- Bill Dunlap
##-
##- I use the following factors(), which uses the enclosed primes():
##-
##-	[MMä: mv'ed examples *AFTER* function definitions ]


factors <- function(x)
{
    factor1 <- function(y, max.factor, .Primes)
    {
	if(missing(.Primes))
	    .Primes <- primes(max.factor)
	else .Primes <- primes(max.factor, .Primes)
	f <- numeric(0)
	while(y > 1) {
	    ## note: 1 has no factors according to this
	    which <- y %% .Primes == 0
	    if(sum(which) == 0) {
		f <- c(f, y)
		break
	    }
	    else f <- c(f, .Primes[which])
	    y <- y/prod(.Primes[which])
	}
	val <- sort(f)
	if(length(val) && any(big <- val > max.factor^2)) {
	    if(sum(big) != 1)
		stop("internal error: sum(big)!=1")
	    val <- sort(c(val[!big],
			  Recall(val[big], min(ceiling(sqrt(val[big])),
					       max.factor^2), .Primes)))
	}
	val
    }
    val <- lapply(x, factor1, 43)
    names(val) <- as.character(x)
    val
}

primes <- function(n, .Primes = c(2, 3, 5, 7, 11, 13, 17, 19,
		      23, 29, 31, 37, 41, 43))
{
    if(max(.Primes) < n) {
	## compute longer .Primes by sieve
	.Primes <- seq(from = 2, to = n)
	for(i in 1:length(.Primes)) {
	    composite <- .Primes %% .Primes[i] == 0
	    composite[i] <- F
	    if(all(!composite))
		break
	    .Primes <- .Primes[!composite]
	    if(i >= length(.Primes))
		break
	}
    }
    .Primes[.Primes <= n]
}

factors( round(gamma(13:14)))
##- $"479001600":
##-  [1]  2  2	2  2  2	 2  2  2  2  2	3  3  3	 3  3  5  5  7 11
##-
##- $"6227020800":
##-  [1]  2  2	2  2  2	 2  2  2  2  2	3  3  3	 3  3  5  5  7 11 13


## --- You can use table() to collect repeated factors : ----

lapply( factors( round(gamma(13:14))), table)
##- $"479001600":
##-   2 3 5 7 11
##-  10 5 2 1  1
##-
##- $"6227020800":
##-   2 3 5 7 11 13
##-  10 5 2 1  1  1



##- factors.simple() is easier to understand and is faster on small numbers
##- but can work very slowly on large numbers with lots of small factors
##- (like numbers arising in combinatorics).


factors.simple <- function(x)
{
	factor1 <- function(y, .Primes)
	{
		f <- numeric(0)
		while(y > 1) {
## note 1 has no factors according to this
			which <- y %% .Primes == 0
			if(sum(which) == 0) {
				f <- c(f, y)
				break
			}
			else f <- c(f, .Primes[which])
			y <- y/prod(.Primes[which])
		}
		sort(f)
	}
	val <- lapply(x, factor1, primes(ceiling(sqrt(max(x)))))
	names(val) <- as.character(x)
	val
}

##- ----------------------------------------------------------------------------
##- ----------------------------------------------------------------------------
##- ----------------------------------------------------------------------------
##-
##- Guido Schwarzer ,Gardar Johannesson,  Remy vande Ven, Henrik Aalborg-Nielsen

DIV <- function(N) {
    N.seq <- 1:N
    N.seq[(N %% N.seq) == 0]
}




##- From: "Frank E Harrell Jr" <fharrell@virginia.edu>
##- To: "s-news" <s-news@wubios.wustl.edu>
##- Subject: [S] An improved factorize()
##- Date: Sat, 12 Sep 1998 22:34:05 -0400
##-
##- Here is a modification of Michael Bramley's (bramley.m@pg.com) factorize
##- function with memory usage of approx. the square root of the original.

divisors <- function(n)
{
 ## Frank E Harrell Jr -- called this "factorize()"
 p <- n/(z <- 1:ceiling(sqrt(n)))
 z <- z[trunc(p) == p]
 unique(c(z, rev(n/z)))
}

##- From: Paul A Tukey <paul@bellcore.com>
##- Date: Wed, 16 Sep 1998 18:27:15 -0400 (EDT)
##- To: fharrell@virginia.edu, lifer@fuse.net, s-news@wubios.wustl.edu
##- Subject: Re: [S] Prime divisors

##- This discussion has been fun. 

##- Seems to me we've been gradually heading toward
##- computing the prime factorization of a number.
##- That is, a collection of prime numbers (with possible
##- duplication) whose product is the given number.

##- A recursive layer on top of Frank Harrell's factorize() does
##- it -- but the code below uses a slightly shortened version
##- of factorize() that only returns the smallest divisor > 1.

fac <- function(n) {
        p <- n/(z <- 1:floor(sqrt(n)))
        z <- z[trunc(p) == p]
        c(z, rev(n/z))[2]
}

pfac <- function(n, nn = 0)
{
        if(nn == 0)
                pfac(n, fac(n))
        else if(n <= nn)
                nn
        else c(nn, pfac(n/nn))
}

##- Note that prod(pfac(n)) == n.

##- Now I'm sure someone can write a more elegant version.
##- Also, recursion is probably neither memory-efficient
##- nor CPU-efficient in Splus.

##-   -- Paul Tukey
##-      Bellcore
