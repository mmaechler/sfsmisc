####--- Utilities -----------------

u.assign0 <- function(x, value, immediate = FALSE) {
    ## Purpose: Simple function with identical UI for both R & S
    ## Author: Martin Maechler, Date: 7 Jul 1999
    assign(x, value, envir = .GlobalEnv)
}
u.get0 <- function(x) get(x, envir = .GlobalEnv)
u.sys <- function(..., intern=TRUE)
    system(paste(..., sep=""), intern=intern)

u.date <- function(short = FALSE)
  format(Sys.time(), paste("%d/%h/%Y", if(!short) ", %H:%M", sep=''))
## Unix-only:  u.sys("date '+%d/%h/%Y", if(!short) ", %H:%M", "'")

u.Datumvonheute <- function(W.tag = 2, Zeit = FALSE)
{
  ## Ziel: Deutsches (kurzes) Datum (als string)
  ##
  ## ==>  ?u.Datumvonheute  [online help]
  ## Unix-only: dat <- as.numeric(system("date '+%w %d %m %Y %H %M' | tr ' ' '\n'",TRUE))
  dat <- as.integer(strsplit(format(Sys.time(),"%w %d %m %Y %H %M"), " ")[[1]])
  ##						 1  2  3  4  5	6
  DMY <- paste(dat[2], ". ", C.Monatsname[dat[3]], " ", dat[4], sep= "")
  r <- if (W.tag) {				#-- wollen Wochentag
    W <- ifelse(dat[1]==0, 7, dat[1])
    if (W.tag==2) Wtag <- C.Wochentag[W]
    else	  Wtag <- C.Wochentagkurz[W]
    paste(Wtag, DMY, sep=", ")
  } else DMY
  if(Zeit) {
    paste(r, if (Zeit==2) paste(dat[5:6], collapse=":")  else dat[5],
	  sep="; ")
  } else  r
}

C.Monatsname <- c("Januar", "Februar", "Maerz", "April", "Mai", "Juni",
	"Juli", "August", "September", "Oktober", "November", "Dezember")

C.Wochentag <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag",
		"Freitag", "Samstag", "Sonntag")
C.Wochentagkurz <- c("Mon", "Die", "Mit", "Don", "Fre", "Sam", "Son")

C.weekday <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

## Months: we had
## C.monthname  === month.name  in R
## C.monthshort === month.abb   in R

##>>> Please: Forget the following !!  it is =====  S function  date() !!
##>>> "u.datum"<- function() unix("date")

u.datumdecode <-
    function(d, YMDHMnames = c("Jahr", "Monat", "Tag", "Std", "Min"))
{
    ## Ziel: Daten der Form 8710230920 aufspalten in Jahr, Monat, Tag, Std, Min
    ## ----------------------------------------------------------------------
    ## Bemerkungen: Dies scheint mir nicht das richtige Konzept.
    ##	Wenn man numerische Datuemer will, soll man doch julianische
    ##	Daten verwenden !! Dann hat man auch eine richtige Zeit-Skala
    ##	Diese Funktionen sind in library(examples) und (verbessert) in
    ##	/u/maechler/s/date.Data !! (Martin Maechler)
    ##=======================================================================
    if(length(YMDHMnames) != 5 || !is.character(YMDHMnames))
        stop("invalid `YMDHMnames': must be character(5)")
    n <- length(d)
    z <- matrix(NA, n, 5, dimnames = list(names(d), YMDHMnames))
    for(j in 5:1) {
        h <- d %/% 100
        z[, j] <- d - 100 * h
        d <- h
    }
    drop(z)# vector if `d' was a scalar (length 1)
}
