glob2rx <- function(pattern)
{
    ## Purpose: Change "ls" aka "wildcard" aka "globbing" _pattern_ to
    ## 	      Regular Expression (as in grep, perl, emacs, ...)
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler ETH Zurich, ~ 1991
    ##         New version using [g]sub() : 2004
    p <- gsub('\\.','\\\\.', paste('^', pattern, '$', sep=''))
    sub('\\.\\*\\$$','', ## < this is only for esthetics
        gsub('\\?',  '.',  gsub('\\*',  '.*', p)))
}
