####--- Utilities -----------------

if(is.R()) {
    u.assign0 <- function(x, value, immediate = FALSE) {
        ## Purpose: Simple function with identical UI for both R & S
        ## Author: Martin Maechler, Date: 7 Jul 1999
        assign(x, value, envir = .GlobalEnv)
    }
    u.get0 <- function(x) get(x, envir = .GlobalEnv)
    u.sys <- function(..., intern=TRUE)
      system(paste(..., sep=""), intern=intern)
    
} else { # S-plus

    u.assign0 <- function(x, value, immediate = FALSE) {
        ## Purpose: Simple function with identical UI for both R & S
        ## Author: Martin Maechler, Date: 7 Jul 1999
        assign(x, value, frame = 0, immediate = immediate)
    }
    u.get0 <- function(x) get(x, frame = 0)
    u.sys <- function(..., intern=TRUE)
      unix(paste(..., sep=""), output.to.S = intern)
}

