if(is.R())
    u.assign0 <- function(x, value, immediate = FALSE) {
        ## Purpose: Simple function with identical UI for both R & S
        ## Author: Martin Maechler, Date: 7 Jul 1999
        assign(x, value, envir = .GlobalEnv)
    }
else # S-plus
    u.assign0 <- function(x, value, immediate = FALSE) {
        ## Purpose: Simple function with identical UI for both R & S
        ## Author: Martin Maechler, Date: 7 Jul 1999
        assign(x, value, frame = 0, immediate = immediate)
    }
