##' A slightly more convenient version of packageDescription()
##' 1. returns named character vector; no other attributes
pkgDesc <- function (pkg, lib.loc = NULL, fields = NULL, onlyLT = 99, ...) {
    pd <- packageDescription(pkg, lib.loc=lib.loc, fields=fields, drop=FALSE, ...)
    if(is.numeric(onlyLT) && onlyLT > 0 && any(tL <- vapply(pd, nchar, -99L) >= onlyLT))
        pd[tL] <- NULL
    file <- attr(pd, "file")
    struct(class = "Dlist", c(unlist(pd), c(file=file)))
}

##' useful: e.g. as  sapply( pkgs, pkgBuilt)
pkgBuilt <- function(pkg, lib.loc = NULL, ...) pkgDesc(pkg, lib.loc=lib.loc, fields = "Built", ...)
