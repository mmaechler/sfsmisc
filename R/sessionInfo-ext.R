sessionInfoX <- function(pkgs=NULL, list.libP = FALSE, extraR.env = TRUE) {
    ## return an object; then print() via method
    if(!is.null(pkgs)) stopifnot(is.character(pkgs), length(pkgs) > 0)
    lP <- .libPaths() # *is* normalized in the sense of normalizePath()
    nRL <- normalizePath(RLIBS <- strsplit(Sys.getenv("R_LIBS"), ":")[[1]])
    si <- sessionInfo()
    Rver <- package_version(si$R.version)
    structure(class = "sessionInfoX",
        list(sInfo  = si,
             sysInf = Sys.info(),
	     capabilities = capabilities(),
	     extSoft = if(Rver >= "3.2.0") extSoftVersion(),
	     LAPACK  = if(Rver >= "3.0.3") La_version(),
	     pcre    = if(Rver >= "3.1.3") pcre_config(),
             pkgDescr = if(!is.null(pkgs)) sapply(pkgs, packageDescription, simplify=FALSE),
             libPath = lP, .Library = .Library, RLIBS = RLIBS, n.RLIBS = nRL,
             list.libP = if(list.libP) sapply(lP, list.files, simplify=FALSE),
             R.env = Sys.getenv(c("R_ENVIRON", "R_PROFILE", "R_CHECK_ENVIRON")),
             xR.env = if(extraR.env) local({
                 ss <- Sys.getenv()
                 ss[grepl("^_?R_", names(ss))]
             })))
}

print.sessionInfoX <- function(x, locale = TRUE, RLIBS = TRUE, Renv = TRUE, ...) {
    cat("Extended  sessionInfo():",
        "-----------------------", sep="\n")# does add a final '\n'
    if(!is.null(pkgD <- x$pkgDescr)) {
        cat("specific  packageDescription()s:\n")
        print(pkgD, ...)
        cat("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n\n")
    }
    cat("Capabilities:\n")
    print(symnum(x$capabilities, symbols = c("-", "X")), ...)
    cat("Sys.info:\n")
    print(structure(x$sysInf[c("nodename", "user")], class="Dlist"), ...)
    cat("\n")
    if(!is.null(x$LAPACK)) cat("LAPACK version:", x$LAPACK, "\n")
    if(!is.null(x$extSoft)) {
        cat("External software (versions):\n")
        print(structure(x$extSoft, class="Dlist"), ...)
    }
    ## if(!is.null(x$pcre)) {
    ##     cat("\nPCRE (regex) config.: ")
    ##     print(........)
    ## }
    cat("\n")
    if(RLIBS) {
        cat("R_LIBS:\n")
        cbind(x$RLIBS)
        xtr.lp <- setdiff(x$libPath,
                          union(normalizePath(x$.Library), x$n.RLIBS))
        if(length(xtr.lp)) {
            cat("libPath [.libPaths()] contents in addition to R_LIBS and .Library:\n")
            print(xtr.lp)
        } else
            cat("libPath contains not more than RLIBS and .Library (normalized)\n")
        if(length(xx <- setdiff(x$n.RLIBS, x$libPath))) { ## typically empty
            cat("** RLIBS has entries not in .libPaths():\n")
            print(xx)
        }
    }
    if(Renv) {
        cat("Main R env. variables",
            if(!is.null(x$xR.env)) " (for more, inspect the 'xR.env' component)",
            ":\n", sep="")
        print(cbind(x$R.env), ...)
    }
    cat("---------------- standard sessionInfo():\n")
    print(x$sInf, locale=locale, ...)
    invisible(x)
}
