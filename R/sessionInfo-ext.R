isRshared <- function(platform = .Platform) {
    platform$ OS.type == "windows" || {
	## works on Linux;  what about others, notably Mac ?
	ldd.s <- try(Rcmd(paste("ldd", R.home("bin/exec/R"), "| head -5"),
			  stdout=TRUE))
	## If in doubt (error etc), assume
	## R executable to be linked to libR.{so,dylib} , i.e., "shared" :
	inherits(ldd.s, "try-error") || is.null(ldd.s) || anyNA(ldd.s) ||
	    any(grepl(paste0("^.?libR", platform$dynlib.ext), ldd.s))
    }
}

sessionInfoX <- function(pkgs=NULL, list.libP = FALSE, extraR.env = TRUE) {
    ## return an object; then print() via method
    if(!is.null(pkgs)) stopifnot(is.character(pkgs), length(pkgs) > 0)
    lP <- .libPaths() # *is* normalized in the sense of normalizePath()
    nRL <- normalizePath(RLIBS <- strsplit(Sys.getenv("R_LIBS"), ":")[[1]])
    si <- sessionInfo()
    Rver <- package_version(si$R.version)
    ## typically the "same" [ setequal(.,.) ] as loadedNamespaces() :
    pkgs <- c(si[["basePkgs"]],
              unlist(lapply(si[c("otherPkgs", "loadedOnly")], names), use.names=FALSE))
    structure(class = "sessionInfoX",
        list(sInfo  = si,
             sysInf = Sys.info(),
	     capabilities = capabilities(),
	     extSoft = if(Rver >= "3.2.0") extSoftVersion(),
	     grSoft  = if(Rver >= "3.2.0") grSoftVersion(),
	     tclVersion=if(Rver >= "3.2.0" && "tcltk" %in% pkgs) tcltk::tclVersion(),
	     LAPACK  = if(Rver >= "3.0.3") La_version(),
	     pcre    = if(Rver >= "3.1.3") pcre_config(),
	     isRshared = isRshared(),
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
    if(!is.null(x$grSoft)) {
        cat("Graphical software (versions):\n")
        print(structure(x$grSoft, class="Dlist"), ...)
    }
    if(!is.null(x$tclVersion))
        cat("Tcl version:", x$tclVersion, "\n")
    if(!is.null(x$pcre))
	cat("\nPCRE (regex) config.:",
	    sub("^c", "", deparse(x$pcre, width.cutoff=99)), "\n")
    cat("R executable linked against libR.* ['is R shared']:", x$isRshared, "\n")
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

shortRversion <- function(Rv = R.version,
                          Rst = Rv$status,
                          Rvstring = if(!is.null(s <- Rv$version.string))
                                         ## in R 0.90.1 had no $version.string
                                         s else R.version.string,
                          date = Rst != "",
                          spaces = TRUE)
{
    pat <- paste0("\\(", if(date) "([^)]+)" else "[0-9]{4}-[0-9]{2}-[0-9]{2} *(.+)",
                  "\\)$")
    r <-
        if(Rst == "Under development (unstable)")
            ## "R Under development (unstable) (2017-10-16 r73554)"
            paste("R devel", sub(paste0(".*",pat), "\\1", Rvstring))
        else if(Rst == "Patched") # "R version 3.4.2 Patched (2017-10-12 r73556)"
            sub(pat, "\\1", sub(" version", "", Rvstring))
        else if(Rst == "") # "R version 3.2.5 (2016-04-14)"  (regular release)
            gsub(if(date) "[()]" else " \\(.*", "", sub(" version", "", Rvstring))
        else
            stop("invalid R.version $ status: ", sQuote(Rst))

    if(spaces) r else gsub(" ", "_", sub("^R ", "R-", r))
}
