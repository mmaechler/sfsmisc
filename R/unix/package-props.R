pkgLibs <- function(pkg) {
    stopifnot(is.character(pkg))
    lapply(setNames(pkg,
                    nm = vapply(pkg, function(p) system.file(package = p), " ")),
           function(p) {
               libD <- Filter(dir.exists,
                              file.path(system.file(package = p), "libs")) # possibly empty
               if(length(libD)) {
                   libs <- list.files(libD, full.names=TRUE)
                   lapply(setNames(libs, basename(libs)),
                          function(so) sub("^[ \t]*", "",
                                           system(paste("ldd", so), intern=TRUE)))
                   ##__TODO_ strsplit() into (2--)3 parts:
                   ##  "libgcc_s.so.1 => /usr/lib64/libgcc_s.so.1 (0x00007fe7ad090000)"

               } # else NULL
           })
}

if(FALSE) {
    ## Example:  "V8" is not working anymore in F26 when installed in Fedora F24

    pl <- pkgLibs(c("sfsmisc", "Rmpfr", "pcalg", "V8", "lme4"))
    pl
    str(pl)

}
