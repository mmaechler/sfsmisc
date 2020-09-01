pkgLibs <- function(pkg, cmd = if(Sys.info()[["sysname"]] == "Darwin") "otool -L" else "ldd") {
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
                                           system(paste(cmd, so), intern=TRUE)))
                   ##__TODO_ strsplit() into (2--)3 parts:
                   ##  "libgcc_s.so.1 => /usr/lib64/libgcc_s.so.1 (0x00007fe7ad090000)"

               } # else NULL
           })
}

if(FALSE) {
    ip <- installed.packages() # can only look at installed pkgs
    str(ip)

    pkgs <- intersect(c("sfsmisc", "MASS", "Matrix", "nlme",
                        "Rmpfr", "pcalg", "V8", "lme4", "round"),
                      ip)
    pkgs

    pl <- pkgLibs(pkgs) # needs  system("ldd" ..)  to work
    pl
    str(pl)
}
