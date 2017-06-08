## .onLoad <- function(lib, pkg)
## {

## }

## if(!exists("rep_len", mode = "function")) # old R version
##     rep_len <- function(x, length.out) rep(x, length.out=length.out)


.set.eps_view <- function() {
    ## This assumes  "gv"  in your path --- ideally this would be configured!
    if(!is.character(getOption("eps_view")) && .Platform $ OS.type == "unix") {
	SYS <- function(cmd) system(cmd, intern=TRUE, ignore.stderr=TRUE)
        ## doesRespond <- function(cmd) length(SYS(cmd)) > 0
        doesRespond <- function(cmd) ## should be portable (thanks BDR):
            all(system(paste(cmd,"> /dev/null")) != c(1,256)*127)
	if(doesRespond("gv -h")) { ## 'gv'
	    cmd <- "gv -watch -geometry -0+0 -magstep -2 -media BBox -noantialias"
	    hyphens <-
		SYS(paste("gv -h | fgrep watch | head -1",
			  "| sed 's/watch.*//; s/^[\\s ]*//'"))
	    if(length(hyphens) && hyphens == "--")
		cmd <- sub(" --geometry", " -geometry",
			   sub(" --magstep ", " --scale=",
			       sub(" --media ", " --media=",
				   gsub(" -([a-z])", " --\\1", cmd))))
	}
	else if (doesRespond("ggv --version")) { ## try 'ggv'
	    cmd <- "ggv --geometry -0+0"
	} else if (doesRespond("evince --version")) { ## try 'evince'
	    cmd <- "evince" # no geometry options
	} else if (doesRespond("kghostview --version")) { ## try 'kghostview'
	    cmd <- "kghostview --geometry -0+0"
	} else {
	    warning("no valid postscript previewer found; consider setting\n",
		    "  options(\"eps_view\"=  \"....\")	    yourself")
	    cmd <- "replace_with_postscript_previewer"
	}
	options("eps_view" = cmd)
    }
}
