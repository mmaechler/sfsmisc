.onLoad <- function(lib, pkg)
{
    ## This assumes  "gv"  in your path --- ideally this would be configured!
    if(.Platform $ OS.type == "unix") {
	SYS <- function(cmd) system(cmd, intern=TRUE, ignore.stderr=TRUE)
        doesRespond <- function(cmd) length(SYS(cmd)) > 0
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

## was	.First.lib <- function(lib, pkg) { .. }


if(getRversion() < "2.5") {

    ## note that this remains hidden in "sfsmisc" namespace
    parse <- function (file = "", n = NULL, text = NULL, prompt = "?",
		       srcfile = NULL, encoding = "unknown")
    {
	## just drop 'srcfile' and 'encoding'
	base::parse(file, n=n, text=text, prompt=prompt)
    }

}
