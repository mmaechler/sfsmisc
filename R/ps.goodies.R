#### PostScript Goodies für R --- `a la /u/sfs/S/ps.goodies.S
####
#### $Id: ps.goodies.R,v 1.6 2001/08/29 13:43:35 sfs Exp sfs $
####

ps.latex <- function(file, height= 5+ main.space*1.25, width= 9.5,
                     main.space = FALSE, lab.space = main.space,
                     iso.latin1 = is.R(), paper = "special",
                     ##not yet in R: ps.title = paste("S+ :",file),
                     lab = c(10, 10, 7), mgp.lab = c(1.6, 0.7, 0),
                     mar = c(4, 4, 0.9, 1.1), ...)
{
  ## Purpose: Setup for 1 LaTeX-includable picture SAVING on white space !
  ##    Calls  ps.do(.) ; par(.)  [ old par in global 'o.p']; USE  ps.end() !
  ## -------------------------------------------------------------------------
  ## Arguments: height & width in INCHES.   (5, 9.5) is 'horizontal look'
  ##            ps.title: to be used in PostScript (-> for gv/ghostview !)
  ##            main.space & lab.space: if T, leave space for 'main' & 'x/ylab'
  ##            lab :  for  par(.);  (10,10,7): use more axis 'labels' ..
  ##            mgp.lab & mar :  for par(.): these are values for 'lab.space=T'
  ## Note: FIRST fiddle with 'main.sp.', 'lab.sp.'  before 'mgp.lab' and 'mar'!
  ## -------------------------------------------------------------------------
  ## EXAMPLE:for(m in c(T,F)){str(ps.latex("q.ps",main=m));acf(hstart);ps.end()}
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, Date: Sep 94; Sept. 95

  if(!missing(lab) && !(length(lab)==3 && is.numeric(lab) && all(lab >=0)))
    stop("'lab' must be numeric vector >= 0, of length 3")
  if(!missing(mgp.lab) && !(length(mgp.lab)==3 && is.numeric(mgp.lab) &&
                            all(mgp.lab >=0) && all(diff(mgp.lab)<=0)))
    stop("'mgp.lab' must be non-increasing numeric vector >= 0, of length 3")
  if(!missing(mar) && !(length(mar)==4 && is.numeric(mar) && all(mar >=0)))
    stop("'mar' must be non-negative numeric vector of length 4")

  ps.do(file=file, height=height, width=width, paper=paper,
        iso.latin1=iso.latin1, ##not yet in R: title = ps.title,
        ...)
  ##=== Now: just do the proper   par(...)  call
  mar.main.Extra  <- c(0,0, 3.2,0)
  mar.nolab.Minus <- c(1,1,  .3,0)
  if(main.space) {
    if(missing(mar)) mar <- mar + mar.main.Extra
  }
  if(!lab.space) {
    mar <- mar - mar.nolab.Minus
    if(main.space)
      warning("'main.space' is TRUE, but 'lab.space' is FALSE ...")
  }
  o.p <- par(mar = mar, mgp= mgp.lab)
  o.p <- c(o.p, par(lab=lab)) # need 2 step for  bug ?
  ## "frame 0 / GlobalEnv assignment:
  u.assign0("o.par.psl", o.p)
  invisible(list(old.par=o.p, new.par= par(c("mar","mgp","lab"))))
}

ps.do <- function(file, width = -1, height = -1,
                  onefile = FALSE, horizontal = FALSE, iso.latin1 = is.R(),
                  do.color = NULL, colors = NULL, image.colors = NULL,
		  ...)
{
  ## Purpose: "Ghostview" device driver. --- to be "closed" by ps.end(..) --
  ## -------------------------------------------------------------------------
  ## Arguments: file, width, height : file name and dims in inch; 1 in:=2.54 cm
  ##            onefile = F  <==> Encapsulated PS  (Splus default: T, simple PS)

  ## -- new Args:  combining former   ps.do(.) and  ps.col(.) :
  ##	do.color: if TRUE, produce COLOR PostScript (and use colors & image.col)
  ##		Default: TRUE, if 'colors' or 'image.colors' are non-NULL.
  ##	colors: Eine Matrix (d.h. echte HSB-Farbe);
  ##	        DEFAULT:  ps.col23.rgb wurde in
  ##		S-news gepostet (und von uns noch etwas um-ge-dimnamed).
  ##		dimnames(ps.col23.rgb)  sagt alles
  ##	  colors = matrix(c(1:9/9, rep(1, 2 * 9)), ncol = 3)
  ##		[ nach einem Vorschlag von Chris Thorson, Statsci ]
  ##		ergibt 9 verschiedene Farben,
  ##		1=gelb, 2=hellgruen, 3=gruen, 4=helles blau, 5=blau,
  ##		6=dunkelblau, 7=violett, 8=rosarot, 9=rot
  ##    image.colors:  Farbmatrix fuer image; Default:
  ##            image.cold.hot _ cbind(c(64:128/128,rep(1,64)),0:128/128,
  ##                                   c(128:64/128,rep(0,64)))
  ##    ...  :  passed to ps.options
  ## -------------------------------------------------------------------------
  ## Author: Martin Maechler, 1992-1995
  ##
  ## --->>>>>> CONSIDER   'ps.latex'   instead  for pictures !

  u.assign0("..ps.file", file)
  if(length(l... <- list(...))) {
    ## This does NOT work : pso are the *NEW*, not the *former* ones!
    oldop <- ps.options()[names(l...)]
    ps.options(...)
    on.exit( do.call("ps.options", oldop) ) #- reset ps.options !
  }
  no.do.col <- is.null(do.color)
  if(no.do.col)
      do.color <- !is.null(colors) || !is.null(image.colors)
  if(!do.color) {
    if(no.do.col)
      do.color <- TRUE # since R does colors anyway..
    else
      warning("In R, you currently ALWAYS get color postscript.\n Setting up colors is VERY different (and MM thinks `nicer') than with S(-plus).")
  }
##-   if(do.color)
##-     {
##-       if(is.null(colors))	colors <- ps.col23.rgb
##-       if(is.null(image.colors)) image.colors <- image.cold.hot
##-       pso <- c(pso, ps.options(colors=colors, image.colors=image.colors))
##-   }
  u.assign0("ps.do.color", do.color)
  if(!iso.latin1)
    stop("In R, you currently MUST allow ISO-latin1 text.")

  postscript(file = file, width = width, height = height, horizontal=horizontal,
             onefile = onefile, print.it = FALSE, ...)
}

ps.end <- function(call.gv = NULL, do.color = u.get0("ps.do.color"),
		   command = paste("gview",if(!do.color)" -monochrome", sep=''),
                   debug = getOption("verbose"))
{
  ## Purpose:  A "ghostview" device driver (almost).
  ## Author: Martin Maechler, Date:  May 26 1992, 15:32
  ## ----------------------------------------------------------------
  ## Arguments:   call.gv: If TRUE,  call ghostview.
  ##    Default:        Find out if ghostview already runs on this file,
  ##                    If yes, do not call it again.
  ## MUST be called after ps.do(..) !
  ## Example:  ps.end(com = "ghostview -a4")
  ## ----------------------------------------------------------------
  ## Only if  postscript is running !! --
  if( names(dev.cur()) == "postscript")
    dev.off()
  if (is.null(call.gv)) {
    ps.cmd <- if(u.sys("uname") == "Linux" ||
                 substring(u.sys("uname -r"),1,1) == "4") #-- SunOS4
      "ps -wx" else "/usr/bin/ps -u $USER -o args"
    f <- u.sys(ps.cmd, " | grep '", command, "' | grep -v grep")
    if(debug) { cat("ps.end(): f:\n");print(f) }
    call.gv <- length(f) == 0
    if(!call.gv) {
      ##--- STILL does NOT work
      ##--- if you work with two different pictures simultaneously
      for(i in 1:length(f)) { #-- only NOT call if THIS ps.file .. --
        fil <- u.sys("echo '", f[i],"' | perl -p -e 's/(.*)",
                     command,"\\s*(.*)/\\2/'")
        cat("ps.end(): fil:",fil,"\n")
        call.gv <- length(fil) < 1 || all(..ps.file != fil)
        if(!call.gv)
          break  #-- don't  call ghostview since it runs this file..
      }
    }
  }
  if (call.gv) u.sys(command, " ", ..ps.file, "&", intern=FALSE)
  else
    cat("\n >> switch to   GV (Ghostview) window -- updated automagically!\n\n")
}

###- end-of-former /u/maechler/S/Good.S goodies---------------------------------

