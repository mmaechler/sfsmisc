#### PostScript Goodies für R --- `a la /u/sfs/S/ps.goodies.S
####
#### $Id: ps.goodies.R,v 1.10 2004/01/12 15:28:21 maechler Exp $
####

ps.latex <- function(file, height= 5+ main.space*1.25, width= 9.5,
                     main.space = FALSE, lab.space = main.space,
                     paper = "special", title = NULL,
                     lab = c(10, 10, 7), mgp.lab = c(1.6, 0.7, 0),
                     mar = c(4, 4, 0.9, 1.1), ...)
{
  ## Purpose: Setup for 1 LaTeX-includable picture SAVING on white space !
  ##    Calls  ps.do(.) ; par(.)  [ old par in global 'o.p']; USE  ps.end() !
  ## -------------------------------------------------------------------------
  ## Arguments: height & width in INCHES.   (5, 9.5) is 'horizontal look'
  ##            title: to be used in PostScript (-> for gv/ghostview !)
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

  ps.do(file=file, height=height, width=width, paper=paper, title = title, ...)
  ##===

  ## Now: just do the proper par(...) calls :
  mar.main.Extra  <- c(0,0, 3.2,0)
  mar.nolab.Minus <- c(1,1, 0.3,0)
  if(main.space && missing(mar))
    mar <- mar + mar.main.Extra

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
                  onefile = FALSE, horizontal = FALSE, title = NULL, ...)
{
  ## Purpose: "Ghostview" device driver. --- to be "closed" by ps.end(..) --
  ## -------------------------------------------------------------------------
  ## Arguments: file, width, height : file name and dims in inch; 1 in:=2.54 cm
  ##            onefile = F  <==> Encapsulated PS  (Splus default: T, simple PS)

  ## -- new Args:  combining former   ps.do(.) and  ps.col(.) :

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

  if(is.null(title))
      title <- paste("R", paste(R.version[c("major", "minor")], collapse = "."),
                     "plot:", file)
  postscript(file = file, width = width, height = height, horizontal=horizontal,
             onefile = onefile, title = title, print.it = FALSE, ...)
}

ps.end <- function(call.gv = NULL, command = getOption("eps_view"),
                   debug = getOption("verbose"))
{
  ## Purpose:  A "ghostview" device driver (almost).
  ## Author: Martin Maechler, Date:  May 26 1992, 15:32
  ## ----------------------------------------------------------------
  ## Arguments:   call.gv: If TRUE,  call ghostview.
  ##    Default:        Find out if ghostview already runs on this file,
  ##                    If yes, do not call it again.
  ## MUST be called after ps.do(..) or ps.latex() !
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
          ## find command in 'ps' output line (sub/gsub have no 'fixed=TRUE')
          ic <- regexpr(command, f[i], fixed=TRUE)
          ## only keep the file name
          fil <- substr(f[i], ic + attr(ic,"match.length") + 1, 1e4)
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

