#### PostScript Goodies für R --- `a la /u/sfs/S/ps.goodies.S
#### 
####

ps.latex <- function(file, height= 5+ main.space*1.25, width= 9.5,
                     main.space = FALSE, lab.space = main.space,
                     iso.latin1 = is.R(),
                     ##not yet in R: ps.title = paste("S+ :",file),
                     lab = c(10,10,7), mgp.lab = c(1.6, .7, 0),
                     mar = c(4,4,0.9,1.1), ...)
{
  ## Purpose: Setup for 1 LaTeX-includable picture SAVING on white space !
  ##    Calls  ps.do(.) ; par(.)  [ old par in global 'o.p']; USE  ps.end() !
  ## -------------------------------------------------------------------------
  ## Arguments: height & width in INCHES.   (5, 9.5) is 'horizontal look'
  ##            ps.title: to be used in PostScript (-> for ghostview !)
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

  ps.do(file=file, height=height, width=width,
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

  u.assign0("ps.file", file)
  if(length(list(...))) {
    pso <- ps.options(...)
    on.exit(ps.options(pso)) #- reset ps.options !
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
             onefile = onefile, print.it = FALSE)
}

ps.end <- function(call.gv = NULL, do.color = u.get0("ps.do.color"),
		   command = paste("gview",if(!do.color)" -monochrome", sep=''),
                   debug = FALSE)
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
  if( names(dev.cur()) == "postscript")    dev.off()
  if (is.null(call.gv)) {
    ps.cmd <- if(substring(u.sys("uname -r"),1,1) == "4") #-- SunOS4
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
        call.gv <- ps.file != fil
        if(!call.gv)
          break  #-- don't  call ghostview since it runs this file..
      }
    }
  }
  if (call.gv) u.sys(command, " ", ps.file, "&", intern=FALSE)
  else
    cat("\n >> switch to   GV (Ghostview) window -- updated automagically!\n\n")
}

## From ~/S/postscript.colors.S
ps.colors.test <- function(ps.color.mat = ps.colors.23,
                           do.post= T, show.it= T,
                           region = ps.region.CS100)
{
  ## Purpose:  Test Color matrix of postscript colors
  ## Authors: Dirk Stoehr & Martin Maechler, Date:  Tue Dec 17 1991 / Apr 92
  ## ----------------------------------------------------------------
  ## Arguments: ps.color.mat: 3-D color coordinates (Hue,Sat,Bright)
  ##                       see "?postscript"
  ##    do.post: should postscript be done, or current device be used ?
  ##    show.it: should the postscript be shown by  ghostview ?
  ## ----------------------------------------------------------------
  ##->>> NOTE: This is time-intensive !!
  ## ----------------------------------------------------------------
  ##- test with  colors.test(cbind(seq(0, 1, len = 9), 1, 1))
  ##- Usage examples :
  ##-   ps.colors.test()
  ##-   ps.colors.test(cbind(seq(0, 1, len = 51), 1, 1))
  ##-   ps.colors.test(cbind(seq(0, 1, len = 51), .5, 1))
  ##-   ps.colors.test(cbind(seq(0, 1, len = 51), 1 , .5))
  ## ----------------------------------------------------------------
  if(Browse) on.exit(browser())         #
  ncolor <- nrow(ps.color.mat)
  has.4 <- ncol(ps.color.mat) == 4
  if(do.post) {
    filename <- paste(deparse(substitute(ps.color.mat)), ".ps", sep="")
    lett <- unique(ichar(filename))
    good.lett <- ichar(c(LETTERS,letters, paste(0:9), "."))
    if(any(is.na(match(lett, good.lett)))) {
      code <- round(sum(ps.color.mat %*% c(if(has.4)0, 10^(2:0)))/ncolor)
      filename <- paste("colors.test", paste(ncolor), paste(code),
                        "ps", sep = ".")
    }
    postscript(file = filename, region = region, colors = ps.color.mat)
    cat("\n PostScript  file name:", filename,"\n")
  }
  par(mar = c(1,0,2,0), usr = c(0, 6, 0, ncolor + 4)); frame()
  mtext("Farben auf dem Thermodrucker im RZ", side =3, cex = 1.2)
  mtext(        if (missing(ps.color.mat))
        "(``ps.colors.23'' -- as posted once to S-news)"
        else    deparse(substitute(ps.color.mat)),
        side =3, line = -1, cex = 0.8)
  name <- dimnames(ps.color.mat)[[1]]
  x.txt <- 3.8;  dx.txt <- .6
  if (is.null(dimnames(ps.color.mat)[[2]]))
    dimnames(ps.color.mat)[2] <-
      list(c(if (has.4) "", "Hue", "Saturation", "Brightness"))
  for (j in 1:3)
    text(x.txt + (j-1)*dx.txt, ncolor + 2 +(j%%2)/2,
         dimnames(ps.color.mat)[[2]][has.4 + j]) # , adj = 0
  if (ncolor > 40) par(cex = 20/ncolor) else
  if (ncolor > 20) par(cex = .75)
  for(i in 1:ncolor) {
    y <- ncolor - i + .5
    polygon(2 + c(0,0, 1,1), y + c(0,1, 1,0), col = i)
    lines(c(0,6), rep(y + .1, 2), col = i)
    text(.1, y + 0.5, paste(i,":",name[i]), adj = 0, col = i) #  srt = 90
    for (j in 1:3)
      text(x.txt + (j-1)*dx.txt, y + 0.5,
           paste(format(round(ps.color.mat[i, has.4 + j], 4))), adj = 0)
  }
  if(do.post) {
    if (exists("dev.off") && is.function(dev.off))
      dev.off()
    else        graphics.off()
    if (show.it)  u.sys("gv ", filename, "&", intern=FALSE)
  } else  Device.Default()
  on.exit()
}



ps.show.fonts <- function(out.file = "test.ps.fonts.ps",
                         font.nrs = seq(ps.fonts), ...)
{
  ## Purpose: Print all the characters of some (default : all) Postscript fonts
  ##       in nice tabular form, two fonts per page
  ## Author: Martin Maechler, Date:  Mar 27 1992, 13:09
  ## ----------------------------------------------------------------
  ## Arguments: out.file = name of file where to put postscript code
  ##            font.nrs = numeric vector; index in ps.fonts, of fonts to use
  ## NB : Needs global variable  "All.ASCII"
  ## ----------------------------------------------------------------
  ## Example : ps.show.fonts (font=13)
  ##    ps.options(reset=T); ps.show.fonts("fonts.1.13.std.ps", font=c(1,13))
  ##    ps.setup.SfS();      ps.show.fonts("fonts.1.13.iso.ps", font=c(1,13))
  postscript(file = out.file, horizontal = F, ...)
  par(mfrow=c(2,1))
  ps.fonts <- ps.options("fonts")[[1]]
  setf <- ps.options("setfont")[[1]]
  setf <- if(is.expression(setf)) deparse(setf) else substring(setf[1],1,20)
  for (ifont in  font.nrs) {
    par(font = ifont)
    ## c.ifont <- formatC (ifont, w=2, flag = "0") #- pad leading zeros
    plot(1:16, 0:15, type='n', lab=c(20,20,7), xlab ="i", ylab = "j", yaxt='n',
         font = ifont,
         main = paste('Postscript font "', ps.fonts[ifont],
           '"; S:  font = ', ifont, sep=""))
    axis(2, at = 15:0, labels = F)
    mtext(paste(15:0*16),2,line=1,at=0:15,font=1)
    abline( h = -.5 + 4*1:3, lty=2)
    abline( v = +.5 + 4*1:3, lty=2)
    text(rep(1:16,16), rep(15:0,rep(16,16)), All.ASCII)
    mtext(paste("ps.options(setfont)=", setf), line=1.2)
    mtext("Char. at [i,j] == All.ASCII[ i + j ] = ascii(i+j-1)", side=3, line=0)
  }
  if (exists("dev.off", mode="function")) dev.off() else  graphics.off()
}

###- end-of-former /u/maechler/S/Good.S goodies---------------------------------

