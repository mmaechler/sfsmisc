## I would really like builtin  Sys.PID() Sys.ps() for these

as.Integer <- function(x){ storage.mode(x) <- "integer"; x }

Sys.PID <- function() {
    ## Return the PID of the R process :

    ## This is not really portable;
    ## it works with GNU ps [e.g. Linux], and Solaris
    as.integer(system("ps -o pid", intern = TRUE)[2])
}

## "Docu" at end ..
.Sys.ps.fields <-
 c("%cpu", "%mem", "alarm", "args", "blocked", "bsdstart", "bsdtime",
   "c", "caught", "cmd", "comm", "command", "cputime", "drs", "dsiz",
   "egid", "egroup", "eip", "esp", "etime", "euid", "euser", "f",
   "fgid", "fgroup", "flag", "flags", "fname", "fsgid", "fsgroup",
   "fsuid", "fsuser", "fuid", "fuser", "gid", "group", "ignored",
   "intpri", "lim", "longtname", "lstart", "m_drs", "m_trs", "maj_flt",
   "majflt", "min_flt", "minflt", "ni", "nice", "nwchan", "opri",
   "pagein", "pcpu", "pending", "pgid", "pgrp", "pid", "pmem", "ppid",
   "pri", "rgid", "rgroup", "rss", "rssize", "rsz", "ruid", "ruser",
   "s", "sess", "session", "sgi_p", "sgi_rss", "sgid", "sgroup", "sid",
   "sig", "sig_block", "sig_catch", "sig_ignore", "sig_pend",
   "sigcatch", "sigignore", "sigmask", "stackp", "start", "start_stack",
   "start_time", "stat", "state", "stime", "suid", "suser", "svgid",
   "svgroup", "svuid", "svuser", "sz", "time", "timeout", "tmout",
   "tname", "tpgid", "trs", "trss", "tsiz", "tt", "tty", "tty4", "tty8",
   "ucomm", "uid", "uid_hack", "uname", "user", "vsize", "vsz", "wchan")

## Note that  proc.time() gives part of that info better

## command == cmd == args   gives "command + arguments : too long
.Sys.ps.multifields <- c("command", "cmd","args", "lstart")

Sys.ps <-
    function(process = Sys.PID(),
             fields = c("pid", "%mem","%cpu",
             "cputime","time",
             "sz","rsz","tsiz","rssize","vsize",
             "user", "comm"),
             usefile = length(fields) > 10,
             verbose = getOption("verbose"))
{
    ps.opt <- {
        if(is.numeric(process) && process == round(process))
            paste("-p",process) # PID
        else if(process == "ALL")
            "-e" # all process
        else if(is.character(process) && length(process) == 1)
            paste("-C",process) # Command name
        else stop(paste("invalid `process':",format(process)))
    }
    i.field <- pmatch(fields, .Sys.ps.fields) # allow abbreviated ones
    if(any(ina <- is.na(i.field))) {
        warning(paste("Dropping invalid field names",
                      fields[ina]))
        i.field <- i.field[!ina]
    }
    fields <- .Sys.ps.fields[i.field]
    imult <- !is.na(match(fields, .Sys.ps.multifields))
    if(any(imult) && length(fields) > 1) {
        warning(paste("Not using `multi fields' ",
                      paste(fields[imult],collapse=",")))
        fields <- fields[!imult]
        imult <- FALSE
    }
    ## Don't use "-w" with  cmd/args, or command : gives space in between
    ## Must use "--width" (GNU ps only) when there are many fields ...
    ## Need temporary file & scan since system cannot get very long
    ## lines ...
    if(usefile)
        ofile <- tempfile("R.Sys.ps")
    cmd <- paste("ps --width 1000", ps.opt,
                 "-o", paste(fields, collapse=","),
                 if(usefile) paste(" >", ofile))
    if(verbose) cat("Now calling\n\t",cmd,"\n")
    lines <- system(cmd, intern = !usefile)
    if(usefile) {
        if(lines) warning(paste("system() returned non-0 :",lines))
        lines <- scan(ofile, what = "", sep="\n", quiet = TRUE)## incl header
    }
    if(length(lines) <= 1)
        stop(paste("call returned less than two lines:", lines, sep="\n"))
    
    r <- sub("^ ","", gsub("[ 	]+"," ", lines))
    ##                     SP & TAB
    if(length(fields) == 1) {
        if(length(r) == 2)
            return(structure(r[2], names = fields))
        else
            warning(paste("Funny result with one `fields': length(r)=",
                          length(r)))
    }
    ## else {
    ll <- strsplit(r, " ")
    d.len <- diff(lenl <- sapply(ll, length))
    if(lenl[1] == length(fields))
        ## use fields!
        ll[[1]] <- fields
    else
        warning(paste("Number returned headers =", lenl[1], " != ",
                      "#{fields} =", length(fields)))
    if(d.len) { # names and result differ
        warning(paste("Lengths differ:",
                      paste(lenl, collapse=",")))
    }
    r <- c(ll[[2]], rep(NA, max(0,-d.len)))
    names(r) <-
        if( d.len > 0) c(ll[[1]], rep(".x.",d.len)) else ll[[1]][1:lenl[2]]
    r
    ##}
}

Sys.sizes <- function(process = Sys.PID())
{
    r <- Sys.ps(process, c("rssize","dsiz","vsize"))
    storage.mode(r) <- "integer"
    r
}

##  STANDARD FORMAT SPECIFIERS
##     These  may be used to control both output format and sorting.
##     For example:
##		ps -eo pid,user,args --sort user
##
##  CODE          HEADER
##  ----          ---------
##  %cpu          %CPU
##  %mem          %MEM
##  alarm         ALARM
##  args          COMMAND
##  blocked       BLOCKED
##  bsdstart      START
##  bsdtime       TIME
##  c             C
##  caught        CAUGHT
##  cmd           CMD
##  comm          COMMAND
##  command       COMMAND
##  cputime       TIME
##  drs           DRS
##  dsiz          DSIZ
##  egid          EGID
##  egroup        EGROUP
##  eip           EIP
##  esp           ESP
##  etime         ELAPSED
##  euid          EUID
##  euser         EUSER
##  f             F
##  fgid          FGID
##  fgroup        FGROUP
##  flag          F
##  flags         F
##  fname         COMMAND
##  fsgid         FSGID
##  fsgroup       FSGROUP
##  fsuid         FSUID
##  fsuser        FSUSER
##  fuid          FUID
##  fuser         FUSER
##  gid           GID
##  group         GROUP
##  ignored       IGNORED
##  intpri        PRI
##  lim           LIM
##  longtname     TTY
##  lstart        STARTED
##  m_drs         DRS
##  m_trs         TRS
##  maj_flt       MAJFL
##  majflt        MAJFLT
##  min_flt       MINFL
##  minflt        MINFLT
##  ni            NI
##  nice          NI
##  nwchan        WCHAN
##  opri          PRI
##  pagein        PAGEIN
##  pcpu          %CPU
##  pending       PENDING
##  pgid          PGID
##  pgrp          PGRP
##  pid           PID
##  pmem          %MEM
##  ppid          PPID
##  pri           PRI
##  rgid          RGID
##  rgroup        RGROUP
##  rss           RSS
##  rssize        RSS
##  rsz           RSZ
##  ruid          RUID
##  ruser         RUSER
##  s             S
##  sess          SESS
##  session       SESS
##  sgi_p         P
##  sgi_rss       RSS
##  sgid          SGID
##  sgroup        SGROUP
##  sid           SID
##  sig           PENDING
##  sig_block     BLOCKED
##  sig_catch     CATCHED
##  sig_ignore    IGNORED
##  sig_pend      SIGNAL
##  sigcatch      CAUGHT
##  sigignore     IGNORED
##  sigmask       BLOCKED
##  stackp        STACKP
##  start         STARTED
##  start_stack   STACKP
##  start_time    START
##  stat          STAT
##  state         S
##  stime         STIME
##  suid          SUID
##  suser         SUSER
##  svgid         SVGID
##  svgroup       SVGROUP
##  svuid         SVUID
##  svuser        SVUSER
##  sz            SZ
##  time          TIME
##  timeout       TMOUT
##  tmout         TMOUT
##  tname         TTY
##  tpgid         TPGID
##  trs           TRS
##  trss          TRSS
##  tsiz          TSIZ
##  tt            TT
##  tty           TT
##  tty4          TTY
##  tty8          TTY
##  ucomm         COMMAND
##  uid           UID
##  uid_hack      UID
##  uname         USER
##  user          USER
##  vsize         VSZ
##  vsz           VSZ
##  wchan         WCHAN

###------------------------------------
##
