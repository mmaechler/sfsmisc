#### Martin Maechler, Aug.2000, originally in /u/maechler/R/MISC/ps.R

## I would really like builtin  Sys.PID() Sys.ps() for these

## For R versions < 1.2:
if(!exists("Sys.info", mode="function"))
  Sys.info <- function() {
    snrm <- unlist(strsplit(system("uname -srnm",TRUE)," "))
    names(snrm) <- c("sysname", "nodename", "release", "machine")
    ul <- structure(getenv(c("LOGNAME","USER")), names = c("login","user"))
    c(snrm, version = system("uname -v",TRUE), ul)
  }

Sys.ps.cmd <- function() {
  sys <- Sys.info()["sysname"]
  if(sys == "Linux") "/bin/ps --width 1000"
  else if(sys == "SunOS") "/usr/bin/ps"
  else {
    warning("Unknown OS [Operating System]; `ps' may not be compatible")
    "ps"
  }
}
Sys.PID <- function(ps.cmd = Sys.ps.cmd()) {
    ## Return the PID of the R process :
    as.integer(system(paste(ps.cmd,"-o pid"), intern = TRUE)[2])
}

.Sys.ps.fields <-
  list(POSIX = c("args", "comm", "time", "etime", "nice", "pcpu",
         "pid", "pgid", "ppid", "group", "rgroup", "user", "ruser",
         "tty", "vsz"),

       ## Now the extras, not in above POSIX:
       SunOS = c( "addr", "pri", "c", "rgid", "class", "rss", "f",
         "ruid", "fname", "s", "gid", "sid", "opri", "stime", "osz",
         "uid", "pmem", "wchan"),   
       Linux =## These are Linux (RH 6.2):"Docu" at end ..
       c("%cpu", "%mem", "alarm", "blocked", "bsdstart", "bsdtime", 
         "c", "caught", "cmd", "command", "cputime", "drs", "dsiz", "egid", 
         "egroup", "eip", "esp", "euid", "euser", "f", "fgid", "fgroup", 
         "flag", "flags", "fname", "fsgid", "fsgroup", "fsuid", "fsuser", 
         "fuid", "fuser", "gid", "ignored", "intpri", "lim", "longtname", 
         "lstart", "m_drs", "m_trs", "maj_flt", "majflt", "min_flt", "minflt", 
         "ni", "nwchan", "opri", "pagein", "pending", "pgrp", "pmem", 
         "pri", "rgid", "rss", "rssize", "rsz", "ruid", "s", "sess", "session", 
         "sgi_p", "sgi_rss", "sgid", "sgroup", "sid", "sig", "sig_block", 
         "sig_catch", "sig_ignore", "sig_pend", "sigcatch", "sigignore", 
         "sigmask", "stackp", "start", "start_stack", "start_time", "stat", 
         "state", "stime", "suid", "suser", "svgid", "svgroup", "svuid", 
         "svuser", "sz", "timeout", "tmout", "tname", "tpgid", "trs", 
         "trss", "tsiz", "tt", "tty4", "tty8", "ucomm", "uid", "uid_hack", 
         "uname", "vsize", "wchan")
       )

## Note that  proc.time() gives part of that info better

## command == cmd == args   gives "command + arguments : too long
.Sys.ps.multifields <- c("command", "cmd","args", "lstart")

Sys.ps <-
    function(process = Sys.PID(),
             fields = c("pid", "pcpu", "time", "vsz", "comm"),
             usefile = length(fields) > 10,
             ps.cmd  = Sys.ps.cmd(),
             verbose = getOption("verbose"),
             warn.multi = verbose || any(fields != "ALL"))
{
    ps.opt <- {
        if(is.numeric(process) && process == round(process))
            paste("-p",process) # PID
        else if(process == "ALL") {
            warning("`process = \"ALL\"' not yet working properly")
            "-e" # all process
          }
        else if(is.character(process) && length(process) == 1)
            paste("-C",process) # Command name
        else stop(paste("invalid `process':",format(process)))
    }
    if(length(ps.opt) > 1)
      warning("Multiple processes : not yet working ...")
    
    Sys.ps.fields <- c(.Sys.ps.fields $ POSIX,
                       if(any(ii <- Sys.info()["sysname"] ==
                              names(.Sys.ps.fields)))
                       .Sys.ps.fields[ii][[1]])
      
    if(fields == "ALL")
      i.field <- TRUE
    else {
      i.field <- pmatch(fields, Sys.ps.fields) # allow abbreviated ones
      if(any(ina <- is.na(i.field))) {
        warning(paste("Dropping invalid field names",
                      fields[ina]))
        i.field <- i.field[!ina]
      }
    }
    fields <- Sys.ps.fields[i.field]
    imult <- !is.na(match(fields, .Sys.ps.multifields))
    if(any(imult) && length(fields) > 1) {
      if(warn.multi)
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
    cmd <- paste(ps.cmd, ps.opt,
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
  ## For both Solaris and GNU(Linux);  GNU/Linux additionally has dsize
  r <- Sys.ps(process, c("rss","vsz"))
  storage.mode(r) <- "integer"
  r
}


###---- Solaris 2.5.1  man ps   has :

## DISPLAY FORMATS
##      Under the -f option, ps tries to determine the command  name
##      and  arguments given when the process was created by examin-
##      ing the user block.   Failing  this,  the  command  name  is
##      printed, as it would have appeared without the -f option, in
##      square brackets.
## 
##      The column headings and the meaning of the columns in  a  ps
##      listing  are  given  below; the letters f and l indicate the
##      option  (full  or  long,  respectively)  that   causes   the
##      corresponding  heading to appear; all means that the heading
##      always appears.  Note:  These  two  options  determine  only
##      what  information  is  provided  for  a process; they do not
##      determine which processes will be listed.
## 
##      F         (l)       Flags (hexadecimal and additive) associ-
##                          ated  with the process.  These flags are
##                          available for  historical  purposes;  no
##                          meaning  should be currently ascribed to
##                          them.
## 
##      S         (l)       The state of the process:
## 
##                          O    Process is running on a processor.
##                          S    Sleeping: process is waiting for an
##                               event to complete.
##                          R    Runnable: process is on run queue.
##                          Z    Zombie  state:  process  terminated
##                               and parent not waiting.
##                          T    Process is stopped, either by a job
##                               control  signal  or  because  it is
##                               being traced.
## 
##      UID       (f,l)     The effective user ID number of the pro-
##                          cess  (the  login  name is printed under
##                          the -f option).
## 
##      PID       (all)     The process  ID  of  the  process  (this
##                          datum  is  necessary  in order to kill a
##                          process).
## 
##      PPID      (f,l)     The process ID of the parent process.
## 
##      C         (f,l)     Processor  utilization  for   scheduling
##                          (obsolete).   Not  printed  when  the -c
##                          option is used.
## 
##      CLS       (f,l)     Scheduling class.  Printed only when the
##                          -c option is used.
## 
##      PRI       (l)       The priority of  the  process.   Without
##                          the -c option, higher numbers mean lower
##                          priority.  With the  -c  option,  higher
##                          numbers mean higher priority.
## 
##      NI        (l)       Nice value, used  in  priority  computa-
##                          tion.  Not printed when the -c option is
##                          used.  Only  processes  in  the  certain
##                          scheduling classes have a nice value.
## 
##      ADDR      (l)       The memory address of the process.
## 
##      SZ        (l)       The size (in  pages)  of  the  swappable
##                          process's image in main memory.
## 
##      WCHAN     (l)       The address of an event  for  which  the
##                          process  is sleeping (if blank, the pro-
##                          cess is running).
## 
##      STIME     (f)       The starting time of the process,  given
##                          in hours, minutes, and seconds.  (A pro-
##                          cess begun more than  twenty-four  hours
##                          before  the  ps  inquiry  is executed is
##                          given in months and days.)
## 
##      TTY       (all)     The controlling terminal for the process
##                          (the  message,  ?, is printed when there
##                          is no controlling terminal).
## 
##      TIME      (all)     The cumulative execution  time  for  the
##                          process.
## 
##      CMD       (all)     The command name (the full command  name
##                          and  its  arguments, up to a limit of 80
##                          characters, are  printed  under  the  -f
##                          option).
## 
##      The following two additional columns are printed when the -j
##      option is specified:
## 
##      PGID                The process  ID  of  the  process  group
##                          leader.
## 
##      SID                 The process ID of the session leader.
## 
##      A process that has exited and has a parent, but has not  yet
##      been waited for by the parent, is marked <defunct>.
## 
##   -o format
##      The -o option allows the output format to be specified under
##      user control.
## 
##      The format specification must be a list of  names  presented
##      as a single argument, blank- or comma-separated.  Each vari-
##      able has a default header.  The default header can be  over-
##      ridden  by  appending an equals sign and the new text of the
##      header.  The rest of the characters in the argument will  be
##      used as the header text.  The fields specified will be writ-
##      ten in the order specified on the command line,  and  should
##      be arranged in columns in the output.  The field widths will
##      be selected by the system to be at  least  as  wide  as  the
##      header  text  (default  or overridden value).  If the header
##      text is null, such as -o user=, the field width will  be  at
##      least  as  wide  as  the default header text.  If all header
##      text fields are null, no header line will be written.
## 
##      The following names are recognized in the POSIX locale:
## 
##      user        The effective user ID of the process.  This will
##                  be  the  textual  user ID, if it can be obtained
##                  and  the  field  width  permits,  or  a  decimal
##                  representation otherwise.
## 
##      ruser       The real user ID of the process.  This  will  be
##                  the  textual  user ID, if it can be obtained and
##                  the field width permits, or a decimal  represen-
##                  tation otherwise.
## 
##      group       The effective group ID  of  the  process.   This
##                  will  be  the  textual  group  ID,  if it can be
##                  obtained and  the  field  width  permits,  or  a
##                  decimal representation otherwise.
## 
##      rgroup      The real group ID of the process.  This will  be
##                  the  textual group ID, if it can be obtained and
##                  the field width permits, or a decimal  represen-
##                  tation otherwise.
## 
##      pid         The decimal value of the process ID.
## 
##      ppid        The decimal value of the parent process ID.
## 
##      pgid        The decimal value of the process group ID.
## 
##      pcpu        The ratio of CPU time used recently to CPU  time
##                  available  in  the  same  period, expressed as a
##                  percentage.  The meaning of ``recently'' in this
##                  context  is unspecified.  The CPU time available
##                  is determined in an unspecified manner.
## 
##      vsz         The size of the process in (virtual)  memory  in
##                  kilobytes as a decimal integer.
## 
##      nice        The  decimal  value  of  the  system  scheduling
## 
##                  priority of the process.  See nice(1).
## 
##      etime       In the POSIX locale, the elapsed time since  the
##                  process was started, in the form:
##                  [[dd-]hh:]mm:ss
## 
##                  where
## 
##                  dd   will represent the number of days,
##                  hh   the number of hours,
##                  mm   the number of minutes, and
##                  ss   the number of seconds.
## 
##                  The dd field will be a decimal integer.  The hh,
##                  mm  and  ss  fields  will  be  two-digit decimal
##                  integers padded on the left with zeros.
## 
##      time        In the POSIX locale, the cumulative CPU time  of
##                  the process in the form:
##                  [dd-]hh:mm:ss
## 
##                  The dd,  hh,  mm,  and  ss  fields  will  be  as
##                  described in the etime specifier.
## 
##      tty         The name of the controlling terminal of the pro-
##                  cess  (if  any)  in  the same format used by the
##                  who(1) command.
## 
##      comm        The name of the command being executed  (argv[0]
##                  value) as a string.
## 
##      args        The command with all its arguments as a  string.
##                  The  implementation  may  truncate this value to
##                  the field width; it is  implementation-dependent
##                  whether  any  further  truncation occurs.  It is
##                  unspecified whether the string represented is  a
##                  version of the argument list as it was passed to
##                  the command when it started, or is a version  of
##                  the  arguments as they may have been modified by
##                  the application.  Applications cannot depend  on
##                  being  able  to  modify  their argument list and
##                  having that modification  be  reflected  in  the
##                  output of ps.  The Solaris implementation limits
##                  the string to 80 bytes; the string is  the  ver-
##                  sion  of  the  argument list as it was passed to
##                  the command when it started.
## 
##      The following names are recognized in the Solaris  implemen-
##      tation:
## 
##      f           Flags (hexadecimal and additive) associated with
##                  the process.
## 
##      s           The state of the process.
## 
##      c           Processor utilization for scheduling (obsolete).
## 
##      uid         The effective user ID number of the process as a
##                  decimal integer.
## 
##      ruid        The real user ID number  of  the  process  as  a
##                  decimal integer.
## 
##      gid         The effective group ID number of the process  as
##                  a decimal integer.
## 
##      rgid        The real group ID number of  the  process  as  a
##                  decimal integer.
## 
##      sid         The process ID of the session leader.
## 
##      class       The scheduling class of the process.
## 
##      pri         The priority of  the  process.   Higher  numbers
##                  mean higher priority.
## 
##      opri        The obsolete priority  of  the  process.   Lower
##                  numbers mean higher priority.
## 
##      addr        The memory address of the process.
## 
##      osz         The size (in pages) of the  swappable  process's
##                  image in main memory.
## 
##      wchan       The address of an event for which the process is
##                  sleeping (if -, the process is running).
## 
##      stime       The  starting  time  or  date  of  the  process,
##                  printed with no blanks.
## 
##      rss         The resident set size of the process,  in  kilo-
##                  bytes as a decimal integer.
## 
##      pmem        The ratio of the process's resident set size  to
##                  the physical memory on the machine, expressed as
##                  a percentage.
## 
##      fname       The first 8  bytes  of  the  base  name  of  the
##                  process's executable file.
## 
##      Only comm and args are allowed to contain blank  characters;
##      all  others, including the Solaris implementation variables,
##      are not.
## 
##      The following table specifies the default header to be  used
##      in the POSIX locale corresponding to each format specifier.
## 
##   _______________________________________________________________________
##  | Format Specifier   Default Header|  Format Specifier   Default Header|
##  |__________________________________|___________________________________|
##  | args               COMMAND       |  ppid               PPID          |
##  | comm               COMMAND       |  rgroup             RGROUP        |
##  | etime              ELAPSED       |  ruser              RUSER         |
##  | group              GROUP         |  time               TIME          |
##  | nice               NI            |  tty                TT            |
##  | pcpu               %CPU          |  user               USER          |
##  | pgid               PGID          |  vsz                VSZ           |
##  | pid                PID           |                                   |
##  |__________________________________|___________________________________|
## 
##      The following table lists the Solaris implementation  format
##      specifiers and the default header used with each.
## 
##   _______________________________________________________________________
##  | Format Specifier   Default Header|  Format Specifier   Default Header|
##  |__________________________________|___________________________________|
##  | addr               ADDR          |  pri                PRI           |
##  | c                  C             |  rgid               RGID          |
##  | class              CLS           |  rss                RSS           |
##  | f                  F             |  ruid               RUID          |
##  | fname              COMMAND       |  s                  S             |
##  | gid                GID           |  sid                SID           |
##  | opri               PRI           |  stime              STIME         |
##  | osz                SZ            |  uid                UID           |
##  | pmem               %MEM          |  wchan              WCHAN         |
##  |__________________________________|___________________________________|
## 


###--- Linux  man ps   has :

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
