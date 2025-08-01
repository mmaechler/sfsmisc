% Check from R:
%  news(db = tools:::.build_news_db_from_package_NEWS_Rd("~/R/Pkgs/sfsmisc/inst/NEWS.Rd"))
\name{NEWS}
\title{News for \R Package \pkg{sfsmisc}}
\encoding{UTF-8}

%%% TODO:
      %% \item New \code{rDatetime()} for random date/time sequences
      %% within a specified time range.
\section{CHANGES in sfsmisc VERSION 1.1-22 [2025-07-30]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{pretty10exp()} and hence \code{eaxis()} get new behaviour
      for non-default \code{sub10 = nn}; notably, \code{nn} may now be a
      negative integer; \code{sub10 = "10"} is now equivalent to
      \code{sub10 = c(-1,1)}, and newly, \code{sub10 = "100"} and
      \code{"1000"} as "mnemonic" for \code{sub10 = c(-1, 2)} or
      \code{c(-2, 3)}, respectively.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item .
    }
  }
}


\section{CHANGES in sfsmisc VERSION 1.1-21 [2025-07-25]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New \code{GCD(m,n)} and \code{LCM(...)}
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-20 [2024-10-23]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{sessionInfoX()} now reports \code{R_compiled_by()} when
      that exists.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-19 [2024-08-16]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item pretty10exp() gets new offset arg \code{off} and now works
      correctly even for subnormal numbers.
    }
  }
  \subsection{Misc}{
    \itemize{
      \item more packages mentioned in Rd xref's
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-18 [2024-04-25]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item sessionInfoX() now also stores (and prints) \code{.Machine}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item  \code{polyn.eval(7, x)} no longer always returns \code{7}, but
      rather a vector of the same length as \code{x}.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-17 [2024-02-01]}{
  \subsection{Misc}{
    \itemize{
      \item  \code{seq_along(.)} instead of (partial arg)
      \code{seq(along=.)}; fix other partial arg.

      \item Get rid of \code{is.R()}, deprecated from \R 4.4.0.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-16 [2023-08-08]}{
  \subsection{Misc}{
    \itemize{
      \item \file{NEWS.Rd} work around "not yet done"-entries.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-15 [2023-04-18]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New \code{list_()} (from my pkg tools) to create auto-named
      \code{list()}s easily.

      \item improved \code{Sys.ps.cmd()} for macOS.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-14 [2022-11-09]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{read.org.table()} now determines a reasonable
      \code{nrows} automatically.
    }
  }
  \subsection{Tweaks}{
    \itemize{
      \item \code{eaxis()} has a larger default, \code{12} instead of 10,
      for \code{nintLog}.

      \item \file{man/*.Rd}: remove some extraneous \code{\\}.

      \item \file{man/helppdf.Rd}: skip example on Windows (too slow).

      \item \code{TA.plot()} now works also with NA residuals.

      \item use new internal \code{struct()} as fast substitute of
      \code{structure()}.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-13 [2022-04-02]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{relErrV(target, current)} now keeps \code{dim()} and
      \code{dimnames()} e.g., when \code{current} is an mpfr-array and
      \code{names()} when present.
    }
  }
  \subsection{Tweaks}{
    \itemize{
      \item No longer import from \pkg{methods}: no need for a while already.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-12 [2021-09-10]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new \code{helppdf(topic)} mostly for interactive use.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{shortRversion()} \emph{still} failed with \code{"RC"}.

      \item \code{relErrV(x1, x2)} now works when both objects are of
      length zero, but no longer when only one of them is.
    }
  }
  %% \subsection{DEPRECATED & DEFUNCT}{
  %%   \itemize{
  %%     \item
  %%   }
  %% }
}

\section{CHANGES in sfsmisc VERSION 1.1-11 [2021-04-03]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{sessionInfoX(TRUE)} now choose chooses \emph{all} loaded
      packages (which were accidentally chosen since 1.1-9, see the bug fixes).
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{sessionInfoX()} no longer (since 1.1-9, accidentally) chooses \emph{all} packages.
      \item The \code{nearcor()} tests are less platform dependent.
    }
  }
  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item \code{u.assign0()} and \code{u.get0()} are defunct now.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-10 [2021-03-29]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{shortRversion()} no longer fails for "beta" or "RC" status.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-9 [2021-03-22]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{sessionInfoX()} now also reports \code{grSoftVersion()}
      and, when \pkg{tcltk} is loaded, \code{tclVersion()}.

      %% partly TODO: not yet giving deprecation warning
      \item \code{repChar()} is deprecated in favour of \R (>= 3.3.0)'s
      \code{\link{strrep}()}.

      \item new \code{shortRversion()} (from an old \R script of mine).

      \item new \code{relErrV()} and \code{relErr()}, improved from the
      \CRANpkg{Matrix} package \file{test-tools.R}
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-8 [2021-01-06]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{isRshared()} has been \dQuote{factorized out} of
      \code{sessionInfoX()}, as it maybe useful elsewhere, e.g., in
      \code{\link{Startup}} code setting \code{\link{.libPaths}()}.

      \item New utilities \code{pkgDesc()} and \code{pkgBuilt()}, giving
      named \code{character} result of \code{packageDescription()}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{sessionInfoX()}'s \code{isRshared()} is now based on
      \code{tools::Rcmd()} instead of \code{system()}, so possibly slightly
      more portable.

      \item Fixed not-yet--as-cran \file{NOTE} about undeclared packages in Rd xrefs.
    }
  }
}
\section{CHANGES in sfsmisc VERSION 1.1-7 [2020-05-05]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{eaxis()} gets new option \code{equidist.at.tol = 0.002},
      previously hardwired to 0.001.

      \item \code{seqXtend(<Date>, *, "interpolate")} now also works with
      changed \code{base::c.Date()} from R(4.1.0)-devel.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-6 [2020-04-04]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{pkgLibs()} now works on macOS, thanks to a hint by
      Brian Ripley.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-5 [2020-02-06]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item on Unix-alikes export new \code{pkgLibs()} utility even though
      its API may change.
    }
  }
  \subsection{Tweaks}{
    \itemize{
      \item replace \code{class(.) != ".."} in a \emph{comment}, just for
      source code purity ;-)
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Regression tests should not show differences, even when a
      version of R crippled by \code{--no-long-double} is used.

      \item examples now use \code{tempfile()}.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-4 [2019-04-25]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{eaxis()} gets new optional argument \code{use.expr} in
      order to force use of \code{pretty10exp()} in a non-log case where
      non-exponential format would be used.

      \item \code{sessionInfoX()} now also returns an element
      \code{isRshared}, and its \code{print()} method prints that and the
      \code{pcre_config()} information on the PCRE.

      \item \code{TA.plot()} now also works for \code{lmer()} results
      (\code{S4} objects, not \code{list}s).
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-3 [2018-11-26]}{
  \subsection{Tweaks}{
    \itemize{
      \item \file{DESCRIPTION} update, contains ORCID.
      \item \file{README.md} links to blog post on (17 yr old -> GH)
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{read.org.table()} thinko (\code{grep()}).
      \item \code{read.org.table(*, encoding=)} notably to work for UTF-8 files.

      \item \code{pretty10exp(*, lab.type="latex")} works better,
      notably for non-trivial \code{sub10}.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-2 [2018-03-05]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new \code{funEnv()} utility: \code{list()}-like construction
      of a set of functions (and more) which all share the same
      environment.

      \item Using \code{Authors@R} in \file{DESCRIPTION} and so finally
      honor the many contributors by listing them.

      \item Requiring R >= 3.2.0, so we can use \code{\\CRANpkg{.}} in
      help files.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{histBxp()} no longer evokes a warning from \code{hist.default()}.

      \item \code{digitsBase(0)} now works.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-1 [2017-06-08]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{sessionInfoX()} now also reports \code{capabilities()}
      (and more, depending on \R's version).

      \item \code{mult.fig()}'s default for \code{mgp} now adapts to a
      non-default value of \code{par("las")}.

      \item \code{eaxis()} gets new options \code{axp} and \code{n.axp}
      for more flexible automatic tick marks.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{integrate.xy()} is more careful in matching "data"
      with grid points, thanks to a bug report by Loraine Liping Seng.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.1-0 [2016-02-22]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{primes()} has optional \code{pSeq} argument,
      surprisingly with no known benefit.
      \item tweaks for \code{xy.unique.x()} speedup.
      \item \code{QUnif(..., p, ...)} now works for large \eqn{p} (and
      gets a \code{silent} option to suppress the message).
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Fix the \code{tkdensity()} bug introduced on 2015-07-22,
      hence, for \pkg{sfsmisc} versions \code{1.0-28} and \code{1.0-29},
      with the \dQuote{codetools cleanup}.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-29 [2016-01-22]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new function \code{read.org.table()} to read emacs
      \file{org} files via \code{\link{read.table}()}.

      \item new \code{loessDemo()}, providing a version of an old
      \code{loess.demo()}.

      \item new \code{sessionInfoX()} utility (with \code{print()} method).
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item (workaround R (<= 3.2.2) bug:) the (invisible) return value
      \code{str_data()} no longer contains extraneous \code{NULL}
      entries in the filtering case.
    }
  }
}


\section{CHANGES in sfsmisc VERSION 1.0-28 [2015-08-06]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{tkdensity()} is tweaked such as to look more standard
      to \pkg{codetools};
      \item similarly (much less) for \code{factorize()} and \code{roundfixS()}
      \item new \code{demo("pretty-lab")}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item also import from "base" packages
      \item \code{pretty10exp(*, lab.type = "latex")} typo fixed;
      \code{eaxis()} works better with \code{lab.type = "latex"}, both
      thanks to David Seifert.
    }
  }
}


\section{CHANGES in sfsmisc VERSION 1.0-27 [2015-01-05]}{
  \subsection{Repository}{
    \itemize{
      \item Moved sources to Github (\url{https://github.com/mmaechler/sfsmisc}),
      on Aug. 9, 2014, from years of emacs backups and a few RCS \dQuote{archives}.
      Blogged about it on \url{http://mmaechler.blogspot.ch/}.
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item New \code{missingCh()} utility, also for didactical purposes;
      \item new \code{rotn()} "utility".
      \item \code{eaxis()} gets an optional \code{sub10} argument.
      \item \code{mat2tex()} gets new option \code{envir = "tabular"}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{cairoSwd()} adapted to new Sweave conventions.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-26 [2014-06-16]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new \code{is.whole()} to test if numbers are integer valued.
      \item new \code{cairoSwd()} from Alain Hauser (not yet exported).
      \item new \code{bi2int()} utility (called from \code{as.intBase()})
      with a nice IP numbers transformation example.
      \item \code{toLatex.numeric()} gets \code{times} arg (from Alain).
      \item \code{pretty10exp()} gets new args \code{lab.type} and
      \code{lab.sep} from Ben Bolker.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item not using \code{prt.DEBUG()} anymore ourselves, as it has been
      deprecated.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-25 [2014-01-24]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item better docu on \code{Sys.cpuinfo()}
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item finally deprecate \code{prt.DEBUG()}
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-24 [2013-08-03]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Deprecate  u.assign0() as globalenv assignment is mostly
      deprecated, and the S <-> R compatibility is unneeded now.
      \item ps.do() and pdf.do() are now closures with their own file name.
      ==> no globalenv assignment needed anymore.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-13 [2010-10-20]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new \code{demo(hatMat)}  being more explicit than \code{?hatMat}
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-12 [2010-09-04]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{plotDS()} bug fix
    }
  }
}


\section{CHANGES in sfsmisc VERSION 1.0-11 [2010-02-22]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item fix nearCor() example
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-10 [2009-12-16]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item pdf.do() with better defaults [ ==> compatible to ps.do() ]
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-9  [2009-11-18]}{
  \subsection{Misc}{ \itemize{ \item maintenance } }
}

\section{CHANGES in sfsmisc VERSION 1.0-8  [2009-08-10]}{
  \subsection{Misc}{ \itemize{ \item maintenance } }
}

\section{CHANGES in sfsmisc VERSION 1.0-7  [2009-01-10]}{
  \subsection{Misc}{ \itemize{ \item maintenance } }
}

\section{CHANGES in sfsmisc VERSION 1.0-6  [2008-11-25]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new Sys.meminfo()
      \item p.profileTraces() improved; eaxis()
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-5  [2008-11-01]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new Duplicated()
      \item ellipsePoints() improvements
      \item eaxis() improvements
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item str_data() buglet
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-4  [2008-07-31]}{
  \subsection{Misc}{ \itemize{ \item maintenance } }
}

\section{CHANGES in sfsmisc VERSION 1.0-3  [2008-06-26]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item pdf.do() has better default behavior.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-2  [2008-05-03]}{
  \subsection{Misc}{ \itemize{ \item maintenance } }
}

\section{CHANGES in sfsmisc VERSION 1.0-1  [2008-01-30]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Encoding (latin1), and other platform issues
      \item inv.seq() and eaxis() improvements
    }
  }
}

\section{CHANGES in sfsmisc VERSION 1.0-0  [2007-12-10]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item plotDS() is new name for pl.ds() now allows 'ys' to be a smooth
      "fit structure".  That is now easily constructed via
      \item new seqXtend() function for constructing a sequence which includes
      a give set of numbers x.
      \item y <- roundfixS(x) yields integers y[i] with the same sum as x[i].
    }
  }
}


\section{CHANGES in sfsmisc VERSION 0.96-01  [2007-11-21]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new function eaxis() for "engineering" / "extended" axis
      drawing, notably for log-axis labeling
    }
  }
}

\section{CHANGES in sfsmisc VERSION 0.95-13  [2007-09-13]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new function nearcor() to find nearest correlation matrix;
      almost as donated by Jens Oehlschlaegel.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item pdf.latex(), pdf.do() : defaults; viewer specifications...
    }
  }
}


\section{CHANGES in sfsmisc VERSION 0.95-12  [2007-06-30]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New internal Sys.procinfo() function for cleaner implementation of
      Sys.cpuinfo() and Sys.meminfo().
    }
  }
}

\section{CHANGES in sfsmisc VERSION 0.95-10  [2007-06-25]}{
  \subsection{NEW FEATURES}{
    \itemize{
    \item new pdf.do(), pdf.end(), pdf.latex()), as analogues to ps.do() etc,
    \item implemented as wrapper to new  dev.latex() utility function
	(dev.latex): instead of ps.latex();
	(pdf.latex, ps.latex): now wrappers to dev.latex
    }
  }
}

\section{CHANGES in sfsmisc VERSION 0.95-2 to *-9}{
  \subsection{Version and Date (Latest First)}{
    \itemize{
      \item 0.95-9 : 2007-03-15
      \item 0.95-8 : 2007-01-18
      \item 0.95-7 : 2006-10-19
      \item 0.95-6 : 2006-06-26
      \item 0.95-5 : 2006-06-22
      \item 0.95-4 : 2006-02-26
      \item 0.95-3 : 2006-01-25
      \item 0.95-2 : 2005-11-03
    }
  }
}

\section{CHANGES in sfsmisc VERSION 0.95-1  [2005-04-25]}{
  \subsection{NEW FEATURES}{
    \itemize{
    \item 'rnls()' for robust nonlinear regression;
    'lseq()' for [l]og scale 'seq()'
    \item now has a NAMESPACE
    }
  }
}

\section{CHANGES in sfsmisc VERSION 0.9-8  [2004-12-14]}{
  \subsection{BUG FIXES}{
    \itemize{
      \item TA.plot() now also works for lme() results.
    }
  }
}

\section{CHANGES in sfsmisc VERSION 0.9-7  [2004-11-04]}{
  \itemize{
    \item Depends on R 1.9.0 (was effectively the case earlier):
    Removing old package names "modreg", "stepfun", etc
  }
}



\section{CHANGES in sfsmisc VERSION 0.9-6  [2004-09-27]}{
  \itemize{
    \item bug fix in p.ts(): case of start(.) of length 2
  }
}


\section{CHANGES in sfsmisc VERSION 0.9-5  [2004-08-12]}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item new posdefify(m) returns a positive definite matrix close to 'm'
      \item New  QUnif() and sHalton() for quasi-random number generation
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item fixed labeling bug in compresid2way().
    }
  }
  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item mult.fig()'s global assignment to 'old.par' is now deprecated.
      do work with op <- mult.fig(...)$old.par instead !!
      \item plotCI() is now deprecated -- use the one from package "gregmisc"!
    }
  }
}


\section{CHANGES in sfsmisc VERSION 0.9-4  [2004-05-26]}{
  \itemize{
    \item p.ts() allows date-time objects for x-axis labeling, with new
    arguments 'date.x', 'do.x.axis', 'do.x.rug', 'ax.format' and 'xlab'
    \item strcodes() had a bug (offset by 1) which also lead to one in AsciiToInt()
    \item p.res.2x() has new arguments 'xlim', 'ylim', 'main' and a new default
    for 'scol' (= 2:3).
  }
}


\section{CHANGES in sfsmisc VERSION 0.9-3  [2004-02-23]}{
  \itemize{
    \item p.res.2x() allows 'scol' of length 2.
    \item n.plot()  fixed the "log = .." (non-default) option.
  }
}


\section{CHANGES in sfsmisc VERSION 0.9-2  [2004-02-07]}{
  \itemize{
    \item put Sys.*() functions into unix-only directory
    \item ps.end() tries behaves better for non-unix finding the file
    \item u.date() becomes OS-independent.
  }
}


\section{CHANGES in sfsmisc VERSION 0.9-1  [2004-02-04 -- 1st CRAN ver.!]}{
  \itemize{
    \item new function compresid2way() from Werner Stahel.
    \item f.robftest() now returns an object of class "htest"

    \cr \cr
%% FIXME: It seems wrong that this cannot be put into a final \note{.}
%%   tools:::.build_news_db_from_package_NEWS_Rd("~/R/Pkgs/sfsmisc/inst/NEWS.Rd")

    \item For more details, see the \file{ChangeLog} file!
  }
}

