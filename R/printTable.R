
printTable2 <- function(table2, digits = 3)
{
  ##-- 2-weg Kontingenztafel mit allem zusammen ... -- ruft  catCon(.)  auf
  ##-- Urspruneglich fuer NDK-Uebungen 1992
  ##-- Verbessert und Fehler korrigiert! : M.Maechler, Feb.1993
  d <- dim(table2)
  if(length(d) != 2)
    stop("Argument muss numerische Matrix sein: Die (2-Weg) Kontingenz Tafel")
  N <- sum(table2)
  cat("\nKontingenz-Tafel mit Randsummen:\n")
  print(margin2table(table2), digits=0)
  cat("\nGemeinsame Verteilung mit Randverteilungen:\n")
  I <- d[1];  J <- d[2];  df <- (I-1)*(J-1)
  r <- margin2table(table2/N)
  print(r, digits)
  joint <- r[1:I, 1:J]
  xrand <- r[I+1, 1:J]
  yrand <- r[1:I, J+1]
  condy <- joint/yrand
  condx <- t(t(joint)/xrand)
  cat("Bedingte Verteilung gegen y:\n"); print(round(condy,digits)); cat("\n")
  cat("Bedingte Verteilung gegen x:\n"); print(round(condx,digits)); cat("\n")
  exp.ind <- N * outer(yrand,xrand)#- Expected under INDEPendence: n * p_i * p_j
  cat("Freiheitsgrade: df =",df,"\n")
  cat("Chi^2 - Annahmebereich: [0,", round(qchisq(0.95,df),1),
      "] (alpha=0.05)\n\n\n", sep = "")
  test.chisq <- sum((as.matrix(table2)-exp.ind)^2/exp.ind)
  cat("Testwerte unter der Unabhaengigkeitshypothese:\n")
  cat("  Test mit Chi^2: ",format(round(test.chisq,2)),
      " (P-Wert: ",round(1-pchisq(test.chisq,df),4),")\n",sep = "")
  is.pos <- table2 != 0
  test.deviance <- 2*sum(table2[is.pos]*log(table2[is.pos]/exp.ind[is.pos]))
  cat("  Test mit Devianz:  ",format(round(test.deviance,2)),
      " (P-Wert: ",round(1-pchisq(test.deviance,df),4),")\n\n",sep = "")
  invisible(list(p.condx = condx, p.condy = condy, expected.indep = exp.ind,
		 df = df, chisq.test = test.chisq, deviance = test.deviance))
}

### The original catCon() function did compute and print;
### now separated :

margin2table <- function(x, totName = "sum") {
    x <- as.matrix(x)
    r <- rowSums(x)
    r <- rbind(cbind(x, r), c(colSums(x), sum(r)))
    dimnames(r) <-
        if(!is.null(dnx <- dimnames(x))) {
            list(if(!is.null(dnx[[1]])) c(dnx[[1]], totName),
                 if(!is.null(dnx[[2]])) c(dnx[[2]], totName))
        } ## else NULL
    class(r) <- "margin2table"
    r
}

print.margin2table <- function(x, digits = 3, ...)
{
  if(is.null(d <- dim(x)) || length(d <- d - 1) !=2)
      stop("'x' is not a matrix")
  N <- d[1];  M <- d[2]
  cx <- format(round(x, digits))
  cx <- cbind(rbind(cx, paste(rep("-", max(nchar(cx))), collapse = "")), "|")
  print(cx[c(1:N,N+2,N+1), c(1:M,M+2,M+1)], quote = FALSE)
  invisible(x)
}
