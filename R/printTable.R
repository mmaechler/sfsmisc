
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
  catCon (table2, 0)
  cat("\nGemeinsame Verteilung mit Randverteilungen:\n")
  I <- d[1];  J <- d[2];  df <- (I-1)*(J-1)
  r <- catCon (table2/N, digits)
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

catCon <- function(mat, digits = 3)
{
  ##-- "CAT CONtingency table"  mit RAND-SUMMEN + "Verzierung"
  ##-- Korrigiert fuer UNsymmetr. Kont.tafeln und stark vereinfacht: M.Maechler
  ## Gibt Resultat zurueck !
  ##>>> Hilfsfunktion fuer 'printTable2' <<<
  mat <- as.matrix(mat)
  d <- dim(mat);  N <- d[1];  M <- d[2]
  mat <- rbind(cbind(mat, mat %*% rep(1, M)),
	       c(rep(1,N) %*% mat,  sum(mat)))
  out <- format(round(mat, digits))
  "--" <- paste(rep("-", max(nchar(out))), collapse = "")
  out <- cbind(rbind(out, get("--")), "|")
  print(out[c(1:N,N+2,N+1), c(1:M,M+2,M+1)], quote = FALSE)
  invisible(mat)			#--- die erweiterte Matrix --
}
