### Port to R and a few small improvements:
### Copyright © 2000 Martin Maechler, ETH Zurich

mat2tex <- function(x, file = "mat.tex", append = TRUE, digits = 3, title)
{
    if(length(d.x <- dim(x)) != 2)
        stop("`x' must be a matrix like object with dim(x) of lenght 2")
    if(any(d.x <= 0))
        stop("`dim(x)' must be positive")
    nr.x <- d.x[1]
    nc.x <- d.x[2]
    ## determine if there are labels to be processed
    dn.x <- dimnames(x)
    if(has.rowlabs <- !is.null(dn.x[[1]]))        rowlabs <- dn.x[[1]]
    if(has.collabs <- !is.null(dn.x[[2]]))        collabs <- dn.x[[2]]

    ## produce column specification
    colspec <- "{|"
    if(has.rowlabs)
        colspec <- paste(colspec, "l||")
    for(i in 1:nc.x)
        colspec <- paste(colspec, "c|")
    colspec <- paste(colspec, "}", sep = "")
    cat(paste("\\begin{tabular}", colspec, " \n"), file = file, append = append)

    span <- nc.x + if(has.rowlabs) 1 else 0
    if(!missing(title))
        cat(paste("\\multicolumn{", span, "}{c}{", title,
                  "} \\\\ \\hline", "\n"), file = file, append = TRUE)	
    else cat("\\hline \n", file=file,append=TRUE)
    ## output column labels if needed
    if(has.collabs) {
        collabline <- " "
        if(has.rowlabs)
            collabline <- paste(collabline, " \\  &")
        collabline <- paste(collabline, collabs[1])
        if(nc.x > 2) {
            for(i in 2:nc.x)
                collabline <- paste(collabline, "&", collabs[i])
        }
        collabline <- paste(collabline, "\\\\ \\hline \\hline")
        cat(paste(collabline, "\n"), file = file, append = TRUE)
    }
    ## output matrix entries
    options(digits = digits)
    for(i in 1:nr.x) {
        thisline <-
            if(has.rowlabs)
                paste(rowlabs[i], "&", format(x[i, 1])) else format(x[i, 1])
        if(nc.x >= 2) for(j in 2:nc.x)
            thisline <- paste(thisline, "&", format(x[i, j]))

        thisline <- paste(thisline, "\\\\ \\hline")
        cat(paste(thisline, "\n"), file = file, append = TRUE)
    }
    cat(paste("\\end{tabular}", " \n"), file = file, append = TRUE)
}

