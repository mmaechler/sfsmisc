## From: Christoph Buser <buser@stat.math.ethz.ch>
## To: maechler@stat.math.ethz.ch
## Subject: Duplicated
## Date: Tue, 25 Sep 2007 14:29:46 +0200


Duplicated <- function(v)
{
  ## Purpose: A counting-generalization of duplicated()
  ## ----------------------------------------------------------------------
  ## Arguments:
  ## ----------------------------------------------------------------------
  ## Author: Martin Maechler & Christoph Buser, Date: Sep 2007

    uv <- unique(nv <- na.omit(v))
    fv <- factor(nv, levels = uv)
    match(v, nv[duplicated(as.integer(fv))])
}
