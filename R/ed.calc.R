#' Calculate evolutionary distinctiveness

#' Calculates evolutionary distinctiveness for trees in a `consInt` object
#' @param x a `consInt` object
ed.calc <- function(x) {
  if(class(x) != 'consInt') warning('Expecting a consInt object')
  out <- data.frame(
    all.ed.equalSplits = phyloregion::evol_distinct(x$phy.full, 'equal.splits'),
    all.ed.fairProportion = phyloregion::evol_distinct(x$phy.full, 'fair.proportion'),
    all.w = w.phy(x$phy.full),
    sub.ed.equalSplits = phyloregion::evol_distinct(x$phy.sub, 'equal.splits'),
    sub.ed.fairProportion = phyloregion::evol_distinct(x$phy.sub, 'fair.proportion'),
    sub.w = w.phy(x$phy.sub)
  )
}
