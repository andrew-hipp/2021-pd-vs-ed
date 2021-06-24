#' Calculate evolutionary distinctiveness

#' Calculates evolutionary distinctiveness for trees in a `consInt` object
#' @param x a `consInt` object
#' @import phyloregion
#' @export
ed.calc <- function(x) {
  if(class(x) != 'consInt') warning('Expecting a consInt object')
  out <- list(
    all = data.frame(
      ed.equalSplits = phyloregion::evol_distinct(x$phy.full, 'equal.splits'),
      ed.fairProportion = phyloregion::evol_distinct(x$phy.full, 'fair.proportion'),
      w = w.phy(x$phy.full)
    ),
    sub = data.frame(
      ed.equalSplits = phyloregion::evol_distinct(x$phy.sub, 'equal.splits'),
      ed.fairProportion = phyloregion::evol_distinct(x$phy.sub, 'fair.proportion'),
      w = w.phy(x$phy.sub)
    )
  )
}
